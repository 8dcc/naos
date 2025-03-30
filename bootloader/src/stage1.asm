; Copyright 2025 8dcc. All Rights Reserved.
;
; This program is part of naos.
;
; This program is free software: you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free Software
; Foundation, either version 3 of the License, or (at your option) any later
; version.
;
; This program is distributed in the hope that it will be useful, but WITHOUT
; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
; details.
;
; You should have received a copy of the GNU General Public License along with
; this program.  If not, see <https://www.gnu.org/licenses/>.
;
;------------------------------------------------------------------------------
;
; This file describes the entry point of the bootloader (called Stage 1).
;
; The BIOS will look for the '55 AA' signature in bytes 510..511, and loads us
; into 0x7C00.
;
; For more information on how the FAT12 file system works, see my blog article:
; https://8dcc.github.io/programming/understanding-fat.html
;
; To disassemble the generated binary (to check for offsets, for example), you
; can use:
;
;   ndisasm -b 16 -e 0x3E stage1.bin
;
; Where 0x3E is the offset of the first code byte after the EBPB; note that all
; displayed offsets are relative to this byte. The 'ndisasm' command ships with
; NASM.
;
;------------------------------------------------------------------------------
;
; TODO:
;
;   - Improve consistency of functions, specially regarding register
;     preservation. Too many pushes/pops is not a problem when it comes to
;     performance, but it can be when it comes to stack space (see comment
;     below, when setting up SS) and specially when it comes to binary size,
;     since we only have 448 bytes for code.
;
;------------------------------------------------------------------------------
; Includes

%include "include/error_codes.asm"
%include "include/bios_codes.asm"
%include "include/fat12_structures.asm"

;------------------------------------------------------------------------------
; Macros

; Number of retries for the BIOS disk operations, since floppy disks can be
; unreliable.
%define DISK_RETRY_NUM 3

; Memory address where the BIOS is supposed to load us.
%define BOOT_LOAD_ADDR 0x7C00

; Memory address for loading the FAT12 root directory (for searching files) and
; the FAT itself (for getting the cluster numbers for the file).
%assign SCRATCH_BUFFER_ADDR (BOOT_LOAD_ADDR + 512)

; Short name of the Stage 2 binary that should be found in the root directory of
; the FAT12 volume.
%define STAGE2_FILENAME "STAGE2  BIN"
%if %strlen(STAGE2_FILENAME) != 11
%error "Expected an 11-byte filename that followed the 8.3 scheme."
%endif

; Address where the Stage 2 should be loaded.
%define STAGE2_ADDR 0xA000

;-------------------------------------------------------------------------------
; Start of boot sector

bits 16

; Specify base address where the BIOS will load us. We need to use NASM's
; built-in "linker" because we are generating a raw binary, and we can't link
; with 'ld'.
org BOOT_LOAD_ADDR

section .text

; Entry point of the bootloader. The Boot Record must start with a short jump to
; the real entry point followed by a NOP.
global _start
_start:
    jmp     short bootloader_entry
    nop

; At offset 3, we need to add 8 arbitrary bytes so the BIOS Parameter
; Block (BPB) starts at offset 0xB.
db "fsosboot"

%if ($-$$) != 0xB
%error "Expected the BIOS Parameter Block at offset 0xB."
%endif

; Extended BIOS Parameter Block (EBPB) fields (section offset 0x0B..0x3D,
; inclusive).
bpb:
istruc ebpb_t
    at bpb_t.bytes_per_sector,       dw 512
    at bpb_t.sectors_per_cluster,    db 1
    at bpb_t.reserved_sectors,       dw 1       ; Includes boot sector
    at bpb_t.fat_count,              db 2
    at bpb_t.dir_entries_count,      dw 0xE0
    at bpb_t.total_sectors,          dw 2880    ; 1.44MiB / 512bps = 2880
    at bpb_t.media_descriptor_type,  db 0xF0    ; 3.5" floppy disk
    at bpb_t.sectors_per_fat,        dw 1
    at bpb_t.sectors_per_track,      dw 16
    at bpb_t.head_count,             dw 2
    at bpb_t.hidden_sectors,         dd 0

    ; Should only be set if entry at BPB offset 0x13 (bpb_t.total_sectors) is
    ; zero.
    at bpb_t.large_sector_count,     dd 0

    at ebpb_t.drive_number,          db 0       ; 0 for floppy, 0x80 for HDDs
    at ebpb_t.reserved,              db 0

    ; Should be 0x28 or 0x29 depending on the number of subsequent fields, see
    ; structure definition.
    at ebpb_t.signature,             db 0x29

    ; The contents of the following two shouldn't matter, but they have to be 4
    ; and 11 bytes respectively.
    at ebpb_t.volume_id,             db "FSOS"
    at ebpb_t.volume_label,          db "Bootloader "

    ; 8-byte file system type, padded with blanks. For example "FAT12   ",
    ; "FAT16   " or "FAT     ". In theory only used for display purposes, but
    ; it's still used sometimes for identification purposes.
    at ebpb_t.system_id,             db "FAT12   "
iend

;-------------------------------------------------------------------------------
; Entry point

bootloader_entry:
    ; Start by setting up the Data Segment (DS) and Extra Segment (ES). We need
    ; to use an intermediate register to write to them.
    xor     ax, ax
    mov     ds, ax
    mov     es, ax

    ; We will also set up the Stack Segment (SS). Since the BIOS loaded us at
    ; address 0x7C00, and the stack grows downwards, we can use the current
    ; address as the bottom of the stack (the highest address).
    ;
    ; Note that, when a value is pushed, the stack pointer is decreased before
    ; the value is written, so our first 16 bytes won't be overwritten.
    ;
    ; We should be able to use the region between 0x7C00 and 0x500, but we
    ; shouldn't rely to heavily on this.
    mov     ss, ax
    mov     sp, BOOT_LOAD_ADDR

    ; Clear the Code segment (CS) by performing a far jump, ensuring that we are
    ; in '0000:7c00' and not '07c0:0000'. The JMP is necessary because we can't
    ; write to CS directly (we could also use two PUSHes and a RETF).
    jmp     0000:.cleared_cs
.cleared_cs:

    ; Get the drive number, sectors per track and head count from the BIOS, and
    ; store it in the BPB.
    ;
    ; FIXME: This currently fails (error code 1) when booting from CD/DVD.
    call    bios_read_disk_info

    mov     si, str_searching
    call    bios_puts

    ; Calculate the location of the root directory:
    ;
    ;     root_dir_lba = (fat_count * sectors_per_fat) + first_fat_lba
    ;
    ; Note that the first FAT is always located after the reserved sectors.
    xor     ah, ah
    mov     al, [bpb + bpb_t.fat_count]
    mul     word [bpb + bpb_t.sectors_per_fat]
    add     ax, [bpb + bpb_t.reserved_sectors]
    push    ax

    ; Calculate the size of the root directory in bytes. We shift the number of
    ; directory entries 5 bits to the left, effectively multiplying it by 32
    ; (the size of a 'DirectoryEntry' structure).
    mov     ax, [bpb + bpb_t.dir_entries_count]
    shl     ax, 5

    ; Calculate the size of the root directory in sectors, rounding up.
    ;
    ;     root_dir_sectors =
    ;         (root_dir_bytes + bytes_per_sector - 1) / bytes_per_sector;
    ;
    ; This operation is suggested by the FAT specification, see my blog article
    ; mentioned above.
    add     ax, word [bpb + bpb_t.bytes_per_sector]
    dec     ax
    xor     dx, dx              ; For the division
    div     word [bpb + bpb_t.bytes_per_sector]

    ; Search for the Stage 2 binary in the root directory, and return its first
    ; cluster index.
    mov     cx, ax              ; CL = root_dir_sectors
    pop     ax                  ; AX = root_dir_lba
    push    ax                  ; Push again for later. Can't do: MOV AX, [SP]
    call    get_stage2_cluster  ; AX = first_cluster_idx

    ; Calculate the start of the data region. Note that CL contains the size of
    ; the root directory in sectors.
    pop     dx                  ; DX = root_dir_lba
    add     dx, cx              ; DX += root_dir_sectors  // data_region_lba

    ; Read the Stage 2 binary into the 'STAGE2_ADDR'.
    ;
    ; Note that DX contains the first sector of the data region. Also note that,
    ; although AX contains the first cluster index of the file we just found,
    ; the function expects it in CX.
    mov     cx, ax              ; CX = first_cluster_idx
    mov     bx, STAGE2_ADDR
    call    read_file_contents

    ; TODO: Add more status messages if possible.

    ; Jump to Stage 2, whose address is stored in ES:BX.
    ;
    ; We use PUSH + RETF to perform a far return into ES:BX, since we can't do a
    ; far JMP unless we use an intermediate 32-bit memory location to store the
    ; contents of BX and ES, in that order.
    push    es
    push    bx
    retf                        ; JMP ES:BX

    jmp     halt                ; Unreachable

;-------------------------------------------------------------------------------

; void die_err(char error_code /* AL */);
;
; Print the specified error code and die.
die_err:
    mov     [err_placeholder], al
    mov     si, str_err
    ; Fallthrough to 'die'

; void die(const char* str /* SI */);
;
; Print the specified error message and halt.
die:
    call    bios_puts
    ; Fallthrough to 'halt'

; void halt(void);
;
; Disable interrupts and stop execution.
halt:
    cli
    hlt
    jmp     halt

;-------------------------------------------------------------------------------
; BIOS functions

; void bios_read_disk_info(uint16_t drive_index /* DL */);
;
; Read the relevant disk information into the BPB, such as the sectors per track
; or head count. This procedure doesn't preserve any caller registers.
bios_read_disk_info:
    ; First, write the drive number to the Extended BPB. We expect the drive
    ; number in DL because it's the register where the BIOS supposedly stored
    ; our drive number.
    mov     [bpb + ebpb_t.drive_number], dl

    ; Read the drive information with a BIOS interrupt.
    push    es
    mov     ah, BIOS_DRIVE_READ_PARAMS
    xor     di, di              ; For some buggy BIOSes
    stc
    int     BIOS_INT_DISK
    pop     es

    ; If the carry flag became unset after the BIOS call, the read succeded.
    jnc     .success

    ; Otherwise, it failed to read parameters from the disk.
    mov     al, ERR_BIOS_READ_INFO
    jmp     die_err

.success:
    ; The number of sectors per track is returned in the lower 6 bits of CX.
    and     cx, 0b0000000000111111
    mov     [bpb + bpb_t.sectors_per_track], cx

    ; Store the head count. Note that the returned value in DH is zero-indexed,
    ; but a one-indexed count is expected in the BPB.
    inc     dh
    shr     dx, 8
    mov     [bpb + bpb_t.head_count], dx

    ret

; void bios_put(const char* str /* SI */);
;
; Print the specified null-terminated string, to the BIOS console.
;
; TODO: The BIOS might overwrite more registers inside the interrupt.
bios_put:
    push    ax
    push    bx

.loop:
    lodsb                   ; Load byte from SI into AL, and increment SI
    test    al, al          ; Did we reach the end of the string?
    jz      .done

    mov     ah, BIOS_TTY_WRITE_CHAR
    mov     bh, 0x0                 ; Page number: 0
    int     BIOS_INT_VIDEO

    jmp     .loop

.done:
    pop     bx
    pop     ax
    ret

; void bios_puts(const char* str /* SI */);
;
; Print an identifier, the specified null-terminated string and along with a
; newline, to the BIOS console.
bios_puts:
    push    si

    mov     si, str_stage1
    call    bios_put

    pop     si
    push    si
    call    bios_put

    mov     si, str_crlf
    call    bios_put

    pop     si
    ret

; uint24_t lba_to_chs(uint16_t lba_addr /* AX */);
;
; Convert Logical Block Address scheme (LBA) to Cylinder-Head-Sector
; scheme (CHS):
;
;   Cylinder/Track: (LBA / SectorsPerTrack) / head_count
;   Head:           (LBA / SectorsPerTrack) % head_count
;   Sector:         (LBA % SectorsPerTrack) + 1
;
; We will return:
;
;   - The Sector number in CX[0..5]
;   - The lower 8 bits of the Cylinder number in CX[8..15] and the upper two
;     bits in CX[6..7]
;   - The Head number in DX[8..15] (DH)
;
; Because it's the format that the BIOS interrupt 0x13,2 expects:
;
;     |          |    CH    |    CL    |    DH    |    DL    |
;     |----------|----------|----------|----------|----------|
;     | Cylinder | 76543210 | 98       |          |          |
;     | Head     |          |          | 76543210 |          |
;     | Sector   |          |   543210 |          |          |
;     | Drive    |          |          |          |   ????   |
;
; Therefore, note that CX and the high part of the DX register (DH) will be
; overwritten by this call. The lower part of the DX register (DL) will be
; preserved.
lba_to_chs:
    push    ax
    push    dx

    ; First, calculate Sector and store in CX.
    xor     dx, dx                                ; For DIV
    div     word [bpb + bpb_t.sectors_per_track]  ; DX = AX % SpT; AX /= SpT;
    inc     dx                                    ; DX++;
    and     dx, 0b00111111                        ; Only keep lower 6 bits
    mov     cx, dx                                ; Return Sector in CX[0..5]

    ; Now that AX contains (LBA / SpT), get Cylinder and Head.
    xor     dx, dx
    div     word [bpb + bpb_t.head_count]   ; DX = AX % heads; AX /= heads;
    mov     dh, dl                          ; Return Head (DL) in DX[8..15]

    ; Return bits [0..7] of Cylinder (AX) in CX[8..15], and bits [8..9] of
    ; Cylinder in CX[6..7]. For the first operation, we copy 'AL' to 'CH'
    ; directly; for the second operation, we move AH[0..1] (i.e. AX[8..9]) to
    ; AH[6..7], and we then OR that with the sector we stored in CX[0..5].
    mov     ch, al          ; CX[8..15] = cylinder[0..7];
    shl     ah, 6           ; Shift lower 2 bits of AH to its upper two bits
    or      cl, ah          ; CX[6..7] = cylinder[8..9];

    pop     ax              ; Restore old DX into AX
    mov     dl, al          ; Only restore the original DL, keeping our DH
    pop     ax              ; Restore old AX into AX
    ret

; void bios_disk_read(uint16_t src,          /* AX */
;                     uint8_t  num_sectors,  /* CL */
;                     uint8_t* dst);         /* ES:BX */
;
; Read the specified number of sectors (CL) from the specified LBA address (AX)
; into the specified address in the "Extra" segment (ES:BX).
bios_disk_read:
    push    ax
    push    cx
    push    dx
    push    di

    ; Convert our first argument AX, in Logical Block Address scheme, to
    ; Cylinder-Head-Sector, storing the results in CH, CL and DH.
    ;
    ; TODO: Assert that the "sector number" argument (CL) is [1..128]
    push    cx          ; Preserve nSectors (CL) from call to 'lba_to_chs'
    call    lba_to_chs

    ; Load the number of sectors (which used to be in CL, now in the top of the
    ; stack) into the lower part of AX, and the BIOS disk function code into the
    ; high part of AX.
    pop     ax
    mov     ah, BIOS_DRIVE_READ

    ; Load the drive number into DL, needed by the BIOS.
    mov     dl, [bpb + ebpb_t.drive_number]

    ; If a read operation fails, we will retry it an arbitrary number of
    ; times. The DI register will be used to keep track of this counter.
    mov     di, DISK_RETRY_NUM

.loop:
    ; Preserve all registers in case the BIOS call modifies them, set the carry
    ; flag in case the BIOS doesn't automatically set it, perform the actual
    ; disk interrupt, and restore the old registers.
    pusha
    stc
    int     BIOS_INT_DISK
    popa

    ; If the carry flag became unset after the BIOS call, the read succeded.
    jnc     .done

    ; If the carry flag is still set, the read operation failed. Check if we are
    ; supposed to keep trying according to our counter in DI.
    dec     di
    test    di, di
    jz      .read_error

    ; If we still have retries left, reset the disk, decrease the counter and
    ; loop.
    call    bios_disk_reset
    jmp     .loop

.read_error:
    mov     al, ERR_BIOS_READ_SECTORS
    jmp     die_err

.done:
    pop     di
    pop     dx
    pop     cx
    pop     ax
    ret

; void bios_disk_reset(void);
;
; Reset the disk controller for the drive number stored in the EBPB.
bios_disk_reset:
    pusha
    mov     ah, BIOS_RESET_DISK_SYSTEM
    mov     dl, [bpb + ebpb_t.drive_number]
    stc
    int     BIOS_INT_DISK
    popa

    ; If the carry flag became unset after the BIOS call, it succeded.
    jnc     .done

    ; If the carry flag is still set after the BIOS call, it failed. Abort.
    mov     al, ERR_BIOS_RESET
    jmp     die_err

.done:
    ret

;-------------------------------------------------------------------------------
; FAT12 functions

; uint16_t get_stage2_cluster(uint16_t root_dir_lba,        /* AX */
;                             uint8_t root_dir_sectors);    /* CL */
;
; Search the FAT12 root directory for a file named STAGE2_FILENAME, and return
; its first cluster index in the FAT. The value is returned in AX, and CL is
; preserved.
;
; NOTE: The SI, DI and BX registers are overwritten with undefined data and AX
; is overwritten with the result, but CL is preserved.
get_stage2_cluster:
    push    cx

    ; Load the entire root directory into the specified buffer address, which we
    ; assume is safe to write to.
    mov     bx, SCRATCH_BUFFER_ADDR
    call    bios_disk_read

    ; Clear direction flag once for CMPSB below
    cld

    ; AX will be used to count the number of entries read, and will be compared
    ; against the number of entries in the root directory.
    xor     ax, ax

.loop:
    ; Iterate the root directory, comparing each filename (DI) with the target
    ; 'stage2_filename' (SI).
    mov     di, bx
    mov     si, stage2_filename
    mov     cx, %strlen(STAGE2_FILENAME)

    ; Compare the strings with:
    ;   - REPE: Repeat string instruction while operands are equal, and while CX
    ;     is not 0.
    ;   - CMPSB: Compare DS:SI with ES:DI, incrementing (since DF=0) SI and DI.
    ; Note that CMPSB increments DI, so BX is used to store/increment the
    ; initial position.
    repe cmpsb

    ; If the ZF flag is set after the previous instructions, all bytes matched
    je      .done

    ; Continue searching in the next entry.
    add     bx, dir_entry_t_size
    inc     ax
    cmp     ax, [bpb + bpb_t.dir_entries_count]
    jl      .loop

    ; Iterated all root directory entries.
    mov     si, str_file_not_found
    jmp     die

.done:
    mov     ax, [bx + dir_entry_t.cluster_idx_low]

    pop     cx
    ret

; void read_file_contents(uint16_t first_cluster_idx, /* CX */
;                         uint16_t data_region_lba,   /* DX */
;                         uint8_t* dst);              /* ES:BX */
;
; Read the clusters that form the specified FAT12 file into the specified memory
; address.
;
; NOTE: The AX, CX, DX and SI registers are overwritten, but BX is preserved.
;
; TODO: If there isn't enough space on Stage 1, we could make this function a
; bit less general by removing the BX argument and just using STAGE2_ADDR.
;
; TODO: This function limits the size of Stage 2 to 0xFFFF.
read_file_contents:
    push    bx
    push    cx

    ; Read the FAT into the scratch buffer.
    mov     ax, [bpb + bpb_t.reserved_sectors]  ; First FAT sector
    mov     cl, [bpb + bpb_t.sectors_per_fat]   ; FAT size in sectors
    mov     bx, SCRATCH_BUFFER_ADDR             ; Destination
    call    bios_disk_read

    pop     cx                  ; CX = first_cluster_idx
    pop     bx                  ; BX = dst
    push    bx                  ; Store again for when we return

.loop:
    ; Did we reach the end of the linked list of cluster indexes?
    cmp     cx, 0xFF8
    jge     .done

    ; Get the absolute sector number corresponding to the current cluster
    ; index. Note that this cluster index was relative to the FAT at this
    ; point (see my article for more information).
    mov     ax, cx              ; AX = cur_cluster_idx
    sub     ax, 2               ; AX -= 2       // Not relative to FAT anymore
    mul     byte [bpb + bpb_t.sectors_per_cluster] ; AX = AL * SpC
    add     ax, dx              ; AX += data_region_lba

    push    cx                  ; Preserve current cluster index

    ; Read a single cluster from AX into ES:BX.
    xor     ch, ch
    mov     cl, [bpb + bpb_t.sectors_per_cluster]
    call    bios_disk_read

    ; Skip over the bytes we just read, for the next iteration.
    push    dx                                  ; Overwritten by MUL with WORD
    mov     ax, cx                              ; AX = written_sectors
    mul     word [bpb + bpb_t.bytes_per_sector] ; DX:AX = written_bytes
    add     bx, ax                              ; dst += written_bytes
    pop     dx

    pop     cx                  ; CX = cur_cluster_idx

    ; Move to the next index in the linked list.
    ;
    ; First, transform the 12-bit FAT index into the absolute 8-bit index.
    mov     ax, cx              ; AX = cur_cluster_idx
    shr     ax, 1               ; AX = cur_cluster_idx / 2
    add     cx, ax              ; CX = cur_cluster_idx + (cur_cluster_idx / 2)

    ; Read the two bytes at the current FAT index.
    ;
    ; Note that we can only use certain registers for addressing in 16-bit mode,
    ; so we can't store the address in AX.
    mov     si, SCRATCH_BUFFER_ADDR     ; Address where the FAT is loaded
    add     si, cx                      ; SI = (uint8_t*)fat + absolute_idx
    mov     ax, [es:si]                 ; AX = next_cluster_idx  // Extra nibble

    ; Check if the absolute 8-bit index is odd or even. Depending on this, we
    ; will keep the upper or lower 3 nibbles, respectively.
    test    ax, 0b0000000000000001
    jz      .even

    ; NOTE: We assume that the current machine has the same endianness as the
    ; FAT filesystems (little-endian).
    shr     ax, 4               ; Keep upper 3 nibbles (index was odd)
    jmp     .continue

.even:
    and     ax, 0xFFF           ; Keep lower 3 nibbles (index was even)

.continue:
    jmp     .loop

.done:
    pop     bx                  ; BX = original_dst
    ret

;-------------------------------------------------------------------------------
; Data

; No '.data' section because the string also needs to be inside the first 512
; bytes, and we need to add the padding. Also note the position of the string
; inside the file. After the file is placed into 0x7C00, the BIOS will jump to
; the first instruction, so the entry point needs to be first.
str_searching:   db "Loading `"
stage2_filename: db STAGE2_FILENAME
                 db `'\0`

str_err:         db "ERR("
err_placeholder: db "?"
                 db `)\0`

str_stage1: db `S1: \0`
str_crlf:   db `\r\n\0`

str_file_not_found:   db `Not found\0` ; Stage 2 file not found in root directory

;-------------------------------------------------------------------------------
; Bootable signature

; Make sure we have enough space for the BIOS signature. See comment bellow.
%if ($-$$) > 510
%error "Binary is too large for boot sector (+512 bytes)."
%endif

; Fill the rest of the binary up to 510 with zeros. We subtract the address of
; the current instruction ($) from the address of the start of the current
; section ($$). Note that according to our linker script (cfg/bootloader.ld),
; the first section will be the current one (.text).
times 510 - ($ - $$) db 0x00

; The BIOS will look for the '55 AA' signature in bytes 510..511. Note the
; endianness.
db 0x55, 0xAA
