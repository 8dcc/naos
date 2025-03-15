;------------------------------------------------------------------------------
; Entry point of our bootloader.
;
; The BIOS will look for the 0xAA55 signature in bytes 511..512, and loads us
; into 0x7C00.
;------------------------------------------------------------------------------

%include "include/bios_codes.asm"
%include "include/bios_structures.asm"

bits 16

; Specify base address where the BIOS will load us. We need to use NASM's
; built-in "linker" because we are generating a raw binary, and we can't link
; with 'ld'.
org 0x7C00

section .text

; Entry point of the bootloader. The Boot Record must start with a short jump to
; the real entry point followed by a NOP.
global _start
_start:
    jmp     short bootloader_entry
    nop

;-------------------------------------------------------------------------------

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
    at bpb_t.reserved_sectors,       dw 1     ; Includes boot sector
    at bpb_t.fat_count,              db 2
    at bpb_t.dir_entries_count,      dw 0xE0
    at bpb_t.total_sectors,          dw 2880  ; 1.44MiB / 512bps = 2880
    at bpb_t.media_descriptor_type,  db 0xF0  ; 3.5" floppy disk
    at bpb_t.sectors_per_fat,        dw 9
    at bpb_t.sectors_per_track,      dw 18
    at bpb_t.heads,                  dw 2
    at bpb_t.hidden_sectors,         dd 0

    ; Should only be set if entry at BPB offset 0x13 (bpb_t.total_sectors) is
    ; zero.
    at bpb_t.large_sector_count,     dd 0

    at ebpb_t.drive_number,          db 0            ; 0 for floppy, 0x80 for HDDs
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

bootloader_entry:
    ; Start by setting up the Data and Extra segments. We need to use an
    ; intermediate register to write to them.
    xor     ax, ax
    mov     ds, ax
    mov     es, ax

    ; We will also set up the Stack segment. Since the BIOS loaded us at address
    ; 0x7C00, and the stack grows downwards, we can use the current address as
    ; the bottom of the stack (The highest address).
    mov     ss, ax
    mov     sp, 0x7C00

    mov     si, msg_boot
    call    bios_puts

.halt:
    ; For now, halt the system
    ; TODO: Jump to kernel
    hlt
    jmp     .halt

;-------------------------------------------------------------------------------

; void bios_puts(const char* si);
bios_puts:
    push    ax
    push    bx
    push    si

.loop:
    lodsb                   ; Load byte from SI into AL, and increment SI
    test    al, al          ; Did we reach the end of the string?
    jz      .done

    mov     ah, BIOS_TTY_WRITE_CHAR
    mov     bh, 0x0                 ; Page number: 0
    int     BIOS_INT_VIDEO          ; BIOS video interrupt

    jmp     .loop

.done:
    pop     si
    pop     bx
    pop     ax
    ret

; uint24_t lba_to_chs(uint16_t ax);
;
; Convert Logical Block Address scheme (LBA) to Cylinder-Head-Sector
; scheme (CHS):
;
;   Cylinder/Track: (LBA / SectorsPerTrack) / heads
;   Head:           (LBA / SectorsPerTrack) % heads
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
    div     word [bpb + bpb_t.heads]        ; DX = AX % heads; AX /= heads;
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

;-------------------------------------------------------------------------------

; No '.data' section because the string also needs to be inside the first 512
; bytes, and we need to add the padding. Also note the position of the string
; inside the file. After the file is placed into 0x7C00, the BIOS will jump to
; the first instruction, so the entry point needs to be first.
msg_boot: db "Hello, world.", 13, 10, 0 ; "\r\n\0"

;-------------------------------------------------------------------------------

; Make sure we have enough space for the BIOS signature. See comment bellow.
%if ($-$$) > 510
%error "Binary is too large for boot sector (+512 bytes)."
%endif

; Fill the rest of the binary up to 510 with zeros. We subtract the address of
; the current instruction ($) from the address of the start of the current
; section ($$). Note that according to our linker script (cfg/bootloader.ld),
; the first section will be the current one (.text).
times 510 - ($ - $$) db 0x00

; The BIOS will look for the 0xAA55 signature in bytes 511..512. Note the
; endianness.
db 0x55, 0xAA
