; Copyright 2025 8dcc. All Rights Reserved.
;
; This file is part of naos.
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
;-------------------------------------------------------------------------------
;
; This file provides basic BIOS disk functions. Specifically:
;
;     uint24_t lba_to_chs(uint16_t lba_addr /* AX */);
;     void bios_disk_read(uint16_t src,          /* AX */
;                         uint8_t  num_sectors,  /* CL */
;                         uint8_t* dst);         /* ES:BX */
;     void bios_disk_reset(void);
;
; This file is meant to be included by the Stage 1 and Stage 2 bootloader, to
; avoid duplicating code. Note that the code will be assembled and loaded twice
; (once for each stage), but that isn't a big deal.
;
;-------------------------------------------------------------------------------
; Includes

%include "include/error_codes.asm"
%include "include/bios_codes.asm"
%include "include/fat12_structures.asm"

;-------------------------------------------------------------------------------
; Macros

; Number of retries for the BIOS disk operations, since floppy disks can be
; unreliable.
%define DISK_RETRY_NUM 3

;-------------------------------------------------------------------------------
; Functions

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
; into the specified address in the "Extra" segment (ES:BX). Reads the drive
; number from the EBPB.
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
; Reset the disk controller for the drive number stored in the EBPB. Reads the
; drive number from the EBPB.
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
