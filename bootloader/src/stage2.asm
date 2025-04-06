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

;------------------------------------------------------------------------------
; Includes

%include "include/boot_config.asm"
%include "include/error_codes.asm"
%include "include/bios_codes.asm"
%include "include/fat12_structures.asm"

extern bpb

;-------------------------------------------------------------------------------
; Stage 2 entry point

bits 16
section .text

global stage2_entry
stage2_entry:
    mov     si, str_loaded
    call    bios_println

    ; Try to enable the A20 line.
    call    enable_a20
    test    ax, ax
    jz      .a20_enabled

    ; The 'enable_a20' function returned false.
    mov     si, str_a20_error
    call    bios_println
    jmp     halt

.a20_enabled:
    ; The 'enable_a20' function returned true.
    mov     si, str_a20_enabled
    call    bios_println

    ; TODO

    jmp     halt

;-------------------------------------------------------------------------------
; Functions from Stage 1

; Included from external file to avoid duplicating code in Stage 1 and Stage 2.
%include "bios_disk.asm"
%include "bios_print.asm"

;-------------------------------------------------------------------------------
; A20 line functions

; bool enable_a20(void);
;
; Try to enable the A20 line with every supported method. Returns 1 in AX if the
; A20 line was enabled, or 0 otherwise.
enable_a20:
    pushf

    ; First, check if the A20 line is already enabled by the BIOS.
    call    is_a20_enabled
    test    ax, ax
    jz      .a20_enabled

    ; TODO

    ; If all of the above methods failed, signal the caller that we couldn't
    ; enable it.
    mov     ax, 0
    jmp     .done

.a20_enabled:
    mov     ax, 1

.done:
    popf
    ret

; bool is_a20_enabled(void);
;
; Returns 1 in AX if the A20 line is enabled, or 0 otherwise.
;
; This check is done by comparing the value at 0000:7DFE (which should contain
; the boot signature 0xAA55) with the value 1MiB higher, at FFFF:7E0E. If the
; two values are different it means that the A20 is enabled.
;
; TODO: Why is 0000:0500 and FFFF:0510 used below?
is_a20_enabled:
    pushf
    push    ds
    push    es
    push    si
    push    di

    mov     ax, 0x0000
    mov     ds, ax
    mov     si, 0x0500          ; DS:SI = 0000:0500

    mov     ax, 0xFFFF
    mov     es, ax
    mov     di, 0x0510          ; ES:DI = FFFF:0510

    ; Preserve values at DS:SI and ES:DI.
    mov     al, byte [ds:si]
    push    ax
    mov     al, byte [es:di]
    push    ax

    ; Clear address A and set address B. If address A also changed, they are
    ; equivalent, so we can assume that the A20 line is disabled.
    mov     byte [ds:si], 0x00
    mov     byte [es:di], 0xFF
    cmp     byte [ds:si], 0xFF

    ; Right after using CMP to set ZF, restore the values that were overwritten
    ; in DS:SI and ES:DI.
    pop     ax
    mov     byte [ds:si], al
    pop     ax
    mov     byte [es:di], al

    ; Now we can return based on the ZF flag. If the previous values we compared
    ; were equal, the A20 line is not enabled.
    mov     ax, 0
    je      .done
    mov     ax, 1

.done:
    pop     di
    pop     si
    pop     es
    pop     ds
    popf
    ret

;-------------------------------------------------------------------------------
; Read-only data

section .rodata

str_loaded:
    db `Initialized Stage 2 at address `, %num(STAGE2_ADDR, -1, -16), `\0`

str_a20_error:   db `Could not enable A20 line. Aborting...\0`
str_a20_enabled: db `Successfuly enabled A20 line\0`
