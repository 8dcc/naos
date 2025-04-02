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
; This file provides basic BIOS printing functions. Specifically:
;
;     void bios_print(const char* str /* SI */);
;     void bios_println(const char* str /* SI */);
;     void die_err(char error_code /* AL */);
;     void die(const char* str /* SI */);
;     void halt(void);
;
; This file is meant to be included by the Stage 1 and Stage 2 bootloader, to
; avoid duplicating code. Note that the code will be assembled and loaded twice
; (once for each stage), but that isn't a big deal.
;
;-------------------------------------------------------------------------------
; Functions

; void bios_print(const char* str /* SI */);
;
; Print the specified null-terminated string, to the BIOS console.
;
; TODO: The BIOS might overwrite more registers inside the interrupt.
bios_print:
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

; void bios_println(const char* str /* SI */);
;
; Print an identifier, the specified null-terminated string and along with a
; newline, to the BIOS console.
bios_println:
    push    si

    call    bios_print          ; print(str)
    mov     si, str_crlf
    call    bios_print          ; print("\r\n")

    pop     si
    ret

; void die_err(char error_code /* AL */);
;
; Print the specified error code and die. See the 'include/error_codes.asm'
; file for more information.
die_err:
    mov     [err_placeholder], al
    mov     si, str_err
    ; Fallthrough to 'die'

; void die(const char* str /* SI */);
;
; Print the specified error message and halt.
die:
    call    bios_println
    ; Fallthrough to 'halt'

; void halt(void);
;
; Disable interrupts and stop execution.
halt:
    cli
    hlt
    jmp     halt

;-------------------------------------------------------------------------------
; Global variables

str_crlf:   db `\r\n\0`

str_err:         db "ERR("
err_placeholder: db "?"
                 db `)\0`
