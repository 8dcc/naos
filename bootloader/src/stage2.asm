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

%include "include/bios_codes.asm"

bits 16

; TODO: Don't hard-code, include file.
org 0xA000

;-------------------------------------------------------------------------------

stage2_entry:
    mov     si, str_hello_world
    call    bios_puts
    jmp     halt

; void halt(void);
;
; Disable interrupts and stop execution.
halt:
    cli
    hlt
    jmp     halt

;-------------------------------------------------------------------------------

; void bios_put(const char* str /* SI */);
;
; Print the specified null-terminated string, to the BIOS console.
;
; TODO: We should avoid duplicating code in Stage 1 and Stage 2.
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

    mov     si, str_stage2
    call    bios_put

    pop     si
    push    si
    call    bios_put

    mov     si, str_crlf
    call    bios_put

    pop     si
    ret

;-------------------------------------------------------------------------------

str_stage2: db `S2: \0`
str_crlf:   db `\r\n\0`

str_hello_world: db `Hello, world!\0`
