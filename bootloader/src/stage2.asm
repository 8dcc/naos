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

bpb: equ BPB_ADDR

;-------------------------------------------------------------------------------
; Stage 2 entry point

bits 16
section .text

global stage2_entry
stage2_entry:
    mov     si, str_loaded
    call    bios_println
    jmp     halt

;-------------------------------------------------------------------------------
; Functions from Stage 1

; Included from external file to avoid duplicating code in Stage 1 and Stage 2.
%include "bios_disk.asm"
%include "bios_print.asm"

;-------------------------------------------------------------------------------
; Read-only data

section .rodata

str_loaded:
    db `Initialized Stage 2 at address `, %num(STAGE2_ADDR, -1, -16), `\0`
