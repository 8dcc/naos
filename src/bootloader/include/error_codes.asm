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
; ------------------------------------------------------------------------------
;
; Internal error codes for the Stage 1 bootloader. Each error should be a
; character that is displayed whenever an error ocurs.

%ifndef ERROR_CODES_ASM_
%define ERROR_CODES_ASM_

%assign ERR_BIOS_READ_INFO    '1' ; BIOS failed to read the drive information.
%assign ERR_BIOS_READ_SECTORS '2' ; BIOS failed to read sectors from drive.
%assign ERR_BIOS_RESET        '3' ; BIOS failed to reset disk system.

%endif ; ERROR_CODES_ASM_
