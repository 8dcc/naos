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

%ifndef BOOT_CONFIG_ASM_
%define BOOT_CONFIG_ASM_ 1

; Memory address where the BIOS is supposed to load the Stage 1 binary.
%assign STAGE1_ADDR 0x7C00

; Offset in the image where the BIOS Parameter Block should be placed.
%assign BPB_OFFSET 0xB

; Absolute physical address of the BPB after the image is loaded by the BIOS.
%assign BPB_ADDR (STAGE1_ADDR + BPB_OFFSET)

; Scratch buffer used by Stage 1 for storing arbitrary information.
;
; It is used, for example, for loading the FAT12 root directory (for searching
; files) and the FAT itself (for getting the cluster numbers for the file).
%assign SCRATCH_BUFFER_ADDR (STAGE1_ADDR + 512)

; Short name of the Stage 2 binary that should be found in the root directory of
; the FAT12 volume.
%define STAGE2_FILENAME "STAGE2  BIN"
%if %strlen(STAGE2_FILENAME) != 11
%error "Expected an 11-byte filename that followed the 8.3 scheme."
%endif

; Address where the Stage 1 should load the Stage 2 binary.
%assign STAGE2_ADDR 0xA000

%endif ; BOOT_CONFIG_ASM_
