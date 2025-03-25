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
; This file provides structures used in FAT12 file systems.

%ifndef FAT12_STRUCTURES_ASM_
%define FAT12_STRUCTURES_ASM_

; BIOS Parameter Block (BPB) for DOS 3.31. It which describes the physical
; layout of a data storage volume.
struc bpb_t
    .bytes_per_sector:      resw 1
    .sectors_per_cluster:   resb 1
    .reserved_sectors:      resw 1
    .fat_count:             resb 1
    .dir_entries_count:     resw 1
    .total_sectors:         resw 1
    .media_descriptor_type: resb 1
    .sectors_per_fat:       resw 1
    .sectors_per_track:     resw 1
    .head_count:            resw 1
    .hidden_sectors:        resd 1
    .large_sector_count:    resd 1
endstruc

%if bpb_t_size != 25
%error "Expected the size of a BIOS Parameter Block to be 25 bytes."
%endif

; Extended BIOS Parameter Block (EBPB) used by FAT12 and FAT16 since DOS 4.0.
struc ebpb_t
    .bpb:           resb bpb_t_size ; BIOS Parameter Block, 25 bytes
    .drive_number:  resb 1
    .reserved:      resb 1
    .signature:     resb 1      ; Should be 0x29
    .volume_id:     resb 4      ; Serial number
    .volume_label:  resb 11     ; Arbitrary name, padded with blanks (0x20)
    .system_id:     resb 8      ; File system type, padded with blanks (0x20)
endstruc

%if ebpb_t_size != 51
%error "Expected the size of an Extended BIOS Parameter Block to be 51 bytes."
%endif

; Directory entry. Each element in the root directory will have this layout.
struc dir_entry_t
    .name:                  resb 11
    .attributes:            resb 1
    .reserved:              resb 1
    .creation_time_tenths:  resb 1  ; Tenths of a second
    .creation_time:         resw 1  ; Hour, minute, second
    .creation_date:         resw 1  ; Year, month, day
    .access_date:           resw 1  ; Year, month, day
    .cluster_idx_high:      resw 1  ; FAT32 only
    .modification_time:     resw 1  ; Hour, minute, second
    .modification_date:     resw 1  ; Year, month, day
    .cluster_idx_low:       resw 1
    .size:                  resd 1  ; Bytes
endstruc

%if dir_entry_t_size != 32
%error "Expected the size of a FAT Directory Entry to be 32 bytes."
%endif

%endif ; FAT12_STRUCTURES_ASM_
