
%ifndef BIOS_STRUCTURES_ASM
%define BIOS_STRUCTURES_ASM

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
    .heads:                 resw 1
    .hidden_sectors:        resd 1
    .large_sector_count:    resd 1
endstruc

%if bpb_t_size != 25
%error "Expected the size of a BIOS Parameter Block to be 25 bytes."
%endif

; Extended BIOS Parameter Block (EBPB) used by FAT12 and FAT16 since DOS 4.0.
struc ebpb_t
    .bpb            resb bpb_t_size ; BIOS Parameter Block, 25 bytes
    .drive_number:  resb 1
    .reserved:      resb 1

    ; A signature of 0x28 indicates that the DOS 3.4 format should be used, with
    ; just the volume ID following. In this case, it should be 0x29 to indicate
    ; DOS 4.0 format, since we have the other 2 fields.
    .signature:     resb 1

    .volume_id:     resb 4      ; Serial number
    .volume_label:  resb 11     ; Arbitrary name, padded with blanks (0x20)
    .system_id:     resb 8      ; File system type, padded with blanks (0x20)
endstruc

%if ebpb_t_size != 51
%error "Expected the size of an Extended BIOS Parameter Block to be 51 bytes."
%endif

%endif ; BIOS_STRUCTURES_ASM
