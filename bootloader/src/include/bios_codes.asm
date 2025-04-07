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
; This file provides macros for the BIOS interrupt call codes.

%ifndef BIOS_CODES_ASM_
%define BIOS_CODES_ASM_

; BIOS interrupts
%define BIOS_INT_VIDEO 0x10
%define BIOS_INT_DISK  0x13
%define BIOS_INT_MISC  0x15

; Video Services. These values should be in AH to indicate the function
; when using the interrupt 0x10.
%define BIOS_SET_VIDEO_MODE                0x00 ; Set Video Mode
%define BIOS_SET_CURSOR_SHAPE              0x01 ; Set Cursor Shape
%define BIOS_SET_CURSOR_POS                0x02 ; Set Cursor Position
%define BIOS_GET_CURSOR_POS                0x03 ; Get Cursor Position And Shape
%define BIOS_GET_LIGHT_PEN_POS             0x04 ; Get Light Pen Position
%define BIOS_SET_DISPLAY_PAGE              0x05 ; Set Display Page
%define BIOS_SCROLL_SCREEN_UP              0x06 ; Clear/Scroll Screen Up
%define BIOS_SCROLL_SCREEN_DOWN            0x07 ; Clear/Scroll Screen Down
%define BIOS_READ_CHAR_AND_ATTR_AT_CURSOR  0x08 ; Read Character and Attribute at Cursor
%define BIOS_WRITE_CHAR_AND_ATTR_AT_CURSOR 0x09 ; Write Character and Attribute at Cursor
%define BIOS_WRITE_CHAR_AT_CURSOR          0x0A ; Write Character at Cursor
%define BIOS_SET_BORDER_COL                0x0B ; Set Border Color
%define BIOS_WRITE_GRAPHICS_PX             0x0C ; Write Graphics Pixel
%define BIOS_READ_GRAPHICS_PX              0x0D ; Read Graphics Pixel
%define BIOS_TTY_WRITE_CHAR                0x0E ; Write Character in TTY Mode
%define BIOS_GET_VIDEO_MODE                0x0F ; Get Video Mode
%define BIOS_SET_PALETTE_REGISTERS         0x10 ; Set Palette Registers (EGA, VGA, SVGA)
%define BIOS_CHAR_GENERATOR                0x11 ; Character Generator (EGA, VGA, SVGA)
%define BIOS_ALTERNATE_SELECT_FUNCS        0x12 ; Alternate Select Functions (EGA, VGA, SVGA)
%define BIOS_WRITE_STR                     0x13 ; Write String
%define BIOS_GET_OR_SET_DISPLAY_COMB       0x1A ; Get or Set Display Combination Code (VGA, SVGA)
%define BIOS_GET_FUNCTIONALITY_INFO        0x1B ; Get Functionality Information (VGA, SVGA)
%define BIOS_SAVE_OR_RESTORE_VIDEO_STATE   0x1C ; Save or Restore Video State (VGA, SVGA)
%define BIOS_VESA_BIOS_EXTENSION_FUNCS     0x4F ; VESA BIOS Extension Functions (SVGA)

; Low Level Disk Services. These values should be in AH to indicate the function
; when using the interrupt 0x13.
%define BIOS_RESET_DISK_SYSTEM          0x00 ; Reset Disk System
%define BIOS_DRIVE_LAST_STATUS          0x01 ; Status of Last Drive Operation
%define BIOS_DRIVE_READ                 0x02 ; Read Sectors From Drive
%define BIOS_DRIVE_WRITE                0x03 ; Write Sectors To Drive
%define BIOS_VERIFY_SECTORS             0x04 ; Verify Sectors
%define BIOS_FORMAT_TRACK               0x05 ; Format Track
%define BIOS_FORMAT_TRACK2              0x06 ; Format Track Set Bad Sector Flags
%define BIOS_FORMAT_DRIVE               0x07 ; Format Drive starting at Track
%define BIOS_DRIVE_READ_PARAMS          0x08 ; Read Drive Parameters
%define BIOS_INIT_DISK_CONTROLLER       0x09 ; Initialize Disk Controller
%define BIOS_DRIVE_LREAD                0x0A ; Read Long Sectors From Drive
%define BIOS_DRIVE_LWRITE               0x0B ; Write Long Sectors To Drive
%define BIOS_MOVE_HEAD_TO_CYL           0x0C ; Move Drive Head To Cylinder
%define BIOS_RESET_DISK_DRIVES          0x0D ; Reset Disk Drives
%define BIOS_TEST_CONTROLLER_READ       0x0E ; Controller Read Test
%define BIOS_TEST_CONTROLLER_WRITE      0x0F ; Controller Write Test
%define BIOS_TEST_DRIVE_READY           0x10 ; Test Whether Drive Is Ready
%define BIOS_DRIVE_RECALIBRATE          0x11 ; Recalibrate Drive
%define BIOS_TEST_CONTROLLER_RAM        0x12 ; Controller RAM Test
%define BIOS_TEST_DRIVE                 0x13 ; Drive Test
%define BIOS_CONTROLLER_DIAGNOSTIC      0x14 ; Controller Diagnostic
%define BIOS_DRIVE_READ_TYPE            0x15 ; Read Drive Type
%define BIOS_DETECT_MEDIA_CHANGE        0x16 ; Detect Media Change
%define BIOS_SET_MEDIA_TYPE_FOR_FORMAT1 0x17 ; Set Media Type For Format (used by DOS versions <=3.1)
%define BIOS_SET_MEDIA_TYPE_FOR_FORMAT2 0x18 ; Set Media Type For Format (used by DOS versions >=3.2)
%define BIOS_PARK_HEADS                 0x19 ; Park Heads
%define BIOS_TEST_EXTENSIONS            0x41 ; Test Whether Extensions Are Available
%define BIOS_DRIVE_READ_SECTORS         0x42 ; Read Sectors From Drive
%define BIOS_DRIVE_WRITE_SECTORS        0x43 ; Write Sectors To Drive
%define BIOS_VERIFY_SECTORS1            0x44 ; Verify Sectors
%define BIOS_DRIVE_TOGGLE_LOCK          0x45 ; Lock/Unlock Drive
%define BIOS_DRIVE_EJECT                0x46 ; Eject Drive
%define BIOS_DRIVE_HEAD_TO_SECTOR       0x47 ; Move Drive Head To Sector
%define BIOS_DRIVE_READ_PARAMS1         0x48 ; Read Extended Drive Parameters
%define BIOS_DETECT_MEDIA_CHANGE1       0x49 ; Detect Media Change
%define BIOS_DRIVE_GET_EMULATION_TYPE   0x4B ; Get Drive Emulation Type

; Miscellaneous system services
%define BIOS_A20_ENABLE  0x2401 ; TODO: Source?
%define BIOS_A20_STATUS  0x2402 ; TODO: Source?
%define BIOS_A20_SUPPORT 0x2403 ; TODO: Source?

%endif ; BIOS_CODES_ASM_
