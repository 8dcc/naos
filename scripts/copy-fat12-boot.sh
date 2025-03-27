#!/usr/bin/env bash
#
# Copyright 2025 8dcc. All Rights Reserved.
#
# This file is part of naos.
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program.  If not, see <https://www.gnu.org/licenses/>.
set -e

# ------------------------------------------------------------------------------

DD='dd'


DDFLAGS=()

BOOT_SECTOR_SIZE=512

# ------------------------------------------------------------------------------

if [ $# -ne 2 ]; then
    echo "Usage: $(basename "$0") SRC DST" 1>&2
    exit 1
fi

assert_cmd() {
    if [ ! "$(command -v "$1")" ]; then
        echo "$(basename "$0"): The '$1' command is not installed." 1>&2
        exit 1
    fi
}

assert_file_size() {
    if [ "$(stat --printf="%s" "$1")" -lt "$BOOT_SECTOR_SIZE" ]; then
        echo "$(basename "$0"): Expected '$1' to be at least 512 bytes." 1>&2
        exit 1
    fi
}

g_src=$1
g_dst=$2

assert_cmd 'stat'
assert_cmd 'dd'

assert_file_size "$g_src"
assert_file_size "$g_dst"

# ------------------------------------------------------------------------------

# Current offset in the files.
g_cur_offset=0

# copy_next_bytes(num_bytes);
#
# Copy the next N bytes of 'g_src' into 'g_dst'.
copy_next_bytes() {
    local num_bytes="$1"
    local tmp_file

    tmp_file="$(mktemp --tmpdir "copy-fat12-boot.XXX.bin")"

    # Copy preceding bytes to the 'num_bytes' offset from the destination.
    $DD if="$g_dst"    \
        of="$tmp_file" \
        bs=1           \
        count="$g_cur_offset"

    # Actual byte replacement.
    #
    # Note that the block size ('bs') affects both the 'count' and the 'skip'
    # arguments.
    #
    # Meaning of last arguments:
    #   - oflag=append: Append to the file (also copy previous).
    #   - conv=notrunc: Don't truncate the file (also copy subsequent bytes).
    $DD if="$g_src"          \
        of="$tmp_file"       \
        bs=1                 \
        count="$num_bytes"   \
        skip="$g_cur_offset" \
        oflag=append         \
        conv=notrunc

    # Calculate new offset for future calls.
    g_cur_offset=$((g_cur_offset + num_bytes))

    # Copy subsequent bytes to the from the destination, skipping after the ones
    # we just copied.
    $DD if="$g_dst"          \
        of="$tmp_file"       \
        bs=1                 \
        skip="$g_cur_offset" \
        oflag=append         \
        conv=notrunc

    mv "$tmp_file" "$g_dst"
}

# skip_next_bytes(num_bytes);
#
# Skip the next N bytes of 'g_src' into 'g_dst'.
skip_next_bytes() {
    num_bytes="$1"
    g_cur_offset=$((g_cur_offset + num_bytes))
}

# ------------------------------------------------------------------------------

# This is the target mask:
#
#   00000000: ffff ffff ffff ffff ffff ff00 0000 0000
#   00000010: 0000 0000 0000 0000 0000 0000 0000 0000
#   00000020: 0000 0000 0000 00ff ffff ffff ffff ffff
#   00000030: ffff ffff ffff 0000 0000 0000 0000 ffff
#   00000040: ffff ffff ffff ffff ffff ffff ffff ffff
#   ...
#   000001e0: ffff ffff ffff ffff ffff ffff ffff ffff
#   000001f0: ffff ffff ffff ffff ffff ffff ffff 0000
#
# Where 'ff' represents the bytes that should be copied from the DST to the SRC.

# Overwrite:
#   - Short jump and NOP (3 bytes)
#   - OEM name (8 bytes)
copy_next_bytes 11

# Skip:
#   - BIOS 3.31 BPB (25 bytes)
#   - Physical drive number (1 byte)
#   - Reserved (1 byte)
#   - Extended boot signature (1 byte)
skip_next_bytes 28

# Overwrite:
#   - Volume serial number (4 bytes)
#   - Volume label (11 bytes)
copy_next_bytes 15

# Skip (should match):
#   - File-system type (8 bytes)
skip_next_bytes 8

# Overwrite:
#   - Boot code and data (up to offset 509, inclusive)
copy_next_bytes 448

# Skip (should match):
#   - Bootable signature (2 bytes: '55 AA')
skip_next_bytes 2

if [ "$g_cur_offset" -ne "$BOOT_SECTOR_SIZE" ]; then
    echo "$(basename "$0"): Warning: Finished script at offset ${g_cur_offset}, expected ${BOOT_SECTOR_SIZE}." 1>&2
fi
