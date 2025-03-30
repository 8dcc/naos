
AS=nasm
ASFLAGS=-f bin

QEMU=qemu-system-i386

# NOTE: The '-enable-kvm' and '-cpu host' options could be added, but they mess
# with GDB breakpoints, specially for bootloader debugging.
#
# See the comments in: https://stackoverflow.com/a/14269843/11715554
QEMUFLAGS=-rtc base=localtime            \
          -audiodev pa,id=audio0         \
          -machine pcspk-audiodev=audio0 \
          -monitor stdio

STAGE1_BIN=stage1.bin
STAGE1_SRC=src/stage1.asm

STAGE2_BIN=stage2.bin
STAGE2_SRC=src/stage2.asm

BOOT_IMG=boot.img

# ------------------------------------------------------------------------------

.PHONY: all clean qemu qemu-debug

all: $(BOOT_IMG)

clean:
	rm -f $(STAGE1_BIN) $(STAGE2_BIN)
	rm -f $(BOOT_IMG)

# TODO: These steps should be part of the parent Makefile.
qemu: $(BOOT_IMG)
	$(QEMU) $(QEMUFLAGS) -boot a -drive file=$^,format=raw,readonly=on,if=floppy

qemu-debug:
	$(MAKE) QEMUFLAGS="$(QEMUFLAGS) -s -S" qemu

# ------------------------------------------------------------------------------

# First, an empty image is created, then a base FAT12 filesystem is created.
#
# We specify many options related to floppy disks, but they don't necessarily
# have to match the values in the BPB in the Stage 1 binary, since the
# 'copy-fat12-boot.sh' script skips most of the BPB.
#
# TODO: This step should be part of the parent Makefile, since it will include
# the kernel.
$(BOOT_IMG): $(STAGE1_BIN) $(STAGE2_BIN)
	dd if=/dev/zero of=$@ bs=512 count=100
	mkfs.fat -F 12 -D 0 -M 0xF0 -s 1 -S 512 -r 112 $@
	mcopy -i $@ $^ "::/"
	../scripts/copy-fat12-boot.sh $(STAGE1_BIN) $(BOOT_IMG)

$(STAGE1_BIN): $(STAGE1_SRC)
	$(AS) $(ASFLAGS) -i$(dir $<) -o $@ $<

$(STAGE2_BIN): $(STAGE2_SRC)
	$(AS) $(ASFLAGS) -i$(dir $<) -o $@ $<
