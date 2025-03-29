
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

# ------------------------------------------------------------------------------

.PHONY: all clean qemu qemu-debug

all: $(STAGE1_BIN)

clean:
	rm -f $(STAGE1_BIN)

qemu: $(STAGE1_BIN)
	$(QEMU) $(QEMUFLAGS) -boot a -drive file=$^,format=raw,readonly=on,if=floppy

qemu-debug:
	$(MAKE) QEMUFLAGS="$(QEMUFLAGS) -s -S" qemu

# ------------------------------------------------------------------------------

$(STAGE1_BIN): src/stage1.asm
	$(AS) $(ASFLAGS) -i$(dir $<) -o $@ $<
