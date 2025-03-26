
AS=nasm
ASFLAGS=-f bin

QEMU=qemu-system-i386
QEMUFLAGS=-rtc base=localtime            \
          -audiodev pa,id=audio0         \
          -machine pcspk-audiodev=audio0 \
          -enable-kvm                    \
          -cpu host                      \
          -monitor stdio

STAGE1_BIN=stage1.bin

# ------------------------------------------------------------------------------

.PHONY: all clean qemu

all: $(STAGE1_BIN)

clean:
	rm -f $(STAGE1_BIN)

qemu: $(STAGE1_BIN)
	$(QEMU) $(QEMUFLAGS) -boot a -drive file=$^,format=raw,readonly=on,if=floppy

# ------------------------------------------------------------------------------

$(STAGE1_BIN): src/stage1.asm
	$(AS) $(ASFLAGS) -i$(dir $<) -o $@ $<
