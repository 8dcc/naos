#-------------------------------------------------------------------------------
#
# Basic setup for debugging the 16-bit bootloader. Run with:
#
#   gdb --command='debug-bootloader.gdb'
#
# To add a breakpoint on a specific instruction/function, you need to first
# obtain the offset of that instruction with:
#
#   ndisasm -b 16 -e 0x3E stage1.bin
#
# And that to add the base address (0x7C00). Note that 0x3E is the offset of the
# first code byte after the EBPB, and the offsets shown in 'ndisasm' are
# relative to that instruction, so you would actually need to add 0x7C3E.
#
#-------------------------------------------------------------------------------

set architecture i8086

# Assuming qemu is running with flags '-s' and '-S' (i.e. break CPU at
# startup).
target remote :1234

# Stage 1 entry point
break *0x7c00

# Comment if something needs to be done before reaching the first instruction
# of Stage 1.
continue
