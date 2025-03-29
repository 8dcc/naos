#-------------------------------------------------------------------------------
#
# Basic setup for debugging the 16-bit bootloader. Run with:
#
#   gdb --command='debug-bootloader.gdb'
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
