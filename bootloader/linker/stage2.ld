
INCLUDE "./boot_config.ld"

/*
 * Just like in Stage 1, the Stage 2 is a flat binary, but this one is less
 * restrictive.
 *
 * Note that this can be overwritten with the '--oformat=FOO' linker option,
 * which is useful for building an ELF version for debugging.
 */
OUTPUT_FORMAT("binary");

/*
 * Entry point of Stage 2, declared in 'stage2.asm'.
 */
ENTRY(stage2_entry);

SECTIONS {
    /*
     * Set the linker's Location Counter to the address where the Stage 1 will
     * load the Stage 2 binary.
     *
     * It is important that this address matches the 'STAGE2_ADDR' macro defined
     * in 'include/boot_config.asm'.
     *
     * TODO: Perhaps move to a 'config.ld' script that also gets used when
     * linking the Stage 1 binary.
     */
    . = STAGE2_ADDR;

    /* Text (i.e. code) */
    .text : {
        *(.text)
    }

    /* Alignment is not necessary, but feels cleaner */
    . = ALIGN(512);

    /* Read-only data (e.g. strings) */
    .rodata : {
        *(.rodata*)
    }
}

/* Defined in 'stage1.asm' */
bpb = BPB_ADDR;
