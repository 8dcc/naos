
INCLUDE "./boot_config.ld"

/*
 * The Stage 1 is a flat binary, not a fancy ELF or 'a.out' file.
 *
 * Note that this can be overwritten with the '--oformat=FOO' linker option,
 * which is useful for building an ELF version for debugging.
 */
OUTPUT_FORMAT("binary" );

/*
 * Entry point of Stage 1, declared in 'stage1.asm'.
 */
ENTRY(stage1_entry);

SECTIONS {
    /*
     * Set the linker's Location Counter to the address where the BIOS will
     * load the Stage 1 binary.
     */
    . = STAGE1_ADDR;

    /*
     * In Stage 1, all of the code and data is stored in a single section, since
     * we need to control where each byte is, and only have 512 bytes available.
     */
    .text : {
        *(.text)
        ASSERT(bpb == BPB_OFFSET, "Invalid BIOS Parameter Block offset.");
        ASSERT(boot_signature == 510, "Invalid Bootable Signature offset.");
    }
}
