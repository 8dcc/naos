/*
 * This file contains the common linker configuration that should be included by
 * the Stage 1 and Stage 2 linker scripts.
 *
 * The information on this file should match the one in
 * 'include/boot_config.asm'. Note that the assembly sources shouldn't rely on
 * that file, but on the linker, whenever possible.
 */

/*
 * Memory address where the BIOS is supposed to load the Stage 1 binary.
 */
STAGE1_ADDR = 0x7C00;

/*
 * Offset in the image where the BIOS Parameter Block should be placed.
 */
BPB_OFFSET = 0xB;

/*
 * Absolute physical address of the BPB after the image is loaded by the BIOS.
 */
BPB_ADDR = (STAGE1_ADDR + BPB_OFFSET);

/*
 * Scratch buffer used by Stage 1 for storing arbitrary information.
 *
 * It is used, for example, for loading the FAT12 root directory (for searching
 * files) and the FAT itself (for getting the cluster numbers for the file).
 */
SCRATCH_BUFFER_ADDR = (STAGE1_ADDR + 512);

/*
 * Address where the Stage 1 should load the Stage 2 binary.
 */
STAGE2_ADDR = 0xA000;
