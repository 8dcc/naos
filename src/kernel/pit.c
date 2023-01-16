
/* Programmable Interval Timer. See: https://wiki.osdev.org/PIT */

#include <kernel/pit.h>
#include <kernel/io.h>

/* pit_init: initialize the programmable interval timer with the specified frequency
 * in interrupts per seccond (freq 1000 would mean 1000 ticks in 1 sec) */
void pit_init(uint32_t freq) {
    /* freq should be how many HZs it should wait bewteen sending interrupt. We pass
     * the frequency per second to convert it to HZ (by dividing how many HZs are in
     * a sec) */
    freq = PIT_INTERVAL_TO_FREQ(freq);

    /* Select mode/cmd and flags */
    io_outb(PIT_CHANNEL_CMD, PIT_FLAG_CHANNEL_0 | PIT_FLAG_ACCESS_LOHI |
                               PIT_FLAG_MODE_RATE | PIT_FLAG_BINARY_OFF);

    /* Set reload values to the current ms */
    io_outb(PIT_CHANNEL_0, (uint8_t)(freq & 0xFF));
    io_outb(PIT_CHANNEL_0, (uint8_t)((freq & 0xFF00) >> 8));
}

/* Stores the remaining ticks for the current PIT wait. Each tick is supposed to be 1
 * ms. */
static volatile uint64_t ticks = 0;

/* pit_dec: decrease the current tick count. Called from the PIT interrupt, on:
 * src/kernel/idt_asm.asm */
void pit_dec(void) {
    if (ticks > 0)
        ticks--;

    /* Tell CPU that it's okay to resume interrupts. See:
     * https://wiki.osdev.org/Interrupts#From_the_OS.27s_perspective */
    io_outb(0x20, 0x20);
}

/* pit_set_ticks: sets the current PIT tick count. Called from the sleep functions
 * for setting the ammount of time we want to wait (src/libk/time.c) */
void pit_set_ticks(uint64_t num) {
    ticks = num;
}

/* pit_get_ticks: returns the current PIT tick count. Called from the sleep functions
 * for checking if the ticks we set have passed or not (reached 0) */
uint64_t pit_get_ticks(void) {
    return ticks;
}

