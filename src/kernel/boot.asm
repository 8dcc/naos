;------------------------------------------------------------------------------
; Entry point of the OS after the bootloader. Performs a basic setup and calls
; kernel_main (src/kernel/kernel.c)
;------------------------------------------------------------------------------

MB_ALIGN    equ 1 << 0      ; Align loaded modules on page boundaries
MB_MEMINFO  equ 1 << 1      ; Provide memory map
MB_GFX      equ 1 << 2      ; Use GFX (For the framebuffer, see bellow)

; Multiboot flag field
MB_FLAGS    equ MB_ALIGN | MB_MEMINFO | MB_GFX

; Magic number for the bootloader.
MB_MAGIC    equ 0x1BADB002

; Checksum of above to prove we are multiboot.
MB_CHECKSUM equ -(MB_MAGIC + MB_FLAGS)

; Declare the multiboot header that marks the program as a kernel. These are
; magic values that are documented in the multiboot standard. The bootloader
; will search for this signature in the first 8KiB of the kernel file, aligned
; at a 32-bit boundary.
; Here we ask for graphics mode type 0, which is linear type. We use 0 as width
; and height because in this case we don't care about a "default resolution",
; we want to use what the bootloader gives us. Depth will be the amount of
; bytes per pixel.
;
; More information on alignment:
;   https://stackoverflow.com/questions/19608845/understanding-assembly-mips-align-and-memory-addressing
; More information on next values & credits:
;   https://www.gnu.org/software/grub/manual/multiboot/html_node/Header-address-fields.html#Header-address-fields
;   https://www.gnu.org/software/grub/manual/multiboot/html_node/Header-graphics-fields.html#Header-graphics-fields
;   https://github.com/ozkl/soso/blob/master/kernel/boot.asm#L17-L26
section .multiboot
    align 4
    dd      MB_MAGIC
    dd      MB_FLAGS
    dd      MB_CHECKSUM
    dd      0x00000000      ; header_addr
    dd      0x00000000      ; load_addr
    dd      0x00000000      ; load_end_addr
    dd      0x00000000      ; bss_end_addr
    dd      0x00000000      ; entry_addr
    dd      0x00000000      ; (graphics) requested mode_type (See last comment)
    dd      0x00000000      ; (graphics) width
    dd      0x00000000      ; (graphics) height
    dd      32              ; (graphics) depth

; Create a symbol at the bottom of the stack, allocate 16384 bytes and create
; another symbol at the top. Keep in mind that the stack grows downwards in
; x86. The stack is in its own section because in nasm, .bss is marked as
; nobits which means the kernel file is smaller, because it does not contain an
; initialized stack.
; From https://www.nasm.us/doc/nasmdoc8.html
;   > nobits defines the section to be one with no explicit contents given,
;   > such as a BSS section.
; The stack must be 16 byte aligned acording to the System V ABI. The compiler
; will assume the stack is properly aligned and failure to align the stack will
; result in undefined behavior.
section .bss
    global stack_bottom     ; Global used in src/kernel/multitask.asm
    global stack_top

    align   16
    stack_bottom:       ; Items pushed to the stack will get 'closer' to this.
        resb    0x4000  ; 16 KiB
    stack_top:

; Declare the kernel entry point.
section .text
    global _start:function (_start.end - _start)
    extern kernel_main                              ; src/kernel/kernel.c
    extern gdt_init                                 ; src/kernel/gdt.asm
    extern is_sse_supported                         ; src/kernel/util.asm
    extern sse_supported                            ; src/kernel/kernel.c
    extern is_msr_supported                         ; src/kernel/util.asm
    extern msr_supported                            ; src/kernel/kernel.c
    extern is_tsc_supported                         ; src/kernel/util.asm
    extern tsc_supported                            ; src/kernel/kernel.c

_start:
    ; The bootloader loaded us into 32bit protected mode on a x86 machine.
    ; Interrupts are disabled. Paging is disabled. The processor state is as
    ; defined in the multiboot standard. There is no stdlib. There are no
    ; security restrictions, safeguards or debugging mechanisms.

    ; To setup the stack, we move esp to the 'top' label we declared earlier,
    ; because the stack grows to a lower memory address in x86 assembly.
    mov     esp, stack_top

    ; Init the Global Descriptor Table
    call    gdt_init

    ; Initialize the FPU
    finit

%ifdef ENABLE_SSE
    call    is_sse_supported            ; Defined in util.asm
    test    eax, eax
    jnz     .enable_sse                 ; Returned true
    mov     [sse_supported], byte 0     ; Returned 0, set to var false

.enable_sse:
    ; Enable SSE
    mov     eax, cr0
    and     al, ~(1 << 2)               ; Clear CR0.EM[bit 2]
    or      al, (1 << 1)                ; Set CR0.MP[bit 1]
    or      al, (1 << 5)                ; Set CR0.NE[bit 5]
    mov     cr0, eax
    mov     eax, cr4
    or      ax, (1 << 9)                ; Set CR4.OSFXSR (9)
    or      ax, (1 << 10)               ; Set CR4.OSXMMEXCPT (10)
    mov     cr4, eax
%endif ; ENABLE_SSE

%ifdef DEBUG
    call    is_msr_supported            ; Defined in util.asm
    test    eax, eax
    jnz     .check_tsc                  ; Returned true
    mov     [msr_supported], byte 0     ; It was 0, set to false

.check_tsc:
    call    is_tsc_supported            ; Defined in util.asm
    test    eax, eax
    jnz     .debug_checks_done          ; Returned true
    mov     [tsc_supported], byte 0     ; It was 0, set to false

.debug_checks_done:
%endif ; DEBUG

    ; The ABI requires the stack to be 16 byte aligned at the time of the call
    ; instruction (Because it pushes the return address to the stack: 4 bytes).
    ; We aligned the stack to 16 bytes when reserving the stack and we are
    ; going to push 4 bytes (ebx). Push another 12 bytes.
    push    dword 0x00000000
    push    dword 0x00000000
    push    dword 0x00000000

    ; Push ebx, containing the multiboot information structure pointer, which
    ; we will pass to main as argument.
    ; See src/kernel/include/kernel/multiboot.h
    push    ebx

    ; Call the C entry point at src/kernel/kernel.c
    call    kernel_main

    ; We should not reach here. If the system has nothing more to do, put the
    ; computer into an infinite loop:
    ; - Disable interrupts with CLI (clear interrupt enable in EFLAGS)
    ; - Wait for the next interrupt to arrive with HLT (Halt instruction). Since
    ;   interrupts are disabled, will lock up the computer.
    ; - Jump to the hlt instruction in case it wakes up from a non-maskable
    ;   interrupt or due to system management mode.
    cli

.hang:
    hlt
    jmp     .hang

.end:
