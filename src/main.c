#include "alloc.h"
#include "interrupts.h"
#include "syscalls.h"
#include "userspace.h"
#include "utils.h"
#include "vga.h"

static void printImpl(const InterruptCtx* ctx) {
    const WindowHandle w = curWindowHandle();
    const usize n = ctx->edi;

    printf(w, "%d\n", n);
}

static void kernelInit() {
    setup8259();
    SyscallDescriptor syscalls[] = {(SyscallDescriptor){.vector = 0x30, .impl = printImpl}};
    setupInterrupts(syscalls, sizeof(syscalls) / sizeof(SyscallDescriptor));

    enableInterrupts;
}

static void userspaceProgram() {
    for (unsigned i = 0;; ++i) {
        printInt(i);
    }

    infiniteLoop();
}

[[noreturn]] void kernelEntry() {
    kernelInit();

    u8* stack = (u8*)mallocImmortal(4096, 16) + 4096;
    // printf(w, "%p\n", stack);

    WindowHandle w = addWindow(0, 0, VGA_ROWS, VGA_COLUMNS);
    initScreen();

    startProcess(userspaceProgram, stack, w);
}
