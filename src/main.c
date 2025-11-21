#include "alloc.h"
#include "interrupts.h"
#include "string.h"
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

static void printEsp() {
    u32 esp;
    __asm__ volatile("mov %0, esp" : "=r"(esp));
    printf(mainWindow(), "0x%x\n", esp);
}

static void disableKernelCodeSegment() {
    extern u64 kernelCsd;
    kernelCsd &= ~(((u64)1) << 47);  // kernel segment descriptor P = 0
}

static void printTrap(unsigned x) {
    if (x == 0) {
        printf(mainWindow(), "EXPLODE\n");
        infiniteLoop();
    } else {
        printf(mainWindow(), "%u ", x);
    }
}

static void timerHandler(const InterruptCtx* ctx) {
    // clearWindow(w);
}

static void userspaceProgram() {
    for (unsigned i = 0;; ++i) {
        printInt(i);
    }

    infiniteLoop();
}

[[noreturn]] void kernelEntry() {
    kernelInit();

    setMasterDeviceMask(DISABLE_ALL);
    overrideIterruptHandler(TIMER_INTERRUPT_VECTOR, timerHandler);

    u8* stack = (u8*)mallocImmortal(4096, 16) + 4096;
    // printf(w, "%p\n", stack);

    WindowHandle w = addWindow(0, 0, VGA_ROWS, VGA_COLUMNS);
    initScreen();

    startProcess(userspaceProgram, stack, w);
}
