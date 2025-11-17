#include "alloc.h"
#include "string.h"
#include "interrupts.h"
#include "syscalls.h"
#include "userspace.h"
#include "utils.h"
#include "vga.h"

static WindowHandle w;

static void setupWindows() {
    w = addWindow(0, 0, VGA_ROWS, VGA_COLUMNS);
}

static void writeImpl(const InterruptCtx* ctx) {
    // currently this window handle is a massive security hole,
    // but it will be patched when I get an idea on how to identify
    // programs so that I can map them to their windows
    const WindowHandle wh = (WindowHandle)ctx->edi;

    const char* buf = (const char*)ctx->esi;
    const usize n = ctx->edx;
    for (usize i = 0; i < n; ++i) {
        putchar(wh, buf[i]);
    }
}

static void kernelInit() {
    setup8259();

    SyscallDescriptor syscalls[] = {(SyscallDescriptor){.vector = 0x30, .impl = writeImpl}};
    setupInterrupts(syscalls, sizeof(syscalls) / sizeof(SyscallDescriptor));

    setupWindows();
    initScreen();

    enableInterrupts;
}

static void printEsp() {
    u32 esp;
    __asm__ volatile("mov %0, esp" : "=r"(esp));
    printf(w, "0x%x\n", esp);
}

static unsigned i = 1;

static void disableKernelCodeSegment() {
    extern u64 kernelCsd;
    kernelCsd &= ~(((u64)1) << 47);  // kernel segment descriptor P = 0
}

static void printTrap(unsigned x) {
    if (x == 0) {
        printf(w, "EXPLODE\n");
        infiniteLoop();
    } else {
        printf(w, "%u ", x);
    }
}

static void timerHandler(const InterruptCtx* ctx) {
    // clearWindow(w);
}

static void userspaceProgram() {
    static const char msg[] = "HELLO SYSCALL ";
    static const char lf = '\n';
    for (;;) {
        for (int i = 0; i < 4; ++i) {
            write(w, msg, strlen(msg));
        }

        write(w, &lf, 1);
    }
    infiniteLoop();
}

[[noreturn]] void kernelEntry() {
    kernelInit();

    setMasterDeviceMask(DISABLE_ALL | ENABLE_TIMER);
    overrideIterruptHandler(TIMER_INTERRUPT_VECTOR, timerHandler);

    u8* stack = (u8*)mallocImmortal(4096, 16) + 4096;
    // printf(w, "%p\n", stack);
    startProcess(&userspaceProgram, stack);

    infiniteLoop();
}
