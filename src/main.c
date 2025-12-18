#include "interrupts.h"
#include "paging.h"
#include "syscalls.h"
#include "userspace.h"
#include "vga.h"

static void printImpl(const InterruptCtx* ctx) {
    const WindowHandle w = curWindowHandle();
    const usize n = ctx->edi;

    printf(w, "%d\n", n);
}

static int param = 0;
static WindowHandle w;

static void userspaceProgram() {
    // for (unsigned i = 0;; ++i) {
    //     printInt(i);
    // }

    // infiniteLoop();
    exit(param);
}

static void exitImpl(const InterruptCtx* ctx) {
    const WindowHandle curW = curWindowHandle();
    const usize status = ctx->edi;

    printf(curW, "%d\n", status);

    disablePaging();

    PageTableEntry* pt = (PageTableEntry*)(pdt[1].addr << 12);
    for (usize i = 0; i < 1024; ++i) {
        freePage((void*)(pt[i].addr << (usize)12));
    }
    freePage(pt);

    pdt[1].present = false;

    enablePaging();

    param += 1;
    startProcess(userspaceProgram, w);
}

static void initInterrupts() {
    setup8259();

    SyscallDescriptor syscalls[] = {
        (SyscallDescriptor){.vector = 0x30, .impl = printImpl},
        (SyscallDescriptor){.vector = 0x31, .impl = exitImpl},
    };
    setupInterrupts(syscalls, sizeof(syscalls) / sizeof(SyscallDescriptor));

    enableInterrupts;
}

[[noreturn]] void kernelEntry() {
    initPaging();
    enablePaging();

    initInterrupts();

    w = addWindow(0, 0, VGA_ROWS, VGA_COLUMNS);
    initScreen();

    startProcess(userspaceProgram, w);
}
