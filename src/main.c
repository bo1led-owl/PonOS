#include "interrupts.h"
#include "paging.h"
#include "syscalls.h"
#include "userspace.h"
#include "utils.h"
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

    switch (param % 4) {
        case 0:
            exit(param);
        case 1:
            __asm__ volatile("mov eax, [0x42]" ::: "eax");
            UNREACHABLE;
        case 2:
            infiniteRecursion();
        case 3:
            __asm__ volatile("mov eax, [0x900000]" ::: "eax");
            UNREACHABLE;
    }
}

static void stopProcess() {
    disablePaging();

    PageTableEntry* pt = (PageTableEntry*)(pdt[1].addr << 12);
    for (usize i = 0; i < 1024; ++i) {
        freePage((void*)(pt[i].addr << (usize)12));
    }
    freePage(pt);

    pdt[1].present = false;

    enablePaging();
}

static void exitImpl(const InterruptCtx* ctx) {
    const WindowHandle curW = curWindowHandle();
    const usize status = ctx->edi;

    printf(curW, "%d\n", status);

    stopProcess();

    param += 1;
    startProcess(userspaceProgram, w);
}

static void pageFaultHandler(const InterruptCtx* ctx) {
    usize accessedAddress;
    __asm__ volatile("mov %0, cr2" : "=r"(accessedAddress));

    bool fromUserspace = (ctx->errorCode >> 2) & 1;
    bool writeAccess = (ctx->errorCode >> 1) & 1;

    if (!fromUserspace) {
        panic(
            "kernel caused a page fault with a %s access\n\n"
            "cs:eip = 0x%x:0x%x\n"
            "acessed address: 0x%x\n",
            writeAccess ? "write" : "read",
            ctx->cs,
            ctx->eip,
            accessedAddress);
    }

    if (accessedAddress < 0x7000) {
        printf(curWindowHandle(), "NPE\n");
    } else if (accessedAddress >= 0x80000 && accessedAddress < 0x400000) {
        printf(curWindowHandle(), "SOE\n");
    } else {
        printf(curWindowHandle(), "UB!\n");
    }

    stopProcess();

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
    overrideIterruptHandler(14, pageFaultHandler);

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
