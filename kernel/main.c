#include "interrupts.h"
#include "paging.h"
#include "panic.h"
#include "userspace.h"
#include "vga.h"

static void printImpl(InterruptCtx* ctx) {
    const WindowHandle w = curWindowHandle();
    const char c = ctx->eax;

    putchar(w, c);
}

#define LINE "\n-----------------\n"

static void resetColor(WindowHandle w) {
    setFgColor(w, Color_White);
    setBgColor(w, Color_Black);
}

static void exitImpl(InterruptCtx* ctx) {
    const WindowHandle curW = curWindowHandle();
    const usize status = ctx->eax;

    resetColor(curW);
    printf(curW, LINE);
    printf(curW, "process exited with status %d\n", status);

    stopCurProcess();
}

static void pageFaultHandler(InterruptCtx* ctx) {
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

    WindowHandle w = curWindowHandle();
    if (accessedAddress < 0x200000) {
        resetColor(w);
        printf(w, LINE "null pointer dereference\n");
    } else if (accessedAddress >= 0x200000 && accessedAddress < 0x400000) {
        resetColor(w);
        printf(w, LINE "stack overflow\n");
    } else if (accessedAddress >= 0x400000 && accessedAddress < (usize)getCurProcessStackLimit()) {
        disablePaging();
        Page* limit = getCurProcessStackLimit();
        PageDirectoryEntry* pd = curPageDirectory();

        PageTableEntry* pt = (PageTableEntry*)(pd[1].addr << 12);
        while ((usize)limit > accessedAddress) {
            limit -= 1;

            PageTableEntry* pte = pt + ((usize)limit - 0x400000) / PAGE_SIZE;
            assignPageTableEntry(pte, allocPage(), true, true, true);
        }

        setCurProcessStackLimit(limit);
        enablePaging();
        return;
    } else {
        resetColor(w);
        printf(w, LINE "unexpected error\n");
    }

    stopCurProcess();
}

static void initInterrupts() {
    setup8259();

    SyscallDescriptor syscalls[] = {
        (SyscallDescriptor){.vector = 0x30, .impl = exitImpl},
        (SyscallDescriptor){.vector = 0x31, .impl = printImpl},
    };

    setupInterrupts(syscalls, sizeof(syscalls) / sizeof(SyscallDescriptor));
    overrideIterruptHandler(14, pageFaultHandler);

    setMasterDeviceMask(ENABLE_TIMER);
    overrideIterruptHandler(TIMER_INTERRUPT_VECTOR, timerHandler);

    enableInterrupts;
}

[[noreturn]] void kernelEntry() {
    initInterrupts();

    WindowHandle w[4] = {
        addWindow(0, 0, 12, 40),
        addWindow(40, 0, 12, 40),
        addWindow(0, 12, 12, 40),
        addWindow(40, 12, 12, 40),
    };

    setFgColor(w[0], Color_LightRed);

    setBgColor(w[1], Color_White);
    setFgColor(w[1], Color_DarkGray);

    setFgColor(w[2], Color_Green);

    initScreen();

    addProcess(0x20000, w[0], 3, "MANKIND IS DEAD", "BLOOD IS FUEL", "HELL IS FULL");
    for (usize i = 1; i < 4; ++i) {
        addProcess(0x20000 + 0x10000 * i, w[i], 0);
    }

    initPaging();
    startProcesses();
}
