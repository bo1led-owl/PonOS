#include "interrupts.h"
#include "paging.h"
#include "panic.h"
#include "syscalls.h"
#include "userspace.h"
#include "vga.h"

static void printImpl(const InterruptCtx* ctx) {
    const WindowHandle w = curWindowHandle();
    const usize n = ctx->edi;

    printf(w, "%d\n", n);
}

static int param = 1000;
static WindowHandle w;

extern void rec(int);

[[noreturn]] static void userspaceProgram() {
    rec(param);
    exit(param);
}

static Page* programStackLimit;

static void* initProgramVirtualAddrSpace() {
    disablePaging();

    PageTableEntry* pt = allocZeroedPage();
    assignPageDirectoryEntry(pd + 1, pt, false, true, true, true);

    programStackLimit = (Page*)0x800000;

    enablePaging();
    return (void*)0x800000;
}

[[noreturn]] static void startProgram() {
    void* stack = initProgramVirtualAddrSpace();
    startProcess(userspaceProgram, stack, w);
}

static void stopProcess() {
    printf(curWindowHandle(), "lowest available stack point: 0x%x\n", programStackLimit);
    disablePaging();

    PageTableEntry* pt = (PageTableEntry*)(pd[1].addr << 12);
    for (usize i = 0; i < 1024; ++i) {
        if (!pt[i].present) {
            continue;
        }

        freePage((void*)(pt[i].addr << (usize)12));
    }
    freePage(pt);

    pd[1].present = false;

    enablePaging();
}

static void exitImpl(const InterruptCtx* ctx) {
    const WindowHandle curW = curWindowHandle();
    const usize status = ctx->edi;

    printf(curW, "%d\n", status);

    stopProcess();

    param += 1;
    startProgram();
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
    } else if (accessedAddress >= 0x400000 && accessedAddress < (usize)programStackLimit) {
        disablePaging();

        PageTableEntry* pt = (PageTableEntry*)(pd[1].addr << 12);
        while ((usize)programStackLimit > accessedAddress) {
            programStackLimit -= 1;

            PageTableEntry* pte = pt + ((usize)programStackLimit - 0x400000) / PAGE_SIZE;
            assignPageTableEntry(pte, allocPage(), true, true, true);
        }

        enablePaging();
        return;
    } else if (accessedAddress >= 0x80000 && accessedAddress < 0x400000) {
        printf(curWindowHandle(), "SOE\n");
    } else {
        printf(curWindowHandle(), "UB! %x %x:%x\n", accessedAddress, ctx->cs, ctx->eip);
    }

    stopProcess();

    param += 1;
    startProgram();
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

    startProgram();
}
