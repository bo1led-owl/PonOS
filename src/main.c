#include "alloc.h"
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

static void kernelInit() {
    setup8259();

    SyscallDescriptor syscalls[] = {(SyscallDescriptor){.vector = 0x30, .impl = printImpl}};
    setupInterrupts(syscalls, sizeof(syscalls) / sizeof(SyscallDescriptor));

    enableInterrupts;

    PageDirectoryEntry* pdt = allocZeroedPage();
    PageTableEntry* pt = allocPage();

    assignPageDirectoryEntry(pdt, pt, false, true, true, true);

    for (usize i = 0; i < 1024; ++i) {
        usize frame = i * 4 * KiB;
        bool isVgaMem = frame >= 0x80000 && frame < 0x100000;
        assignPageTableEntry(pt + i, (void*)frame, !isVgaMem, true, true);
    }

    setupPaging(pdt);
    enablePaging();
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

    WindowHandle w = addWindow(0, 0, VGA_ROWS, VGA_COLUMNS);
    initScreen();

    startProcess(userspaceProgram, stack, w);
}
