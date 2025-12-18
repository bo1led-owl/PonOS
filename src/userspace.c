#include "userspace.h"

#include <alloca.h>

#include "interrupts.h"
#include "paging.h"
#include "types.h"

typedef struct {
    InterruptCtx ctx;
    u32 esp;
    alignas(4) u16 ss;
} FakeCtx;

static constexpr u32 IOPL_0 = ~((u32)0b11 << 12);
static constexpr u32 ENABLE_IF = 1 << 9;
static constexpr u32 CPL3 = 0b11;
static constexpr u32 RPL3 = 0b11;

static WindowHandle windowHandle;

static __attribute__((naked)) u32 getEflags() {
    __asm__ volatile(
        "pushfd\n"
        "pop eax\n"
        "ret\n");
}

static void* initVirtualAddrSpace() {
    disablePaging();

    PageTableEntry* pt = allocPage();
    assignPageDirectoryEntry(pdt + 1, pt, false, true, true, true);

    for (usize i = 0; i < 1024; ++i) {
        assignPageTableEntry(pt + i, allocPage(), true, true, true);
    }

    enablePaging();

    return (void*)(0x800000);
}

[[noreturn]] void startProcess(void (*entry)(), WindowHandle w) {
    void* stack = initVirtualAddrSpace();

    windowHandle = w;
    FakeCtx ctx = (FakeCtx){
        .ctx =
            (InterruptCtx){
                .cs = APP_CODE_SEGMENT | CPL3,
                .ds = APP_DATA_SEGMENT | RPL3,
                .es = APP_DATA_SEGMENT | RPL3,
                .fs = APP_DATA_SEGMENT | RPL3,
                .gs = APP_DATA_SEGMENT | RPL3,
                .eip = (u32)entry,
                .eflags = (getEflags() & IOPL_0) | ENABLE_IF,
            },
        .ss = APP_DATA_SEGMENT | CPL3,
        .esp = (usize)stack,
    };
    restoreCtx(&ctx);
}

WindowHandle curWindowHandle() {
    return windowHandle;
}
