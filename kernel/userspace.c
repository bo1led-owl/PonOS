#include "userspace.h"

#include <alloca.h>

#include "interrupts.h"
#include "mem.h"
#include "types.h"
#include "utils.h"

typedef struct {
    InterruptCtx ctx;
    u32 esp;
    alignas(4) u16 ss;
} FakeCtx;

static constexpr u32 IOPL_0 = ~((u32)0b11 << 12);
static constexpr u32 ENABLE_IF = 1 << 9;
static constexpr u32 CPL3 = 0b11;
static constexpr u32 RPL3 = 0b11;

typedef struct {
    WindowHandle w;
    bool stopped;
    Page* stackLimit;
    PageDirectoryEntry* pageDirectory;
    FakeCtx ctx;
} ProcessState;

static ProcessState processes[4];
ProcessState* curProcess = processes;

static ProcessState* nextRunningProcess() {
    usize cur = curProcess - processes;
    for (usize i = 1; i < 4; ++i) {
        ProcessState* p = processes + (cur + i) % 4;
        if (!p->stopped) {
            return (curProcess = p);
        }
    }

    return (curProcess->stopped) ? nullptr : curProcess;
}

WindowHandle curWindowHandle() {
    return curProcess->w;
}

PageDirectoryEntry* curPageDirectory() {
    return curProcess->pageDirectory;
}

Page* getCurProcessStackLimit() {
    return curProcess->stackLimit;
}

void setCurProcessStackLimit(Page* limit) {
    curProcess->stackLimit = limit;
}

[[noreturn]] void stopCurProcess() {
    curProcess->stopped = true;

    disablePaging();

    PageDirectoryEntry* pd = curPageDirectory();

    PageTableEntry* pt = (PageTableEntry*)(pd[1].addr << 12);
    for (usize i = 0; i < 1024; ++i) {
        if (!pt[i].present) {
            continue;
        }

        freePage((void*)(pt[i].addr << 12));
    }
    freePage(pt);

    pt = (PageTableEntry*)(pd[2].addr << 12);
    for (usize i = 16; i < 1024; ++i) {
        if (!pt[i].present) {
            continue;
        }
        freePage((void*)(pt[i].addr << 12));
    }
    freePage(pt);
    freePage(pd);

    curProcess = nextRunningProcess();
    if (!curProcess) {
        infiniteLoop();
    }

    setPageDirectory(curProcess->pageDirectory);
    enablePaging();
    restoreCtx(&curProcess->ctx);
}

static __attribute__((naked)) u32 getEflags() {
    __asm__ volatile(
        "pushfd\n"
        "pop eax\n"
        "ret\n");
}

static void prepareCtx(void* ctx, void (*entry)(), void* stack) {
    *(FakeCtx*)ctx = (FakeCtx){
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
}

static usize strlen(char* s) {
    if (*s == 0) {
        return 0;
    }
    return 1 + strlen(s + 1);
}

static PageDirectoryEntry* initProcessVirtualAddrSpace(usize package, int argc, va_list argv) {
    PageDirectoryEntry* pd = allocZeroedPage();

    assignPageDirectoryEntry(pd, 0, true, false, true, true);

    PageTableEntry* pt = allocZeroedPage();
    assignPageDirectoryEntry(pd + 1, pt, false, true, true, true);

    pt = allocZeroedPage();
    assignPageDirectoryEntry(pd + 2, pt, false, true, true, true);
    for (usize i = 0; i < 16; ++i) {
        assignPageTableEntry(pt + i, (void*)(package + PAGE_SIZE * i), true, true, true);
    }

    char** argvPage = allocPage();
    assignPageTableEntry(pt + 16, argvPage, true, false, true);

    for (usize i = 0; i < (usize)argc; ++i) {
        char* argPage = allocPage();
        assignPageTableEntry(pt + i + 17, argPage, true, false, true);

        char* arg = va_arg(argv, char*);
        memcpy(argPage, arg, strlen(arg) + 1);
    }

    for (int i = 0; i < argc; ++i) {
        argvPage[i] = (char*)(0x811000 + i * PAGE_SIZE);
    }
    argvPage[argc] = nullptr;
    return pd;
}

void addProcess(usize package, WindowHandle w, int argc, ...) {
    assert(curProcess < processes + 4);

    va_list argv;
    va_start(argv);
    *curProcess = (ProcessState){
        .stopped = false,
        .w = w,
        .stackLimit = (Page*)0x800000,
        .pageDirectory = initProcessVirtualAddrSpace(package, argc, argv),
    };
    prepareCtx(&curProcess->ctx, (void (*)(void))0x800000, (void*)0x800000);
    curProcess->ctx.ctx.eax = argc;
    curProcess->ctx.ctx.ecx = 0x810000;

    va_end(argv);

    curProcess += 1;
}

[[noreturn]] void startProcesses() {
    curProcess = processes;

    setPageDirectory(curProcess->pageDirectory);
    enablePaging();
    restoreCtx(&curProcess->ctx);
}

void timerHandler(InterruptCtx* ctx) {
    memcpy(&curProcess->ctx, ctx, sizeof(FakeCtx));

    curProcess = nextRunningProcess();

    setPageDirectory(curProcess->pageDirectory);
    memcpy(ctx, &curProcess->ctx, sizeof(FakeCtx));
}
