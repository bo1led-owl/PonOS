#include "userspace.h"

#include <alloca.h>

#include "interrupts.h"
#include "types.h"
#include "utils.h"

typedef struct {
    InterruptCtx ctx;
    u32 esp;
    alignas(4) u16 ss;
} FakeCtx;

constexpr u32 DISABLE_IOPL = ~((u32)0b11 << 12);
constexpr u32 ENABLE_IF = 1 << 9;
constexpr u32 CPL3 = 0b11;
constexpr u32 RPL3 = 0b11;

[[noreturn]] void startProcess(void (*entry)(), void* stack) {
    FakeCtx ctx = (FakeCtx){
        .ctx =
            (InterruptCtx){
                .cs = APP_CODE_SEGMENT | CPL3,
                .ds = APP_DATA_SEGMENT | RPL3,
                .es = APP_DATA_SEGMENT | RPL3,
                .fs = APP_DATA_SEGMENT | RPL3,
                .gs = APP_DATA_SEGMENT | RPL3,
                .eip = (u32)entry,
                .eflags = (getEflags() & DISABLE_IOPL) | ENABLE_IF,
            },
        .ss = APP_DATA_SEGMENT | CPL3,
        .esp = (usize)stack,
    };
    restoreCtx(&ctx);
    UNREACHABLE;
}
