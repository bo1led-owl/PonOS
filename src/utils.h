#pragma once

#include "assert.h"
#include "types.h"

[[noreturn]] extern void restoreCtx(void* ctx);
extern u32 getEflags();

[[noreturn]] void infiniteLoop();

#define disableInterrupts __asm__ volatile("cli")
#define enableInterrupts __asm__ volatile("sti")
#define UNREACHABLE assert(false && "unreachable code reached")
