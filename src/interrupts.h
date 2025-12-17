#pragma once

#include "types.h"

typedef struct {
    u32 edi, esi, ebp, esp, ebx, edx, ecx, eax;
    alignas(4) u16 gs, fs, es, ds;
    alignas(4) u8 vector;
    u32 errorCode;
    u32 eip;
    alignas(4) u16 cs;
    u32 eflags;
} InterruptCtx;

static_assert(sizeof(InterruptCtx) == 17 * sizeof(u32));

typedef void (*InterruptHandler)(const InterruptCtx*);

typedef struct {
    u8 vector;
    InterruptHandler impl;
} SyscallDescriptor;

constexpr u8 DISABLE_ALL = 0;
constexpr u8 ENABLE_ALL = (u8)~0;
constexpr u8 ENABLE_TIMER = 1;
constexpr u8 ENABLE_KEYBOARD = 2;

constexpr u8 MASTER_IRQ_START = 0x20;
constexpr u8 SLAVE_IRQ_START = 0x28;

constexpr u8 TIMER_INTERRUPT_VECTOR = MASTER_IRQ_START;
constexpr u8 KEYBOARD_INTERRUPT_VECTOR = MASTER_IRQ_START + 1;

void setupInterrupts(SyscallDescriptor* syscalls, usize nSyscalls);
void setup8259();

u8 getMasterDeviceMask();
u8 getSlaveDeviceMask();
void setMasterDeviceMask(u8 mask);
void setSlaveDeviceMask(u8 mask);

void universalHandler(const InterruptCtx* ctx);

void overrideIterruptHandler(u8 vector, InterruptHandler handler);

[[noreturn]] extern void restoreCtx(void* ctx);

#define disableInterrupts __asm__ volatile("cli")
#define enableInterrupts __asm__ volatile("sti")
