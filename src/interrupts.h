#pragma once

#include "types.h"

typedef struct InterruptCtx InterruptCtx;

typedef void (*InterruptHandler)(const InterruptCtx*);

typedef enum {
    Gate_Interrupt = 0b110,
} GateDescriptorType;

constexpr u8 DISABLE_ALL = 0;
constexpr u8 ENABLE_ALL = 0xFF;
constexpr u8 ENABLE_TIMER = 1;
constexpr u8 ENABLE_KEYBOARD = 2;

constexpr u8 MASTER_IRQ_START = 0x20;
constexpr u8 SLAVE_IRQ_START = 0x28;

constexpr u8 TIMER_INTERRUPT_VECTOR = MASTER_IRQ_START;
constexpr u8 KEYBOARD_INTERRUPT_VECTOR = MASTER_IRQ_START + 1;

void setupInterrupts();
void setup8259();

u8 getMasterDeviceMask();
u8 getSlaveDeviceMask();
void setMasterDeviceMask(u8 mask);
void setSlaveDeviceMask(u8 mask);

void universalHandler(const InterruptCtx* ctx);

void overrideIterruptHandler(u8 vector, InterruptHandler handler);
