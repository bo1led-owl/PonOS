#include "interrupts.h"

#include "alloc.h"
#include "assert.h"
#include "hardwareIo.h"
#include "mem.h"
#include "panic.h"

extern void collectCtx();

typedef enum {
    Gate_Interrupt = 0b110,
} GateDescriptorType;

typedef struct {
    u16 offsetLow;
    u16 segmentSelector;
    u8 fixed1;  // zeros and reserved (also zeros)
    GateDescriptorType type : 3;
    u8 fixed2 : 2;  // fixed `0b01`
    u8 dpl : 2;
    u8 present : 1;
    u16 offsetHi;
} IdtEntry;

static_assert(sizeof(IdtEntry) == 8);

static constexpr usize TRAMPOLINE_SIZE = 8;
static constexpr usize N_VECTORS = 256;

static bool hasErrorCode(u8 vector) {
    switch (vector) {
        case 0x8:
        case 0xA:
        case 0xB:
        case 0xC:
        case 0xD:
        case 0xE:
        case 0x11:
        case 0x15:
        case 0x1D:
        case 0x1E:
            return true;
        default:
            return false;
    }
}

static void writeTrampoline(u8 vector, u8* buffer) {
    usize offset = 0;
    if (!hasErrorCode(vector)) {
        buffer[offset++] = 0x50;  // push eax
    }
    // push $vector
    buffer[offset++] = 0x6A;
    buffer[offset++] = vector;
    // jmp collectCtx
    // +5 to account for the `jmp` size
    u32 offsetToCollectCtx = (u32)collectCtx - (u32)(buffer + offset + 5);
    buffer[offset++] = 0xE9;
    *(u32*)(buffer + offset) = offsetToCollectCtx;
    offset += 4;
    assert(offset <= TRAMPOLINE_SIZE);
}

static void* genTrampolines() {
    u8* trampolines = mallocImmortal(TRAMPOLINE_SIZE * N_VECTORS, 8);

    for (u16 vector = 0; vector < N_VECTORS; ++vector) {
        u8* trampoline = trampolines + vector * TRAMPOLINE_SIZE;
        writeTrampoline(vector, trampoline);
    }

    return trampolines;
}

static void* genIdtEntry(IdtEntry* entry, const InterruptHandler* trampoline) {
    entry->fixed1 = 0;
    entry->fixed2 = 0b01;
    entry->offsetLow = ((usize)trampoline) & 0xFFFF;
    entry->offsetHi = (((usize)trampoline) >> 16) & 0xFFFF;
    entry->dpl = 0;
    entry->present = 1;
    entry->segmentSelector = KERNEL_CODE_SEGMENT;
    entry->type = Gate_Interrupt;
    return entry;
}

static void* genIdt(const u8* trampolines) {
    IdtEntry* idtBase = mallocImmortal(sizeof(IdtEntry) * N_VECTORS, sizeof(IdtEntry));

    for (u32 vector = 0; vector < N_VECTORS; ++vector) {
        const void* trampoline = trampolines + TRAMPOLINE_SIZE * vector;
        genIdtEntry(idtBase + vector, trampoline);
    }

    return idtBase;
}

static constexpr u16 MASTER_COMMAND_PORT = 0x20;
static constexpr u16 MASTER_DATA_PORT = 0x21;

static constexpr u16 SLAVE_COMMAND_PORT = 0xA0;
static constexpr u16 SLAVE_DATA_PORT = 0xA1;

u8 getMasterDeviceMask() {
    return ~readFromPort(MASTER_DATA_PORT);
}

u8 getSlaveDeviceMask() {
    return ~readFromPort(SLAVE_DATA_PORT);
}

void setMasterDeviceMask(u8 mask) {
    writeToPort(MASTER_DATA_PORT, ~mask);
}

void setSlaveDeviceMask(u8 mask) {
    writeToPort(SLAVE_DATA_PORT, ~mask);
}

void setup8259() {
    setMasterDeviceMask(DISABLE_ALL);
    setSlaveDeviceMask(DISABLE_ALL);

    constexpr u8 icw1 = 0b10001;
    writeToPort(MASTER_COMMAND_PORT, icw1);
    writeToPort(SLAVE_COMMAND_PORT, icw1);

    // icw2: starting vector for IRQ mapping
    writeToPort(MASTER_DATA_PORT, MASTER_IRQ_START);
    writeToPort(SLAVE_DATA_PORT, SLAVE_IRQ_START);

    // icw3: cascade configuration
    writeToPort(MASTER_DATA_PORT, 0b100);  // mask for slave 8259s
    writeToPort(SLAVE_DATA_PORT, 2);       // pin that slave is connected to

    constexpr u8 icw4 = 0b11;  // enable automatic EOI and 8259 mode
    writeToPort(MASTER_DATA_PORT, icw4);
    writeToPort(SLAVE_DATA_PORT, icw4);

    // because these get overwritten during configuration somehow
    setMasterDeviceMask(DISABLE_ALL);
    setSlaveDeviceMask(DISABLE_ALL);
}

static InterruptHandler handlerTable[N_VECTORS];
static IdtEntry* idt;

void setupInterrupts(SyscallDescriptor* syscalls, usize nSyscalls) {
    memzero(handlerTable, sizeof(handlerTable));

    void* trampolines = genTrampolines();
    idt = genIdt(trampolines);

    for (usize i = 0; i < nSyscalls; ++i) {
        SyscallDescriptor desc = syscalls[i];
        overrideIterruptHandler(desc.vector, desc.impl);
        idt[desc.vector].dpl = 3;
    }

    constexpr u16 idtLimit = N_VECTORS * sizeof(IdtEntry) - 1;
    u64 idtDesc = ((u64)idt << 16) | idtLimit;
    __asm__ volatile("lidt [%0]" ::"r"(&idtDesc));
}

void overrideIterruptHandler(u8 vector, InterruptHandler handler) {
    handlerTable[vector] = handler;
}

static const char* interruptName(u8 vector) {
    switch (vector) {
        case 0x0:
            return "#DE";
        case 0x1:
            return "#DB";
        case 0x2:
            return "NMI";
        case 0x3:
            return "#BP";
        case 0x4:
            return "#OF";
        case 0x5:
            return "#BR";
        case 0x6:
            return "#UD";
        case 0x7:
            return "#NM";
        case 0x8:
            return "#DF";
        case 0xA:
            return "#TS";
        case 0xB:
            return "#NP";
        case 0xC:
            return "#SS";
        case 0xD:
            return "#GP";
        case 0xE:
            return "#PF";
        case 0x10:
            return "#MF";
        case 0x11:
            return "#AC";
        case 0x12:
            return "#MC";
        case 0x13:
            return "#XM";
        case 0x14:
            return "#VE";
        case 0x15:
            return "#CP";
        default:
            return "N/A";
    }
}

void universalHandler(const InterruptCtx* ctx) {
    InterruptHandler handler = handlerTable[ctx->vector];
    if (handler) {
        handler(ctx);
        return;
    }

#define MSG_WITHOUT_ERROR_CODE                                                      \
    "unhandled interrupt 0x%x (%s) at 0x%x:0x%x\n\n"                                \
    "registers:\n"                                                                  \
    "  eax: 0x%x\n"                                                                 \
    "  ecx: 0x%x\n"                                                                 \
    "  edx: 0x%x\n"                                                                 \
    "  ebx: 0x%x\n"                                                                 \
    "  esp: 0x%x\n"                                                                 \
    "  ebp: 0x%x\n"                                                                 \
    "  esi: 0x%x\n"                                                                 \
    "  edi: 0x%x\n"                                                                 \
    "  ds:  0x%x\n"                                                                 \
    "  es:  0x%x\n"                                                                 \
    "  fs:  0x%x\n"                                                                 \
    "  gs:  0x%x\n"                                                                 \
    "  eflags: 0b%b (PF=%u,   AF=%u, ZF=%u, SF=%u, TF=%u, IF=%u,  DF=%u,  OF=%u,\n" \
    "               IOPL=%u, NT=%u, RF=%u, VM=%u, AC=%u, VIF=%u, VIP=%u, ID=%u)\n\n"

#define EXTRACT_BIT(n) ((ctx->eflags >> n) & 1)
#define CF EXTRACT_BIT(0)
#define PF EXTRACT_BIT(2)
#define AF EXTRACT_BIT(4)
#define ZF EXTRACT_BIT(6)
#define SF EXTRACT_BIT(7)
#define TF EXTRACT_BIT(8)
#define IF EXTRACT_BIT(9)
#define DF EXTRACT_BIT(10)
#define OF EXTRACT_BIT(11)
#define IOPL ((ctx->eflags >> 12) & 0b11)
#define NT EXTRACT_BIT(14)
#define RF EXTRACT_BIT(16)
#define VM EXTRACT_BIT(17)
#define AC EXTRACT_BIT(18)
#define VIF EXTRACT_BIT(19)
#define VIP EXTRACT_BIT(20)
#define ID EXTRACT_BIT(21)

#define CTX                                                                                       \
    ctx->vector, interruptName(ctx->vector), ctx->cs, ctx->eip, ctx->eax, ctx->ecx, ctx->edx,     \
        ctx->ebx, ctx->esp, ctx->ebp, ctx->esi, ctx->edi, ctx->ds, ctx->es, ctx->fs, ctx->gs, CF, \
        PF, AF, ZF, SF, TF, IF, DF, OF, IOPL, NT, RF, VM, AC, VIF, VIP, ID

    if (ctx->vector == 14) {  // page fault
        usize cr2;
        __asm__ volatile("mov %0, cr2" : "=r"(cr2) : : "eax");
        panic(MSG_WITHOUT_ERROR_CODE "error code: 0x%x\naccessed address: 0x%x", CTX, ctx->errorCode, cr2);
    } else if (hasErrorCode(ctx->vector)) {
        panic(MSG_WITHOUT_ERROR_CODE "error code: 0x%x", CTX, ctx->errorCode);
    } else {
        panic(MSG_WITHOUT_ERROR_CODE, CTX);
    }
}
