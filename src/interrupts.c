#include "interrupts.h"

#include "alloc.h"
#include "assert.h"
#include "panic.h"

void collectCtx();

typedef enum {
    Gate_Interrupt = 0b110,
    Gate_Trap = 0b111,
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

typedef struct InterruptCtx {
    u32 edi, esi, ebp, esp, ebx, edx, ecx, eax;
    alignas(4) u16 gs, fs, es, ds;
    alignas(4) u8 vector;
    u32 errorCode;
    u32 eip;
    alignas(4) u16 cs;
    u32 eflags;
} InterruptCtx;

static_assert(sizeof(IdtEntry) == 8);
static_assert(sizeof(InterruptCtx) == 17 * sizeof(u32));

constexpr usize TRAMPOLINE_SIZE = 8;
constexpr usize N_VECTORS = 256;

static bool hasErrorCode(u8 vector) {
    static constexpr u8 INTERRUPTS_WITH_ERROR_CODE[8] = {0x8, 0xA, 0xB, 0xC, 0xD, 0xE, 0x11, 0x15};
    for (usize i = 0; i < 8; ++i) {
        if (INTERRUPTS_WITH_ERROR_CODE[i] == vector) {
            return true;
        }
    }
    return false;
}

static void* genTrampolines() {
    u8* trampolines = mallocImmortal(TRAMPOLINE_SIZE * N_VECTORS, 8);

    for (u16 vector = 0; vector < N_VECTORS; ++vector) {
        u8* trampoline = trampolines + vector * TRAMPOLINE_SIZE;
        usize offset = 0;
        if (!hasErrorCode(vector)) {
            trampoline[offset++] = 0x50;  // push eax
        }
        // push $vector
        trampoline[offset++] = 0x6A;
        trampoline[offset++] = vector;
        // jmp collectCtx
        // +5 to account for the `jmp` size
        u32 offsetToCollectCtx = (u32)collectCtx - (u32)(trampoline + offset + 5);
        trampoline[offset++] = 0xE9;
        *(u32*)(trampoline + offset) = offsetToCollectCtx;
        offset += 4;
        assert(offset <= TRAMPOLINE_SIZE);
    }

    return trampolines;
}

static void* genIdtEntry(IdtEntry* entry, const void* trampoline) {
    entry->fixed1 = 0;
    entry->fixed2 = 0b01;
    entry->offsetLow = ((usize)trampoline) & 0xFFFF;
    entry->offsetHi = (((usize)trampoline) >> 16) & 0xFFFF;
    entry->dpl = 0;
    entry->present = 1;
    entry->segmentSelector = 8;  // CODE_SEGMENT
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

void setupInterrupts() {
    void* trampolines = genTrampolines();
    void* idt = genIdt(trampolines);
    constexpr u16 idtLimit = N_VECTORS * sizeof(IdtEntry) - 1;
    u64 idtDesc = ((u64)idt << 16) | idtLimit;
    __asm__ volatile("lidt [%0]" ::"r"(&idtDesc));
}

void universalHandler(const InterruptCtx* ctx) {
    panic(
        "unhandled interrupt 0x%x at 0x%x:0x%x\n\n"
        "registers:\n"
        "  eax: 0x%x\n"
        "  ecx: 0x%x\n"
        "  edx: 0x%x\n"
        "  ebx: 0x%x\n"
        "  esp: 0x%x\n"
        "  ebp: 0x%x\n"
        "  esi: 0x%x\n"
        "  edi: 0x%x\n"
        "  ds: 0x%x\n"
        "  es: 0x%x\n"
        "  fs: 0x%x\n"
        "  gs: 0x%x\n"
        "error code: 0x%x\n\n"
        "eflags: 0x%x",
        ctx->vector, ctx->cs, ctx->eip, ctx->eax, ctx->ecx, ctx->edx, ctx->ebx, ctx->esp, ctx->ebp,
        ctx->esi, ctx->edi, ctx->ds, ctx->es, ctx->fs, ctx->gs, ctx->errorCode, ctx->eflags);
}
