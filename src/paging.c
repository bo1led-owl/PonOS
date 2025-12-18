#include "paging.h"

#include "assert.h"
#include "mem.h"
#include "panic.h"
#include "types.h"

static constexpr usize ARENA_START = 0x400000;
static constexpr usize ARENA_END = ARENA_START + RAM_SIZE;

static void** freeList = nullptr;
static Page* cur = (Page*)ARENA_START;

PageDirectoryEntry* pdt = nullptr;

void* allocPage() {
    if (freeList) {
        void* res = freeList;
        freeList = *freeList;
        return res;
    }

    if ((usize)cur >= ARENA_END - PAGE_SIZE) {
        panic("not enough memory to allocate a page");
    }

    void* res = cur;
    cur += 1;
    return res;
}

void* allocZeroedPage() {
    void* p = allocPage();
    if (p) {
        memzero(p, sizeof(Page));
    }
    return p;
}

void freePage(void* p) {
    *((void**)p) = freeList;
    freeList = p;
}

void assignPageTableEntry(PageTableEntry* pte,
                          void* addr,
                          bool accessibleInUserspace,
                          bool writeAllowed,
                          bool present) {
    assert(((usize)addr & 0x111) == 0);
    *pte = (PageTableEntry){
        .accessibleInUserspace = accessibleInUserspace,
        .addr = (usize)addr >> 12,
        .reserved = 0b111000000,
        .writeAllowed = writeAllowed,
        .present = present,
    };
}

void assignPageDirectoryEntry(PageDirectoryEntry* pde,
                              void* addr,
                              bool hugePage,
                              bool accessibleInUserspace,
                              bool writeAllowed,
                              bool present) {
    assert(pdt);
    assert(((usize)addr & 0x111) == 0);

    *pde = (PageDirectoryEntry){
        .accessibleInUserspace = accessibleInUserspace,
        .hugePage = hugePage,
        .addr = (usize)addr >> 12,
        .ones = 0b111,
        .writeAllowed = writeAllowed,
        .present = present,
    };
}

extern void setupPagingControlRegisters(PageDirectoryEntry* pdt);

void initPaging() {
    pdt = allocZeroedPage();

    PageTableEntry* pt = allocPage();

    assignPageDirectoryEntry(pdt, pt, false, true, true, true);

    for (usize i = 0; i < 1024; ++i) {
        usize frame = i * 4 * KiB;
        bool isVgaMem = frame >= 0x80000 && frame < 0x100000;
        assignPageTableEntry(pt + i, (void*)frame, !isVgaMem, true, true);
    }

    setupPagingControlRegisters(pdt);
}
