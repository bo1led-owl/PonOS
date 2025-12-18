#pragma once

#include "types.h"

constexpr usize KiB = 1024;
constexpr usize MiB = 1024 * KiB;
constexpr usize GiB = 1024 * MiB;

constexpr usize PAGE_SIZE = 4 * KiB;
constexpr usize HUGE_PAGE_SIZE = 4 * MiB;

typedef u8 Page[PAGE_SIZE];
typedef u8 HugePage[HUGE_PAGE_SIZE];

typedef struct {
    bool present : 1;
    bool writeAllowed : 1;
    bool accessibleInUserspace : 1;
    usize reserved : 9;
    usize addr : 20;
} PageTableEntry;

typedef struct {
    bool present : 1;
    bool writeAllowed : 1;
    bool accessibleInUserspace : 1;
    usize zeros : 4;
    bool hugePage : 1;
    bool zero : 1;
    usize ones : 3;
    usize addr : 20;
} PageDirectoryEntry;

static_assert(sizeof(Page) == 4 * KiB);
static_assert(sizeof(HugePage) == 4 * MiB);
static_assert(sizeof(PageTableEntry) == 4);
static_assert(sizeof(PageDirectoryEntry) == 4);

extern PageDirectoryEntry* pdt;

void* allocPage();
void* allocZeroedPage();
void freePage(void* p);

void assignPageTableEntry(PageTableEntry* pte,
                          void* addr,
                          bool accessibleInUserspace,
                          bool writeAllowed,
                          bool present);

void assignPageDirectoryEntry(PageDirectoryEntry* pde,
                              void* addr,
                              bool hugePage,
                              bool accessibleInUserspace,
                              bool writeAllowed,
                              bool present);

extern void initPaging();
extern void enablePaging();
extern void disablePaging();
