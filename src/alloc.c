#include "alloc.h"

#include "mem.h"
#include "panic.h"

constexpr usize ARENA_END = 0x80000;

static u8* cur = (u8*)(0x7C00 + (KERNEL_SIZE_KB) * 1024);

void* mallocImmortal(usize size, usize alignment) {
    if (size == 0) {
        return nullptr;
    }
    if (alignment == 0) {
        alignment = 1;
    }

    if ((usize)cur % alignment != 0) {
        cur += (alignment - (usize)cur % alignment);
    }

    if ((usize)cur + size > ARENA_END) {
        panic("failed to allocate %u bytes: arena overflow", size);
    }

    void* res = cur;
    cur += size;
    return res;
}

void* callocImmortal(usize size, usize alignment) {
    void* res = mallocImmortal(size, alignment);
    if (res == nullptr) {
        return nullptr;
    }
    memzero(res, size);
    return res;
}
