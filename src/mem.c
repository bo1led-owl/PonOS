#include "mem.h"

void* memcpy(void* restrict dest, const void* restrict src, usize n) {
    return copyForwards(dest, src, n);
}

void* copyForwards(void* dest, const void* src, usize n) {
    for (usize i = 0; i < n; ++i) {
        *((u8*)dest + i) = *((u8*)src + i);
    }
    return dest;
}

void* copyBackwards(void* dest, const void* src, usize n) {
    for (usize i = 0; i < n; ++i) {
        *((u8*)dest + n - i - 1) = *((u8*)src + n - i - 1);
    }
    return dest;
}

void* memmove(void* dest, const void* src, usize n) {
    if ((usize)dest + n > (usize)src) {
        copyForwards(dest, src, n);
    } else {
        copyBackwards(dest, src, n);
    }

    return dest;
}

void* memzero(void* dest, usize n) {
    for (usize i = 0; i < n; ++i) {
        *((u8*)dest + i) = 0;
    }

    return dest;
}
