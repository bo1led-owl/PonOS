#include "mem.h"

void* memcpy(void* restrict dest, const void* restrict src, usize n) {
    const u8* src_bytes = src;
    u8* dest_bytes = dest;

    usize i = 0;

    for (; i + 4 < n; i += 4) {
        *(u32*)(dest_bytes + i) = *(u32*)(src_bytes + i);
    }
    for (; i + 2 < n; i += 2) {
        *(u16*)(dest_bytes + i) = *(u16*)(src_bytes + i);
    }
    if (i < n) {
        *(dest_bytes + i) = *(src_bytes + i);
    }

    return dest;
}

void* memmove(void* dest, const void* src, usize n) {
    const u8* src_bytes = src;
    u8* dest_bytes = dest;

    if (dest_bytes + n < src_bytes || src_bytes + n < dest_bytes) {
        return memcpy(dest, src, n);
    }

    usize i = 0;
    if (dest_bytes + n > src_bytes) {
        // dest: xxxxxxxxxx
        //  src:       xxxxxxxxxx
        // copy from left to right

        for (; i + 4 < n; i += 4) {
            *(u32*)(dest_bytes + i) = *(u32*)(src_bytes + i);
        }
        for (; i + 2 < n; i += 2) {
            *(u16*)(dest_bytes + i) = *(u16*)(src_bytes + i);
        }
        if (i < n) {
            *(dest_bytes + i) = *(src_bytes + i);
        }
    } else {
        // dest:       xxxxxxxxxx
        //  src: xxxxxxxxxx
        // copy from right to left

        dest_bytes += n - 1;
        src_bytes += n - 1;
        for (; i + 4 < n; i += 4) {
            *(u32*)(dest_bytes - i) = *(u32*)(src_bytes - i);
        }
        for (; i + 2 < n; i += 2) {
            *(u16*)(dest_bytes - i) = *(u16*)(src_bytes - i);
        }
        if (i < n) {
            *(dest_bytes - i) = *(src_bytes - i);
        }
    }

    return dest;
}

void* memzero(void* dest, usize n) {
    u8* dest_bytes = dest;

    usize i = 0;

    for (; i + 4 < n; i += 4) {
        *(u32*)(dest_bytes + i) = 0;
    }
    for (; i + 2 < n; i += 2) {
        *(u16*)(dest_bytes + i) = 0;
    }
    if (i < n) {
        *(dest_bytes + i) = 0;
    }

    return dest;
}
