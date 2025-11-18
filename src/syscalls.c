#include "syscalls.h"

void write(const char* buf, usize n) {
    __asm__ volatile(
        "mov edi, %0\n"
        "mov esi, %1\n"
        "int 0x30\n"
        :
        : "r"(buf), "r"(n));
}
