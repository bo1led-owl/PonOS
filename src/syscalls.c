#include "syscalls.h"

void write(WindowHandle w, const char* buf, usize n) {
    __asm__ volatile(
        "mov edi, %0\n"
        "mov esi, %1\n"
        "mov edx, %2\n"
        "int 0x30\n"
        :
        : "r"(w), "r"(buf), "r"(n));
}
