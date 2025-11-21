#include "syscalls.h"

void printInt(unsigned n) {
    __asm__ volatile(
        "mov edi, %0\n"
        "int 0x30\n"
        :
        : "r"(n));
}
