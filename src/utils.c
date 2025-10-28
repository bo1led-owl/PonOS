#include "utils.h"

[[noreturn]] void infiniteLoop() {
    __asm__ volatile("jmp infiniteLoop");
    __builtin_unreachable();
}
