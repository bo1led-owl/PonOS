#include "panic.h"

#include "utils.h"
#include "vga.h"

[[noreturn]] void vpanic(const char* fmt, va_list args) {
    cli();
    vgaSetBgColor(Color_Black);
    vgaSetFgColor(Color_Red);
    printf("panic: ");
    vprintf(fmt, args);
    infiniteLoop();
}

[[noreturn]] void panic(const char* fmt, ...) {
    va_list args;
    va_start(args);
    vpanic(fmt, args);
}
