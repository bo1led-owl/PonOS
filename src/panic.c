#include "panic.h"

#include "utils.h"
#include "vga.h"

[[noreturn]] void vpanic(const char* fmt, va_list args) {
    disableInterrupts;

    WindowHandle w = mainWindow();
    setBgColor(w, Color_Blue);
    setFgColor(w, Color_White);
    clearWindow(w);
    printf(w, "panic: ");
    vprintf(w, fmt, args);
    infiniteLoop();
}

[[noreturn]] void panic(const char* fmt, ...) {
    va_list args;
    va_start(args);
    vpanic(fmt, args);
}
