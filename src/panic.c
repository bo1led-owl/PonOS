#include "panic.h"

#include "asmUtils.h"
#include "vga.h"

[[noreturn]] void vpanic(const char* fmt, va_list args) {
    cli();
    WindowHandle w = mainWindow();
    // идея с синим экраном честно украдена у @EugeneKornev
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
