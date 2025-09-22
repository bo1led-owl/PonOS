#include "alloc.h"
#include "utils.h"
#include "vga.h"

[[noreturn]] void kernelEntry() {
    WindowHandle windows[4];
    windows[0] = addWindow(0, 0, 10, 40);
    windows[1] = addWindow(40, 0, 10, 40);
    windows[2] = addWindow(0, 10, 15, 40);
    windows[3] = addWindow(40, 10, 15, 40);
    initScreen();

    setFgColor(windows[1], Color_Red);
    setFgColor(windows[2], Color_Cyan);
    setFgColor(windows[3], Color_Green);

    for (usize i = 0;; i += 27) {
        usize size = i % 256;
        usize alignment = i % 64;
        printf(windows[i % 4], "size = %u alignment = %u: ", size, alignment);
        void* p = mallocImmortal(size, alignment);
        printf(windows[i % 4], "0x%p\n", p);
    }

    infiniteLoop();
}
