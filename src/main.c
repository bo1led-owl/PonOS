#include "interrupts.h"
#include "utils.h"
#include "vga.h"

static WindowHandle w;

static void setupWindows() {
    w = addWindow(0, 0, VGA_ROWS, VGA_COLUMNS);
}

static void kernelInit() {
    setup8259();
    setupInterrupts();

    setupWindows();
    initScreen();

    enableInterrupts;
}

[[noreturn]] void kernelEntry() {
    kernelInit();

    infiniteLoop();
}
