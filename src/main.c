#include "interrupts.h"
#include "utils.h"
#include "vga.h"

static void setupWindows() {}

static void kernelInit() {
    setup8259();
    setupWindows();
    initScreen();
    setupInterrupts();
}

[[noreturn]] void kernelEntry() {
    kernelInit();

    infiniteLoop();
}
