#include "hardwareIo.h"
#include "interrupts.h"
#include "utils.h"
#include "vga.h"

static WindowHandle w;
static int globalCounter = 0;

constexpr int LIMIT = 42;

static void delay() {
    for (int i = 0; i < 2500; ++i) {
        printf(w, "%d\n", i);
        writeToPort(0x80, 0);
    }
}

static void timerHandler([[maybe_unused]] const InterruptCtx* ctx) {
    u8 mask = getMasterDeviceMask();
    mask &= ~ENABLE_TIMER;
    setMasterDeviceMask(mask);

    delay();

    sti;

    delay();
}

static void keyboardHandler([[maybe_unused]] const InterruptCtx* ctx) {
    printf(w, "%u ", readFromPort(0x60));
    infiniteLoop();
}

[[noreturn]] void experiment() {
    setup8259(true);
    setMasterDeviceMask(ENABLE_TIMER | ENABLE_KEYBOARD);
    overrideIterruptHandler(TIMER_INTERRUPT_VECTOR, Gate_Interrupt, timerHandler);
    overrideIterruptHandler(KEYBOARD_INTERRUPT_VECTOR, Gate_Interrupt, keyboardHandler);
    sti;
    infiniteLoop();
}

static void kernelInit() {
    w = mainWindow();
    clearWindow(w);
    initScreen();
    setupInterrupts();
}

[[noreturn]] void kernelEntry() {
    kernelInit();
    experiment();
}
