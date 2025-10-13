#include "portIo.h"

static void writeToPortRaw(u16 port, u8 payload) {
    __asm__ volatile(
        "mov al, %0\n"
        "mov dx, %1\n"
        "out dx, al\n"
        :
        : "r"(payload), "r"(port)
        : "al", "dx");
}

[[gnu::noinline]] [[clang::noinline]] static void readWriteDelay() {
    for (int i = 0; i < 100; ++i) {
        writeToPortRaw(0x80, 0);
    }
}

u8 readFromPort(u16 port) {
    u8 res;
    __asm__ volatile(
        "mov dx, %1\n"
        "in al, dx\n"
        "mov %0, al\n"
        : "=r"(res)
        : "r"(port)
        : "al", "dx");
    readWriteDelay();
    return res;
}

void writeToPort(u16 port, u8 payload) {
    writeToPortRaw(port, payload);
    readWriteDelay();
}
