#include "alloc.h"
#include "utils.h"
#include "vga.h"

[[noreturn]] void kernelEntry() {
    vgaInit();

    for (usize i = 0;; i += 36) {
        usize size = i % 256;
        usize alignment = i % 64;
        printf("size = %u alignment = %u: ", size, alignment);
        void* p = mallocImmortal(size, alignment);
        printf("0x%p\n", p);
    }

    infiniteLoop();
}
