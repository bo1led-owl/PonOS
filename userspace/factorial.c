#include "lib.h"

int main() {
    for (int i = 0;; ++i) {
        printf("%d\n", i);
        __asm__ volatile(
            "sub esp, 4096\n"
            "mov [esp], %0" ::"r"(i)
            : "esp");
    }

    return 0;
}
