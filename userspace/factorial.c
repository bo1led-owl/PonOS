int main(int argc) {
    for (;;) {
        __asm__ volatile(
            "sub esp, 4096\n"
            "mov [esp], %0" ::"r"(argc)
            : "esp");
    }

    return 0;
}
