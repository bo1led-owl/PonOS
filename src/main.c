#include "asmUtils.h"
#include "interrupts.h"

#define SETUP_REGISTERS \
    __asm__ volatile(   \
        "mov eax, 0\n"  \
        "mov ecx, 1\n"  \
        "mov edx, 2\n"  \
        "mov ebx, 3\n"  \
        "mov ebp, 4\n"  \
        "mov esi, 5\n"  \
        "mov edi, 6\n")

#define divisionByZeroExperiment \
    SETUP_REGISTERS;             \
    __asm__ volatile("idiv eax\n");

#define syscallExperiment \
    SETUP_REGISTERS;      \
    __asm__ volatile("int 42\n");

#define ioExperiment \
    SETUP_REGISTERS; \
    __asm__ volatile("sti\n");

[[noreturn]] void kernelEntry() {
    setupInterrupts();

    // divisionByZeroExperiment
    // syscallExperiment
    // ioExperiment

    infiniteLoop();
}
