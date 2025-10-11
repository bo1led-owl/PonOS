#pragma once

[[noreturn]] void infiniteLoop();

#define cli __asm__ volatile("cli")
#define sti __asm__ volatile("sti")
