#pragma once

[[noreturn]] void infiniteLoop();

#define disableInterrupts __asm__ volatile("cli")
#define enableInterrupts __asm__ volatile("sti")
