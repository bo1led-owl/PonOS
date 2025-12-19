#pragma once

#include "vga.h"

[[noreturn]] void startProcess(void (*entry)(), void* stack, WindowHandle w);

void prepareCtx(void* ctx, void (*entry)(), void* stack, WindowHandle w);

WindowHandle curWindowHandle();
