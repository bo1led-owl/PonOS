#pragma once

#include "vga.h"

[[noreturn]] void startProcess(void (*entry)(), WindowHandle w);

WindowHandle curWindowHandle();
