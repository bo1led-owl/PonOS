#pragma once

#include "interrupts.h"
#include "paging.h"
#include "vga.h"

void addProcess(usize package, WindowHandle w, int argc, ...);

[[noreturn]] void startProcesses();
[[noreturn]] void stopCurProcess();

WindowHandle curWindowHandle();
PageDirectoryEntry* curPageDirectory();
Page* getCurProcessStackLimit();
void setCurProcessStackLimit(Page* limit);

void timerHandler(InterruptCtx* ctx);
