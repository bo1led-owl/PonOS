#pragma once

#include <stdarg.h>

#include "types.h"

typedef enum : u8 {
    Color_Black = 0,
    Color_Blue = 0x1,
    Color_Green = 0x2,
    Color_Cyan = 0x3,
    Color_Red = 0x4,
    Color_Purple = 0x5,
    Color_Brown = 0x6,
    Color_Gray = 0x7,
    Color_DarkGray = 0x8,
    Color_LightBlue = 0x9,
    Color_LightGreen = 0xa,
    Color_LightCyan = 0xb,
    Color_LightRed = 0xc,
    Color_LightPurple = 0xd,
    Color_Yellow = 0xe,
    Color_White = 0xf,
} Color;

struct Window;
typedef struct Window* WindowHandle;

/// Add a new window to pool. This must be called before `initScreen`.
/// Returns a handle to the new window.
WindowHandle addWindow(usize start_x, usize start_y, usize rows, usize columns);

/// Initialize all windows on screen.
void initScreen();
WindowHandle mainWindow();

void clearWindow(WindowHandle w);

void setBgColor(WindowHandle w, Color c);
void setFgColor(WindowHandle w, Color c);

void vprintf(WindowHandle w, const char* fmt, va_list v);
void printf(WindowHandle w, const char* fmt, ...);

void putchar(WindowHandle w, char c);
