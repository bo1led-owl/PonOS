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

void vgaInit();
void vgaClear();
void vgaSetBgColor(Color c);
void vgaSetFgColor(Color c);

void vprintf(const char* fmt, va_list v);
void printf(const char* fmt, ...);

void putchar(char c);
