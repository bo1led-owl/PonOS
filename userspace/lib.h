#pragma once

#include <stdarg.h>

#ifdef NDEBUG
#define assert(cond) ((cond) ? (void)(0) : __builtin_unreachable())
#else
#define assert(cond) \
    ((cond) ? (void)(0) : exit(1))
#endif

void vprintf(const char* fmt, va_list v);
void printf(const char* fmt, ...);

[[noreturn]] void exit(int status);
void printChar(char c);
