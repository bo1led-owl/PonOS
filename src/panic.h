#pragma once

#include <stdarg.h>

[[noreturn]] void vpanic(const char* fmt, va_list args);
[[noreturn]] void panic(const char* fmt, ...);
