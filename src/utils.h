#pragma once

#include "assert.h"

[[noreturn]] extern void infiniteLoop();
[[noreturn]] extern void infiniteRecursion();

#define UNREACHABLE assert(false && "unreachable code reached")
