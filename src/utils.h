#pragma once

#include "assert.h"

[[noreturn]] void infiniteLoop();

#define UNREACHABLE assert(false && "unreachable code reached")
