#pragma once

#include "panic.h"

#ifdef NDEBUG
#define assert(cond) ((cond) ? (void)(0) : __builtin_unreachable())
#else
#define assert(cond) ((cond) ? (void)(0) : panic(__FILE__ ":%d: assertion " #cond " failed", __LINE__))
#endif
