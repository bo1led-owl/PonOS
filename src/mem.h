#pragma once

#include "types.h"

void* memcpy(void* restrict dest, const void* restrict src, usize n);
void* memmove(void* dest, const void* src, usize n);
void* memzero(void* dest, usize n);
