#pragma once

#include "types.h"

/// Copy [`dest`, `dest` + `n`) into [`src`, `src` + `n`).
/// If the ranges intersect, behaviour if undefined.
void* memcpy(void* restrict dest, const void* restrict src, usize n);

/// Copy [`dest`, `dest` + `n`) into [`src`, `src` + `n`) from lower adresses to higher.
void* copyForwards(void* dest, const void* src, usize n);

/// Copy [`dest`, `dest` + `n`) into [`src`, `src` + `n`) from higher adresses to lower.
void* copyBackwards(void* dest, const void* src, usize n);

/// Copy [`dest`, `dest` + `n`) into [`src`, `src` + `n`).
/// Copying is performed as if `dest` was first copied into a temporary buffer.
void* memmove(void* dest, const void* src, usize n);

/// Fill [`dest`, `dest` + `n`) with zeros
void* memzero(void* dest, usize n);
