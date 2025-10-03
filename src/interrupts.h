#pragma once

struct InterruptCtx;

void setupInterrupts();

void universalHandler(const struct InterruptCtx* ctx);
