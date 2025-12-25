bits 32
extern main

global _start
_start:
    sub esp, 8
    push ecx
    push eax
    call main
    int 0x30
