bits 32

extern universalHandler
global collectCtx
collectCtx:
    push ds
    push es
    push fs
    push gs
    pusha

    ; clear D flag
    cld

    ; reset segment registers
    mov eax, KERNEL_DATA_SEGMENT
    mov ds, eax
    mov es, eax
    mov fs, eax
    mov gs, eax

    mov ebx, esp

    ; align stack
    lea ecx, [esp + 12]
    and ecx, 0xF
    sub esp, ecx

    push ebx
    call universalHandler
    mov esp, ebx
restoreCtxFromEsp:
    popa
    pop gs
    pop fs
    pop es
    pop ds
    add esp, 8 ; error code and vector
    iret

global restoreCtx
restoreCtx:
    mov esp, [esp + 4]
    jmp restoreCtxFromEsp
