bits 32

global infiniteLoop
infiniteLoop:
    jmp infiniteLoop

global infiniteRecursion
infiniteRecursion:
    sub esp, 4092
    call infiniteRecursion

global rec
rec:
    mov eax, [esp + 4]
    test eax, eax
    jz to_ret
        sub esp, 4088
        dec eax
        push eax
        call rec
        add esp, 4092
to_ret:
    ret
