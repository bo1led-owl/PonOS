bits 32

global printInt
printInt:
    mov edi, [esp + 4]
    int 0x30
    ret

global exit
exit:
    mov edi, [esp + 4]
    int 0x31
