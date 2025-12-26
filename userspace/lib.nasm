bits 32

global exit
exit:
    mov eax, [esp + 4]
    int 0x30

global printChar
printChar:
    mov eax, [esp + 4]
    int 0x31
    ret
