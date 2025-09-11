[BITS 16]
[ORG 0x7C00]

cli
xor ax, ax
mov ds, ax
mov ss, ax
mov sp, 0x7C00

mov bx, msg
mov ah, 0x0E

printLoop:
    mov al, [bx]
    test al, al
    jz loop
    int 0x10
    inc bx
    jmp printLoop

loop:
    jmp loop

msg:
    db "Hello world!", 0x0A, 0x0D, 0

times 510-($-$$) db 0
dw 0xAA55
