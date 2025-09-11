bits 16
org 0x7C00

cli
xor ax, ax
mov ss, ax
mov ds, ax
mov es, ax
mov sp, 0x7C00

mov ah, 2
mov al, 1
mov bx, 0x7E00
xor dh, dh
xor ch, ch
mov cl, 2

; pmemsave 0x7E00 110 out

int 0x13
jc err

loop:
    jmp loop

err:
    mov ah, 0x0E
    mov bx, errMsg
    printLoop:
        mov al, [bx]
        test al, al
        jz loop
        int 0x10
        inc bx
        jmp printLoop
    hlt

errMsg:
    db "Error reading from disk", 0x0A, 0x0D, 0

times 510-($-$$) db 0
dw 0xAA55
