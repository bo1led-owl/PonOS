bits 16
org 0x7C00

; pmemsave 0x7E00 14696 out

; geometry: https://www.deathwombat.com/diskgeometry.html
%define SECTORS_PER_TRACK 18
%define HEADS_PER_CYLINDER 2
%define CYLINDERS 80

cli
xor ax, ax

mov ss, ax
mov sp, 0x7C00

mov bx, ax
mov ds, ax

mov ax, 0x7E0     ; minus one zero because segment registers are shifted
mov es, ax

mov al, 1         ; amount of sectors to read

xor dh, dh        ; header number = 0
xor ch, ch        ; cylinder number = 0
mov cl, 2         ; sector number = 2 (to skip the first sector)

readLoop:
    ; this should be reset each time because BIOS resets `ah`
    mov ah, 2     ; function number (0x2 - read sectors)
    int 0x13      ; read
    jc handleErr

    add bx, 0x200 ; move dest
    inc cl        ; next sector
    cmp cl, SECTORS_PER_TRACK
    jbe .continueReading
    ; should move header
    mov cl, 1     ; reset sector number to 1
    inc dh        ; move header
    cmp dh, HEADS_PER_CYLINDER
    jbe .continueReading
    ; should change cylinder
    xor dh, dh    ; reset header
    inc ch        ; next cylinder
    cmp ch, CYLINDERS
    jae handleErr ; in case we go too far
.continueReading:
    cmp bx, PAYLOAD_SIZE
    jb readLoop

end:
    hlt

handleErr:
    mov ah, 0x0E
    mov bx, errMsg
    .printLoop:
        mov al, [bx]
        test al, al
        jz end
        int 0x10
        inc bx
        jmp .printLoop

errMsg: db "Error reading from disk", 0

times 510-($-$$) db 0
dw 0xAA55
