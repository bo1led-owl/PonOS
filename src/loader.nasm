bits 16
section .boot

; geometry: https://www.deathwombat.com/diskgeometry.html
%define SECTORS_PER_TRACK 18
%define HEADS_PER_CYLINDER 2
%define SECTOR_PAIRS_TO_LOAD ((0x80000 - 0x7E00) / 1024)

cli
xor ax, ax

mov ss, ax
mov sp, 0x7C00

xor bx, bx
mov ds, bx

mov ax, 0x7E0     ; minus one zero because segment registers are shifted
mov es, ax

xor dh, dh        ; header number   = 0
xor ch, ch        ; cylinder number = 0
mov cl, 2         ; sector number   = 2 (to skip the first sector)

mov si, SECTOR_PAIRS_TO_LOAD

readLoop:
    ; function : sectors to read
    mov ax, 0x0202
    int 0x13      ; read
    ; jc handleErr

    mov di, es
    add di, 0x40  ; move dest
    mov es, di

    add cl, 2     ; next kb
    cmp cl, SECTORS_PER_TRACK
    jbe .continueReading
    ; should move header
    mov cl, 1     ; reset sector number to 1
    inc dh        ; move header
    cmp dh, HEADS_PER_CYLINDER
    jb .continueReading
    ; should change cylinder
    xor dh, dh    ; reset header
    inc ch        ; next cylinder
.continueReading:
    sub si, 2
    jnz readLoop

; going into 32 bit mode
lgdt [gdt_descriptor]
cld

mov eax, cr0
or eax, 1
mov cr0, eax

jmp CODE_SEGMENT:trampoline

bits 32
trampoline:
mov eax, DATA_SEGMENT
mov ds, eax
mov ss, eax
mov es, eax
mov fs, eax
mov gs, eax

extern kernelEntry
sub esp, 12
jmp kernelEntry

end:
    hlt

bits 16
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

gdt_descriptor:
  dw 0x17
  dd gdt

align 8
gdt:
    .null:                  dq 0
    csd:
        .limitLo:           dw 0xFF
        .baseLo:            dw 0
        .baseMid:           db 0
        .P_DPL_S_type:      db 0b1001_1010
        .G_B_0_AVL_limitHi: db 0b1100_1111
        .baseHi:            db 0
    dsd:
        .limitLo:           dw 0xFF
        .baseLo:            dw 0
        .baseMid:           db 0
        .P_DPL_S_type:      db 0b1001_0010
        .G_B_0_AVL_limitHi: db 0b1100_1111
        .baseHi:            db 0

CODE_SEGMENT equ 8
DATA_SEGMENT equ 16

times 510-($-$$) db 0
dw 0xAA55
