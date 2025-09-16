target remote localhost:1234
tui enable
lay asm
lay regs
b *0x7c00
c
