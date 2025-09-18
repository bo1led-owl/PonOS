const std = @import("std");

export fn kernelEntry() callconv(.Naked) noreturn {
    asm volatile ("jmp main");
    while (true) {}
}

export fn main() void {
    @as(*u16, @ptrFromInt(0xB8000)).* = 0;
}
