const std = @import("std");

export fn kernelEntry() callconv(.Naked) noreturn {
    asm volatile (
        \\ call main
        \\ hlt
    );
}

export fn main() void {
    @as(*u16, @ptrFromInt(0xB8000)).* = 0;
}
