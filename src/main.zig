const std = @import("std");

export fn kernelEntry() callconv(.Naked) noreturn {
    @as(*u16, @ptrFromInt(0xB8000)).* = 0;
    while (true) {}
}
