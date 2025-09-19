const std = @import("std");

pub fn build(b: *std.Build) !void {
    var disabled_features = std.Target.Cpu.Feature.Set.empty;
    var enabled_features = std.Target.Cpu.Feature.Set.empty;

    disabled_features.addFeature(@intFromEnum(std.Target.x86.Feature.x87));
    disabled_features.addFeature(@intFromEnum(std.Target.x86.Feature.mmx));
    disabled_features.addFeature(@intFromEnum(std.Target.x86.Feature.sse));
    disabled_features.addFeature(@intFromEnum(std.Target.x86.Feature.sse2));
    enabled_features.addFeature(@intFromEnum(std.Target.x86.Feature.soft_float));

    const i386_freestanding = b.resolveTargetQuery(.{
        .cpu_arch = std.Target.Cpu.Arch.x86,
        .os_tag = std.Target.Os.Tag.freestanding,
        .abi = std.Target.Abi.none,
        .cpu_model = .{ .explicit = &std.Target.x86.cpu.i386 },
        .cpu_features_sub = disabled_features,
        .cpu_features_add = enabled_features,
    });

    const optimize = b.standardOptimizeOption(.{ .preferred_optimize_mode = .ReleaseSmall });

    const kernel_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = i386_freestanding,
        .optimize = optimize,
        .code_model = .kernel,
        .pic = false,
        .single_threaded = true,
        .link_libc = false,
        .stack_protector = false,
        .strip = false,
    });

    const kernel = b.addExecutable(.{
        .name = "kernel.o",
        .root_module = kernel_mod,
    });
    kernel.entry = .{ .symbol_name = "kernelEntry" };
    kernel.bundle_compiler_rt = false;
    kernel.bundle_ubsan_rt = false;
    kernel.addObjectFile(addNasm(b, b.path("src/loader.nasm"), "loader.o"));
    kernel.setLinkerScript(b.path("link.ld"));

    b.getInstallStep().dependOn(&b.addInstallFile(kernel.getEmittedBin(), "kernel.o").step);
    const kernel_bin = b.addObjCopy(kernel.getEmittedBin(), .{ .format = .bin });
    b.getInstallStep().dependOn(&b.addInstallFile(kernel_bin.getOutput(), "kernel.bin").step);

    const image = b.addSystemCommand(&.{"sh"});
    image.addFileArg(b.path("build_image.sh"));
    image.addFileArg(kernel_bin.getOutput());
    const boot_img = image.addOutputFileArg("boot.img");

    b.getInstallStep().dependOn(&b.addInstallFile(boot_img, "boot.img").step);

    b.installArtifact(kernel);

    const run_image = b.addSystemCommand(&.{
        "qemu-system-i386", "-cpu",
        "pentium2",         "-m",
        "4g",               "-monitor",
        "stdio",            "-device",
        "VGA",              "-fda",
    });
    run_image.addFileArg(boot_img);
    if (b.args) |args| {
        run_image.addArgs(args);
    }

    const run_step = b.step("run", "Run the kernel in QEMU");
    run_step.dependOn(&run_image.step);
}

pub fn addNasm(b: *std.Build, source: std.Build.LazyPath, name: []const u8) std.Build.LazyPath {
    const cmd = b.addSystemCommand(&.{ "nasm", "-O0", "-felf", "-o" });
    const res = cmd.addOutputFileArg(name);
    cmd.addFileArg(source);
    return res;
}
