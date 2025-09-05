const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const clap_dep = b.dependency("clap", .{});
    const clap_mod = clap_dep.module("clap");

    const zcc_mod = b.addModule("zcc", .{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "clap", .module = clap_mod },
        },
    });

    const zcc_exe = b.addExecutable(.{
        .name = "zcc",
        .root_module = zcc_mod,
    });

    const zcc_install = b.addInstallArtifact(zcc_exe, .{});

    const zcc_run = b.addRunArtifact(zcc_exe);
    zcc_run.step.dependOn(&zcc_install.step);

    if (b.args) |args| {
        zcc_run.addArgs(args);
    }

    const zcc_run_step = b.step("run", "run zcc");
    zcc_run_step.dependOn(&zcc_run.step);

    const book_test = b.addSystemCommand(&.{ "./book-tests/test_compiler", "./zig-out/bin/zcc", "--verbose" });
    book_test.step.dependOn(&zcc_install.step);
    if (b.args) |args| {
        book_test.addArgs(args);
    }

    const book_test_step = b.step("book-test", "run book tests");
    book_test_step.dependOn(&book_test.step);
}
