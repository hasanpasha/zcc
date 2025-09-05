const std = @import("std");

const x86_64 = @import("x86_64/root.zig");

pub const Arch = enum {
    x86_64,
};

pub const ProgramASM = union(Arch) {
    x86_64: x86_64.as.Program,

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self) {
            inline else => |program| try writer.print("{f}", .{program}),
        }
    }

    pub fn emit(self: ProgramASM, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        switch (self) {
            inline else => |p| try p.emit(writer),
        }
    }

    pub const FileEmitError = std.fs.File.OpenError || std.Io.Writer.Error;

    pub fn emitToFile(self: ProgramASM, path: []const u8) FileEmitError!void {
        const file = try std.fs.cwd().createFile(path, .{});
        defer file.close();

        var buffer: [1024]u8 = undefined;
        var file_buffer = file.writer(&buffer);
        const writer = &file_buffer.interface;

        try self.emit(writer);
    }

    pub fn free(self: ProgramASM) void {
        switch (self) {
            inline else => |p| p.free(),
        }
    }
};

const tacky_ir = @import("tacky_ir.zig");

pub fn generate(ir: tacky_ir.ProgramTackyIR, arch: Arch, allocator: std.mem.Allocator) ProgramASM {
    return switch (arch) {
        .x86_64 => .{ .x86_64 = x86_64.generate(ir, allocator) },
    };
}
