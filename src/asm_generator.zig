const std = @import("std");
const Writer = std.Io.Writer;

const x86_64 = @import("x86_64/root.zig");

pub const Arch = enum {
    x86_64,
};

pub const AIR = union(Arch) {
    x86_64: x86_64.AIR,

    pub fn format(self: @This(), writer: *Writer) Writer.Error!void {
        switch (self) {
            inline else => |program| try writer.print("{f}", .{program}),
        }
    }

    pub fn emit(self: AIR, writer: *std.Io.Writer) Writer.Error!void {
        switch (self) {
            inline else => |p| try p.emit(writer),
        }
    }

    pub const FileEmitError = std.fs.File.OpenError || Writer.Error;

    pub fn emitToFile(self: AIR, path: []const u8) FileEmitError!void {
        const file = try std.fs.cwd().createFile(path, .{});
        defer file.close();

        var buffer: [1024]u8 = undefined;
        var file_buffer = file.writer(&buffer);
        const writer = &file_buffer.interface;

        try self.emit(writer);
    }

    pub fn free(self: AIR) void {
        switch (self) {
            inline else => |p| p.free(),
        }
    }
};

const TackyIR = @import("TackyIR.zig");

pub fn lower(ir: TackyIR, arch: Arch, allocator: std.mem.Allocator) AIR {
    return switch (arch) {
        .x86_64 => .{ .x86_64 = x86_64.lower(ir, allocator) },
    };
}
