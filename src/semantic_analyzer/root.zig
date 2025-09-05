const std = @import("std");
const ast = @import("../ast.zig");
const vir = @import("vir.zig");
const lir = @import("lir.zig");
const Result = @import("../result.zig").Result;

const VariableResolution = @import("VariableResolution.zig");
const LabelResolution = @import("LabelResolution.zig");

pub const Error = union(enum) {
    variable_resolution: VariableResolution.Error,
    label_resoluion: LabelResolution.Error,

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self) {
            inline else => |err| try writer.print("{f}", .{err}),
        }
    }
};

pub fn analyze(in: ast.Program, allocator: std.mem.Allocator) Result(lir.Program, Error) {
    const vars_reolved = switch (VariableResolution.resolve(in, allocator)) {
        .ok => |val| val,
        .err => |err| return .Err(.{ .variable_resolution = err }),
    };

    const labels_resolved = switch (LabelResolution.resolve(vars_reolved, allocator)) {
        .ok => |val| val,
        .err => |err| return .Err(.{ .label_resoluion = err }),
    };

    return .Ok(labels_resolved);
}
