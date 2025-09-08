const std = @import("std");
const AST = @import("../AST.zig");
const LIR = @import("LIR.zig");
const PIR = @import("PIR.zig");
const Result = @import("../result.zig").Result;

const VariableResolution = @import("VariableResolution.zig");
const LabelResolution = @import("LabelResolution.zig");
const LoopLabel = @import("LoopLabel.zig");

pub const Error = union(enum) {
    variable_resolution: VariableResolution.Error,
    label_resoluion: LabelResolution.Error,
    loop_label: LoopLabel.Error,

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self) {
            inline else => |err| try writer.print("{f}", .{err}),
        }
    }
};

pub fn analyze(in: AST, allocator: std.mem.Allocator) Result(PIR, Error) {
    const vars_reolved = switch (VariableResolution.resolve(in, allocator)) {
        .ok => |val| val,
        .err => |err| return .Err(.{ .variable_resolution = err }),
    };

    const labels_resolved = switch (LabelResolution.resolve(vars_reolved, allocator)) {
        .ok => |val| val,
        .err => |err| return .Err(.{ .label_resoluion = err }),
    };

    const loops_labeled = switch (LoopLabel.label(labels_resolved, allocator)) {
        .ok => |val| val,
        .err => |err| return .Err(.{ .loop_label = err }),
    };

    return .Ok(loops_labeled);
}
