const std = @import("std");
const Writer = std.Io.Writer;

const AST = @import("../AST.zig");

const VariableResolver = @import("VariableResolver.zig");
const LabelResolver = @import("LabelResolver.zig");
const StmtLabel = @import("StmtLabel.zig");

pub fn analyze(in: *AST) !struct { usize, usize } {
    var var_counter: usize = 0;
    var label_counter: usize = 0;
    try VariableResolver.resolve(in, &var_counter);
    std.log.debug("after VariableResolution {f}", .{in.*});
    try LabelResolver.resolve(in, &label_counter);
    std.log.debug("after LabelResolution {f}", .{in.*});
    try StmtLabel.label(in, &label_counter);
    std.log.debug("after StmtLabel {f}", .{in.*});
    return .{ var_counter, label_counter };
}
