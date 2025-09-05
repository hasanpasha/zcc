const std = @import("std");

line: usize,
column: usize,

pub const start = @This(){ .line = 1, .column = 1 };

pub fn format(self: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
    try writer.print("Location{{ line: {}, column: {} }}", .{
        self.line,
        self.column,
    });
}

pub fn readableFmt(
    self: @This(),
    writer: *std.Io.Writer,
) std.Io.Writer.Error!void {
    try writer.print("{}:{}", .{
        self.line,
        self.column,
    });
}
