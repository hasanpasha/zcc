const std = @import("std");
const Writer = std.Io.Writer;

line: usize,
column: usize,

pub const start = @This(){ .line = 1, .column = 1 };

pub fn format(self: @This(), writer: *std.Io.Writer) Writer.Error!void {
    try writer.print("Location{{ line: {}, column: {} }}", .{
        self.line,
        self.column,
    });
}

pub fn readableFmt(self: @This(), writer: *Writer) Writer.Error!void {
    try writer.print("{}:{}", .{
        self.line,
        self.column,
    });
}
