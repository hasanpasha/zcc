const std = @import("std");

pub fn oneOf(e: anytype, es: []const @TypeOf(e)) bool {
    for (es) |_e| if (std.meta.eql(e, _e)) return true;
    return false;
}

pub fn onHeap(self: anytype, x: anytype) std.mem.Allocator.Error!*@TypeOf(x) {
    const ptr = try self.allocator.create(@TypeOf(x));
    ptr.* = x;
    return ptr;
}
