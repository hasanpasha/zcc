const std = @import("std");

pub fn oneOf(e: anytype, es: []const @TypeOf(e)) bool {
    for (es) |_e| if (std.meta.eql(e, _e)) return true;
    return false;
}

pub fn onHeap(self: anytype, x: anytype) *@TypeOf(x) {
    const ptr = self.allocator.create(@TypeOf(x)) catch @panic("OOM");
    ptr.* = x;
    return ptr;
}
