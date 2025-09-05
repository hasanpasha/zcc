const std = @import("std");
const lir = @import("lir.zig");
const vir = @import("vir.zig");
const Result = @import("../result.zig").Result;
const Location = @import("../location.zig").Location;

allocator: std.mem.Allocator,
labels: std.StringHashMap([]const u8),
errs: std.array_list.Managed(ErrorItem),
counter: usize = 0,

const LabelResolution = @This();

const ErrorVariant = union(enum) {
    duplicate: []const u8,
    undeclared: []const u8,

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self) {
            .duplicate => |name| try writer.print("duplicate label '{s}'", .{name}),
            .undeclared => |name| try writer.print("undeclared label '{s}'", .{name}),
        }
    }
};

const ErrorItem = struct {
    err: ErrorVariant,
    loc: Location,

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try writer.print("{f} at {f}", .{
            self.err,
            std.fmt.alt(self.loc, .readableFmt),
        });
    }
};

pub const Error = struct {
    errs: std.array_list.Managed(ErrorItem),

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        for (self.errs.items, 1..) |err, i| {
            try writer.print("{}: {f}\n", .{ i, err });
        }
    }
};

pub fn resolve(in: vir.Program, allocator: std.mem.Allocator) Result(lir.Program, Error) {
    var arena = std.heap.ArenaAllocator.init(allocator);

    var self: LabelResolution = .{
        .allocator = arena.allocator(),
        .labels = .init(allocator),
        .errs = .init(allocator),
    };
    defer self.labels.deinit();

    self.gather(in.main_function.body);
    const main_func = self.function(in.main_function);

    if (self.errs.items.len > 0) {
        return .Err(.{ .errs = self.errs });
    }

    return .Ok(.{ .main_function = main_func, .arena = arena });
}

fn gather(self: *LabelResolution, blk: vir.Block) void {
    for (blk.items) |item| {
        if (item == .declaration) continue;
        self.gatherInStmt(item.statement);
    }
}

fn gatherInStmt(self: *LabelResolution, stmt: vir.Statement) void {
    switch (stmt) {
        .@"return", .expr, .goto, .null => {},
        .@"if" => |_if| {
            self.gatherInStmt(_if.then.*);
            if (_if.or_else) |s| self.gatherInStmt(s.*);
        },
        .labeled_stmt => |ls| {
            if (self.labels.contains(ls.label.name)) {
                self.fail(.{
                    .err = .{ .duplicate = ls.label.name },
                    .loc = ls.label.location,
                });
                return;
            }

            const unique_name = self.makeUniqueLabel(ls.label.name);
            self.labels.put(ls.label.name, unique_name) catch @panic("OOM");

            self.gatherInStmt(ls.stmt.*);
        },
        .compound => |blk| self.gather(blk),
    }
}

fn function(self: *LabelResolution, func: vir.Function) lir.Function {
    return .{
        .name = func.name,
        .body = self.block(func.body),
    };
}

fn block(self: *LabelResolution, blk: vir.Block) lir.Block {
    var items = self.allocator.alloc(lir.BlockItem, blk.items.len) catch @panic("OOM");
    for (blk.items, 0..) |item, i|
        items[i] = self.blockItem(item);
    return .{ .items = items };
}

fn blockItem(self: *LabelResolution, item: vir.BlockItem) lir.BlockItem {
    return switch (item) {
        .declaration => |decl| .{ .declaration = decl },
        .statement => |stmt| .{ .statement = self.statement(stmt) },
    };
}

fn statement(self: *LabelResolution, stmt: vir.Statement) lir.Statement {
    return switch (stmt) {
        .null => .null,
        .@"return" => |expr| .{ .@"return" = expr },
        .expr => |expr| .{ .expr = expr },
        .@"if" => |_if| .{
            .@"if" = .{
                .cond = _if.cond,
                .then = self.onHeap(self.statement(_if.then.*)),
                .or_else = if (_if.or_else) |s| self.onHeap(self.statement(s.*)) else null,
            },
        },
        .goto => |target| .{
            .goto = self.labels.get(target.name) orelse placeholder: {
                self.fail(.{
                    .err = .{ .undeclared = target.name },
                    .loc = target.location,
                });
                break :placeholder target.name;
            },
        },
        .labeled_stmt => |ls| .{ .labeled_stmt = .{
            .label = self.labels.get(ls.label.name) orelse unreachable,
            .stmt = self.onHeap(self.statement(ls.stmt.*)),
        } },
        .compound => |blk| .{ .compound = self.block(blk) },
    };
}

const onHeap = @import("../utils.zig").onHeap;

fn fail(self: *LabelResolution, err: ErrorItem) void {
    self.errs.append(err) catch @panic("OOM");
}

fn makeUniqueLabel(self: *LabelResolution, suffix: []const u8) []u8 {
    defer self.counter += 1;
    return std.fmt.allocPrint(self.allocator, ".{s}.{}", .{
        suffix,
        self.counter,
    }) catch @panic("OOM");
}
