const std = @import("std");
const Writer = std.Io.Writer;
const LIR = @import("LIR.zig");
const VIR = @import("VIR.zig");
const Result = @import("../result.zig").Result;
const Location = @import("../Location.zig");

allocator: std.mem.Allocator,
labels: std.StringHashMap([]const u8),
errs: std.array_list.Managed(ErrorItem),
counter: usize = 0,

const LabelResolver = @This();

const ErrorVariant = union(enum) {
    duplicate: []const u8,
    undeclared: []const u8,

    pub fn format(self: @This(), writer: *Writer) Writer.Error!void {
        switch (self) {
            .duplicate => |name| try writer.print("duplicate label '{s}'", .{name}),
            .undeclared => |name| try writer.print("undeclared label '{s}'", .{name}),
        }
    }
};

const ErrorItem = struct {
    err: ErrorVariant,
    loc: Location,

    pub fn format(self: @This(), writer: *Writer) Writer.Error!void {
        try writer.print("{f} at {f}", .{
            self.err,
            std.fmt.alt(self.loc, .readableFmt),
        });
    }
};

pub const Error = struct {
    errs: std.array_list.Managed(ErrorItem),

    pub fn format(self: @This(), writer: *Writer) Writer.Error!void {
        for (self.errs.items, 1..) |err, i| {
            try writer.print("{}: {f}\n", .{ i, err });
        }
    }
};

pub fn resolve(in: VIR, allocator: std.mem.Allocator) Result(LIR, Error) {
    var arena = std.heap.ArenaAllocator.init(allocator);

    var self: LabelResolver = .{
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

fn gather(self: *LabelResolver, blk: VIR.Block) void {
    for (blk.items) |item| {
        if (item == .declaration) continue;
        self.gatherInStmt(item.statement);
    }
}

fn gatherInStmt(self: *LabelResolver, stmt: VIR.Statement) void {
    switch (stmt) {
        .@"return", .expr, .goto, .null, .@"break", .@"continue" => {},
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
        .@"for" => |_for| self.gatherInStmt(_for.body.*),
        .@"while" => |_while| self.gatherInStmt(_while.body.*),
        .do_while => |do_while| self.gatherInStmt(do_while.body.*),
        .@"switch" => |_switch| self.gatherInStmt(_switch.body.*),
        .case => |case| if (case.stmt) |_stmt| self.gatherInStmt(_stmt.*),
        .default => |default| if (default.stmt) |_stmt| self.gatherInStmt(_stmt.*),
    }
}

fn function(self: *LabelResolver, func: VIR.Function) LIR.Function {
    return .{
        .name = func.name,
        .body = self.block(func.body),
    };
}

fn block(self: *LabelResolver, blk: VIR.Block) LIR.Block {
    var items = self.allocator.alloc(LIR.BlockItem, blk.items.len) catch @panic("OOM");
    for (blk.items, 0..) |item, i|
        items[i] = self.blockItem(item);
    return .{ .items = items };
}

fn blockItem(self: *LabelResolver, item: VIR.BlockItem) LIR.BlockItem {
    return switch (item) {
        .declaration => |decl| .{ .declaration = decl },
        .statement => |stmt| .{ .statement = self.statement(stmt) },
    };
}

fn statement(self: *LabelResolver, stmt: VIR.Statement) LIR.Statement {
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
        .@"break" => .@"break",
        .@"continue" => .@"continue",
        .@"while" => |_while| .{ .@"while" = .{
            .cond = _while.cond,
            .body = self.onHeap(self.statement(_while.body.*)),
        } },
        .do_while => |do_while| .{ .do_while = .{
            .body = self.onHeap(self.statement(do_while.body.*)),
            .cond = do_while.cond,
        } },
        .@"for" => |_for| .{ .@"for" = .{
            .init = switch (_for.init) {
                .decl => |decl| .{ .decl = decl },
                .expr => |expr| .{ .expr = expr },
            },
            .cond = _for.cond,
            .post = _for.post,
            .body = self.onHeap(self.statement(_for.body.*)),
        } },
        .@"switch" => |_switch| .{ .@"switch" = .{
            .cond = _switch.cond,
            .body = self.onHeap(self.statement(_switch.body.*)),
        } },
        .case => |case| .{ .case = .{
            .expr = case.expr,
            .stmt = if (case.stmt) |_stmt| self.onHeap(self.statement(_stmt.*)) else null,
        } },
        .default => |default| .{ .default = .{
            .stmt = if (default.stmt) |_stmt| self.onHeap(self.statement(_stmt.*)) else null,
        } },
    };
}

const onHeap = @import("../utils.zig").onHeap;

fn fail(self: *LabelResolver, err: ErrorItem) void {
    self.errs.append(err) catch @panic("OOM");
}

fn makeUniqueLabel(self: *LabelResolver, suffix: []const u8) []u8 {
    defer self.counter += 1;
    return std.fmt.allocPrint(self.allocator, ".{s}.{}", .{
        suffix,
        self.counter,
    }) catch @panic("OOM");
}
