const std = @import("std");

const LIR = @import("LIR.zig");

const PIR = @import("PIR.zig");
const Location = @import("../Location.zig");
const Result = @import("../result.zig").Result;

const LoopLabel = @This();

pub const ErrorVariant = union(enum) {
    break_stmt_outside_of_loop,
    continue_stmt_outside_of_loop,
};

// const ErrorItem = struct {
//     err: ErrorVariant,
//     // loc: Location,

//     pub fn format(
//         self: @This(),
//         writer: *std.Io.Writer,
//     ) std.Io.Writer.Error!void {
//         try writer.print("{f}", .{
//             self.err,
//                 // std.fmt.alt(self.loc, .readableFmt),
//         });
//     }
// };

pub const Error = struct {
    errs: std.array_list.Managed(ErrorVariant),

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        for (self.errs.items, 1..) |err, i| {
            try writer.print("{}: {}\n", .{ i, err });
        }
    }
};

allocator: std.mem.Allocator,
errs: std.array_list.Managed(ErrorVariant),
labels_stack: std.array_list.Managed([]const u8),
counter: usize = 0,

pub fn label(in: LIR, allocator: std.mem.Allocator) Result(PIR, Error) {
    var arena = std.heap.ArenaAllocator.init(allocator);

    var self = LoopLabel{
        .allocator = arena.allocator(),
        .labels_stack = .init(allocator),
        .errs = .init(allocator),
    };
    defer self.labels_stack.deinit();

    const main = self.function(in.main_function);

    if (self.errs.items.len > 0) {
        return .Err(.{ .errs = self.errs });
    }

    return .Ok(.{ .main_function = main, .arena = arena });
}

fn function(self: *LoopLabel, in: LIR.Function) PIR.Function {
    const body = self.block(in.body);

    return .{ .name = in.name, .body = body };
}

fn block(self: *LoopLabel, in: LIR.Block) PIR.Block {
    var items = self.allocator.alloc(PIR.BlockItem, in.items.len) catch @panic("OOM");
    for (in.items, 0..) |item, i|
        items[i] = switch (item) {
            .declaration => |decl| .{ .declaration = decl },
            .statement => |stmt| .{ .statement = self.statement(stmt) },
        };

    return .{ .items = items };
}

fn statement(self: *LoopLabel, in: LIR.Statement) PIR.Statement {
    return switch (in) {
        .null => .null,
        .@"return" => |expr| .{ .@"return" = expr },
        .expr => |expr| .{ .expr = expr },
        .goto => |target| .{ .goto = target },
        .labeled_stmt => |ls| .{ .labeled_stmt = .{
            .label = ls.label,
            .stmt = self.onHeap(self.statement(ls.stmt.*)),
        } },
        .@"if" => |_if| .{ .@"if" = .{
            .cond = _if.cond,
            .then = self.onHeap(self.statement(_if.then.*)),
            .or_else = if (_if.or_else) |or_else| self.onHeap(self.statement(or_else.*)) else null,
        } },
        .compound => |blk| .{ .compound = self.block(blk) },
        .@"break" => .{ .@"break" = if (self.currentLabel()) |lbl| lbl else lbl: {
            self.fail(.break_stmt_outside_of_loop);
            break :lbl "";
        } },
        .@"continue" => .{ .@"continue" = if (self.currentLabel()) |lbl| lbl else lbl: {
            self.fail(.continue_stmt_outside_of_loop);
            break :lbl "";
        } },
        .@"while" => |_while| stmt: {
            const lbl = self.pushLabel("while");
            defer self.popLabel();

            break :stmt .{ .@"while" = .{
                .cond = _while.cond,
                .body = self.onHeap(self.statement(_while.body.*)),
                .label = lbl,
            } };
        },
        .do_while => |do_while| stmt: {
            const lbl = self.pushLabel("do_while");
            defer self.popLabel();

            break :stmt .{ .do_while = .{
                .cond = do_while.cond,
                .body = self.onHeap(self.statement(do_while.body.*)),
                .label = lbl,
            } };
        },
        .@"for" => |_for| stmt: {
            const lbl = self.pushLabel("while");
            defer self.popLabel();

            break :stmt .{ .@"for" = .{
                .init = switch (_for.init) {
                    .decl => |decl| .{ .decl = self.onHeap(decl.*) },
                    .expr => |expr| .{ .expr = expr },
                },
                .cond = _for.cond,
                .post = _for.post,
                .body = self.onHeap(self.statement(_for.body.*)),
                .label = lbl,
            } };
        },
    };
}

const onHeap = @import("../utils.zig").onHeap;

fn fail(self: *LoopLabel, err: ErrorVariant) void {
    self.errs.append(err) catch @panic("OOM");
}

fn makeUniqueLabel(self: *LoopLabel, suffix: []const u8) []const u8 {
    defer self.counter += 1;
    return std.fmt.allocPrint(self.allocator, ".loop.{s}.{}", .{
        suffix,
        self.counter,
    }) catch @panic("OOM");
}

fn currentLabel(self: *LoopLabel) ?[]const u8 {
    return self.labels_stack.getLastOrNull();
}

fn pushLabel(self: *LoopLabel, str: []const u8) []const u8 {
    const lbl = self.makeUniqueLabel(str);
    self.labels_stack.append(lbl) catch @panic("OOM");
    return lbl;
}

fn popLabel(self: *LoopLabel) void {
    _ = self.labels_stack.pop();
}
