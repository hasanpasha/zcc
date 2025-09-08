const std = @import("std");

const LIR = @import("LIR.zig");

const PIR = @import("PIR.zig");
const Location = @import("../Location.zig");
const Result = @import("../result.zig").Result;

const StmtLabel = @This();

pub const ErrorVariant = union(enum) {
    break_stmt_outside_of_loop_or_switch,
    continue_stmt_outside_of_loop,
    multiple_defaults,
    case_stmt_outside_of_switch,
    default_stmt_outside_of_switch,
    multiple_case_stmts_have_same_constant_expr,
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
            try writer.print("{}: {s}\n", .{ i, @tagName(err) });
        }
    }
};

const Array = std.array_list.Managed;

const LabelClass = enum {
    loop,
    @"switch",
};

const Label = struct {
    class: LabelClass,
    name: []const u8,
};

allocator: std.mem.Allocator,
errs: Array(ErrorVariant),
labels: Array(Label),
cases: Array(*Array(PIR.Statement.Case)),
defaults: Array(*Array(PIR.Statement.Default)),
counter: usize = 0,

pub fn label(in: LIR, allocator: std.mem.Allocator) Result(PIR, Error) {
    var arena = std.heap.ArenaAllocator.init(allocator);

    var self = StmtLabel{
        .allocator = arena.allocator(),
        .labels = .init(allocator),
        .cases = .init(allocator),
        .defaults = .init(allocator),
        .errs = .init(allocator),
    };
    defer self.labels.deinit();

    const main = self.function(in.main_function);

    if (self.errs.items.len > 0) {
        return .Err(.{ .errs = self.errs });
    }

    return .Ok(.{ .main_function = main, .arena = arena });
}

fn function(self: *StmtLabel, in: LIR.Function) PIR.Function {
    const body = self.block(in.body);

    return .{ .name = in.name, .body = body };
}

fn block(self: *StmtLabel, in: LIR.Block) PIR.Block {
    var items = self.allocator.alloc(PIR.BlockItem, in.items.len) catch @panic("OOM");
    for (in.items, 0..) |item, i|
        items[i] = switch (item) {
            .declaration => |decl| .{ .declaration = decl },
            .statement => |stmt| .{ .statement = self.statement(stmt) },
        };

    return .{ .items = items };
}

fn statement(self: *StmtLabel, in: LIR.Statement) PIR.Statement {
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
        .@"break" => .{
            .@"break" = if (self.currentLabel()) |lbl| lbl else lbl: {
                self.fail(.break_stmt_outside_of_loop_or_switch);
                break :lbl "";
            },
        },
        .@"continue" => .{
            .@"continue" = if (self.getTopClassLabelOrNull(.loop)) |lbl| lbl else lbl: {
                self.fail(.continue_stmt_outside_of_loop);
                break :lbl "";
            },
        },
        .@"while" => |_while| stmt: {
            const lbl = self.pushLabel(.@"while");
            defer self.popLabel();

            break :stmt .{ .@"while" = .{
                .cond = _while.cond,
                .body = self.onHeap(self.statement(_while.body.*)),
                .label = lbl,
            } };
        },
        .do_while => |do_while| stmt: {
            const lbl = self.pushLabel(.do_while);
            defer self.popLabel();

            break :stmt .{ .do_while = .{
                .cond = do_while.cond,
                .body = self.onHeap(self.statement(do_while.body.*)),
                .label = lbl,
            } };
        },
        .@"for" => |_for| stmt: {
            const lbl = self.pushLabel(.@"for");
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
        .@"switch" => |_switch| stmt: {
            const lbl = self.pushLabel(.@"switch");
            defer self.popLabel();

            const body = self.onHeap(self.statement(_switch.body.*));

            const cases = self.cases.getLast().toOwnedSlice() catch @panic("OOM");
            const defaults = self.defaults.getLast().toOwnedSlice() catch @panic("OOM");

            for (cases, 0..) |case, i| {
                for (cases, 0..) |xcase, j| {
                    if (case.expr.int_lit == xcase.expr.int_lit and i != j) {
                        self.fail(.multiple_case_stmts_have_same_constant_expr);
                        break;
                    }
                }
            }

            if (defaults.len > 1)
                self.fail(.multiple_defaults);

            const default = if (defaults.len > 0) defaults[0] else null;

            break :stmt .{ .@"switch" = .{
                .cond = _switch.cond,
                .body = body,
                .label = lbl,
                .cases = cases,
                .default = default,
            } };
        },
        .case => |case| stmt: {
            const new_case: PIR.Statement.Case = .{
                .expr = case.expr,
                .stmt = if (case.stmt) |_stmt| self.onHeap(self.statement(_stmt.*)) else null,
                .label = self.makeUniqueLabel(self.getTopClassLabelOrNull(.@"switch") orelse lbl: {
                    self.fail(.case_stmt_outside_of_switch);
                    break :lbl "";
                }),
            };
            if (self.cases.getLastOrNull()) |last| {
                last.append(new_case) catch @panic("OOM");
            }
            break :stmt .{ .case = new_case };
        },
        .default => |default| stmt: {
            const new_default: PIR.Statement.Default = .{
                .stmt = if (default.stmt) |_stmt| self.onHeap(self.statement(_stmt.*)) else null,
                .label = self.makeUniqueLabel(self.getTopClassLabelOrNull(.@"switch") orelse lbl: {
                    self.fail(.default_stmt_outside_of_switch);
                    break :lbl "";
                }),
            };
            if (self.defaults.getLastOrNull()) |last| {
                last.append(new_default) catch @panic("OOM");
            }
            break :stmt .{ .default = new_default };
        },
    };
}

const onHeap = @import("../utils.zig").onHeap;

fn fail(self: *StmtLabel, err: ErrorVariant) void {
    self.errs.append(err) catch @panic("OOM");
}

fn makeUniqueLabel(self: *StmtLabel, suffix: []const u8) []const u8 {
    defer self.counter += 1;
    return std.fmt.allocPrint(self.allocator, ".{s}.{}", .{
        suffix,
        self.counter,
    }) catch @panic("OOM");
}

fn currentLabel(self: *StmtLabel) ?[]const u8 {
    return if (self.labels.getLastOrNull()) |lbl| lbl.name else null;
}

fn pushLabel(self: *StmtLabel, variant: @Type(.enum_literal)) []const u8 {
    const lbl_name = self.makeUniqueLabel(@tagName(variant));
    const lbl_class: LabelClass = if (variant == .@"switch") .@"switch" else .loop;

    self.labels.append(.{
        .name = lbl_name,
        .class = lbl_class,
    }) catch @panic("OOM");

    if (lbl_class == .@"switch") {
        self.cases.append(self.onHeap(Array(PIR.Statement.Case).init(self.allocator))) catch @panic("OOM");
        self.defaults.append(self.onHeap(Array(PIR.Statement.Default).init(self.allocator))) catch @panic("OOM");
    }

    return lbl_name;
}

fn popLabel(self: *StmtLabel) void {
    if (self.labels.pop()) |lbl| {
        if (lbl.class == .@"switch") {
            _ = self.cases.pop();
            _ = self.defaults.pop();
        }
    }
}

fn getTopClassLabelOrNull(self: *StmtLabel, class: LabelClass) ?[]const u8 {
    var i = self.labels.items.len;
    while (i > 0) {
        i -= 1;
        const lbl = self.labels.items[i];
        if (lbl.class == class) return lbl.name;
    }
    return null;
}
