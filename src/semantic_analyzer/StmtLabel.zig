const std = @import("std");
const AllocErr = std.mem.Allocator.Error;

const AST = @import("../AST.zig");

const StmtLabel = @This();

pub const ErrorVariant = union(enum) {
    break_stmt_outside_of_loop_or_switch,
    continue_stmt_outside_of_loop,
    multiple_defaults,
    case_stmt_outside_of_switch,
    default_stmt_outside_of_switch,
    multiple_case_stmts_have_same_constant_expr,
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
labels: Array(Label),
cases: Array(*Array(AST.Statement.Case)),
defaults: Array(*Array(AST.Statement.Default)),
counter: *usize,

pub fn label(in: *AST, glob_counter: *usize) AllocErr!void {
    const allocator = in.arena.allocator();

    var self = StmtLabel{
        .allocator = allocator,
        .labels = .init(allocator),
        .cases = .init(allocator),
        .defaults = .init(allocator),
        .counter = glob_counter,
    };
    defer self.labels.deinit();

    for (in.declarations) |*ldecl| {
        var decl, _ = ldecl.*;
        if (decl == .function) {
            try self.function(&decl.function);
        }
    }
}

fn function(self: *StmtLabel, func: *AST.Declaration.Function) AllocErr!void {
    std.log.debug("function {f}", .{func});
    _ = try self.block(&func.body);
}

fn block(self: *StmtLabel, blk: *AST.Block) AllocErr!*AST.Block {
    for (blk.items) |*item| {
        if (item.* == .statement) {
            std.log.debug("STMT BEFORE {f}", .{item.*.statement});
            _ = try self.statement(&item.statement);
            std.log.debug("STMT AFTER {f}", .{item.*.statement});
        }
        if (item.* == .declaration) {
            std.log.debug("vardecl: {f}", .{item.*.declaration.*.variable.name});
        }
    }

    return blk;
}

fn statement(self: *StmtLabel, stmt: *AST.Statement) AllocErr!*AST.Statement {
    stmt.* = switch (stmt.*) {
        .null, .@"return", .expr, .goto, .err => stmt.*,
        .labeled_stmt => |ls| .{ .labeled_stmt = .{
            .label = ls.label,
            .stmt = try self.statement(ls.stmt),
        } },
        .@"if" => |_if| .{ .@"if" = .{
            .cond = _if.cond,
            .then = try self.statement(_if.then),
            .or_else = if (_if.or_else) |or_else| try self.statement(or_else) else null,
        } },
        .compound => |*blk| .{ .compound = (try self.block(blk)).* },
        .@"break" => if (self.currentLabel()) |lbl|
            .{ .@"break" = lbl }
        else
            .{ .err = .break_stmt_outside_of_loop_or_switch },
        .@"continue" => if (self.getTopClassLabelOrNull(.loop)) |lbl|
            .{ .@"continue" = lbl }
        else
            .{ .err = .continue_stmt_outside_of_loop },
        .@"while" => |_while| stmt: {
            const lbl = try self.pushLabel(.@"while");
            defer self.popLabel();

            break :stmt .{ .@"while" = .{
                .cond = _while.cond,
                .body = try self.statement(_while.body),
                .label = lbl,
            } };
        },
        .do_while => |do_while| stmt: {
            const lbl = try self.pushLabel(.do_while);
            defer self.popLabel();

            break :stmt .{ .do_while = .{
                .cond = do_while.cond,
                .body = try self.statement(do_while.body),
                .label = lbl,
            } };
        },
        .@"for" => |_for| stmt: {
            const lbl = try self.pushLabel(.@"for");
            defer self.popLabel();

            break :stmt .{ .@"for" = .{
                .init = switch (_for.init) {
                    .decl => |decl| .{ .decl = decl },
                    .expr => |expr| .{ .expr = expr },
                },
                .cond = _for.cond,
                .post = _for.post,
                .body = try self.statement(_for.body),
                .label = lbl,
            } };
        },
        .@"switch" => |_switch| stmt: {
            const lbl = try self.pushLabel(.@"switch");
            defer self.popLabel();

            const body = try self.statement(_switch.body);

            var cases_arr = self.cases.getLast();
            var defaults_arr = self.defaults.getLast();

            const cases = try cases_arr.toOwnedSlice();
            const defaults = try defaults_arr.toOwnedSlice();

            for (cases, 0..) |case, i| {
                for (cases, 0..) |xcase, j| {
                    if (case.expr.int_lit.value == xcase.expr.int_lit.value and i != j) {
                        break :stmt .{ .err = .multiple_case_stmts_have_same_constant_expr };
                    }
                }
            }

            if (defaults.len > 1)
                break :stmt .{ .err = .multiple_defaults };

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
            const new_case: AST.Statement.Case = .{
                .expr = case.expr,
                .stmt = if (case.stmt) |_stmt| try self.statement(_stmt) else null,
                .label = try self.makeUniqueLabel(self.getTopClassLabelOrNull(.@"switch") orelse {
                    break :stmt .{ .err = .case_stmt_outside_of_switch };
                }),
            };

            if (self.cases.getLastOrNull()) |last|
                try last.append(new_case);

            break :stmt .{ .case = new_case };
        },
        .default => |default| stmt: {
            const new_default: AST.Statement.Default = .{
                .stmt = if (default.stmt) |_stmt| try self.statement(_stmt) else null,
                .label = try self.makeUniqueLabel(self.getTopClassLabelOrNull(.@"switch") orelse {
                    break :stmt .{ .err = .default_stmt_outside_of_switch };
                }),
            };
            if (self.defaults.getLastOrNull()) |last|
                try last.append(new_default);

            break :stmt .{ .default = new_default };
        },
    };

    return stmt;
}

fn makeUniqueLabel(self: *StmtLabel, suffix: []const u8) AllocErr![]const u8 {
    std.log.debug("stmtlabel counter: {}", .{self.counter.*});
    defer self.counter.* += 1;
    return try std.fmt.allocPrint(self.allocator, ".{s}.{}", .{
        suffix,
        self.counter.*,
    });
}

fn currentLabel(self: *StmtLabel) ?[]const u8 {
    return if (self.labels.getLastOrNull()) |lbl| lbl.name else null;
}

fn pushLabel(self: *StmtLabel, variant: @Type(.enum_literal)) AllocErr![]const u8 {
    const lbl_name = try self.makeUniqueLabel(@tagName(variant));
    const lbl_class: LabelClass = if (variant == .@"switch") .@"switch" else .loop;

    try self.labels.append(.{
        .name = lbl_name,
        .class = lbl_class,
    });

    if (lbl_class == .@"switch") {
        const cases_ptr = try self.allocator.create(Array(AST.Statement.Case));
        const defaults_ptr = try self.allocator.create(Array(AST.Statement.Default));

        cases_ptr.* = Array(AST.Statement.Case).init(self.allocator);
        defaults_ptr.* = Array(AST.Statement.Default).init(self.allocator);

        try self.cases.append(cases_ptr);
        try self.defaults.append(defaults_ptr);
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
