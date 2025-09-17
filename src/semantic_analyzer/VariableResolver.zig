const std = @import("std");
const AllocErr = std.mem.Allocator.Error;
const Writer = std.Io.Writer;
const AST = @import("../AST.zig");
const Result = @import("../result.zig").Result;
const Location = @import("../Location.zig");
const oneOf = @import("../utils.zig").oneOf;

pub const Environment = struct {
    parent: ?*Environment,
    variables: std.StringHashMap([]const u8),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, parent: ?*Environment) AllocErr!*Environment {
        const self = try allocator.create(Environment);
        self.* = .{
            .parent = parent,
            .variables = .init(allocator),
            .allocator = allocator,
        };
        return self;
    }

    pub fn count(self: *Environment) u32 {
        return self.variables.count();
    }

    pub fn isGLobal(self: *Environment) bool {
        return self.parent == null;
    }

    pub fn getCur(self: *Environment, name: []const u8) ?[]const u8 {
        return self.variables.get(name);
    }

    pub fn getRec(self: *Environment, name: []const u8) ?[]const u8 {
        return self.getCur(name) orelse
            if (self.parent) |parent| parent.getRec(name) else null;
    }

    pub fn put(self: *Environment, name: []const u8, unique: []const u8) AllocErr!void {
        try self.variables.put(name, unique);
    }
};

allocator: std.mem.Allocator,
variables: *Environment,
counter: *usize,

const VariableResolver = @This();

pub fn resolve(in: *AST, glob_counter: *usize) AllocErr!void {
    var arena = in.arena;

    var self: VariableResolver = .{
        .allocator = arena.allocator(),
        .variables = try .init(arena.allocator(), null),
        .counter = glob_counter,
    };

    for (in.declarations) |*ldecl|
        _ = try self.declaration(&ldecl.@"0");
}

fn block(self: *VariableResolver, blk: *AST.Block) AllocErr!*AST.Block {
    try self.pushEnv(null);
    defer self.popEnv();

    for (blk.items) |*item|
        _ = try self.blockItem(item);

    return blk;
}

fn blockItem(self: *VariableResolver, item: *AST.BlockItem) AllocErr!*AST.BlockItem {
    item.* = switch (item.*) {
        .err => item.*,
        .declaration => |decl| .{ .declaration = try self.declaration(decl) },
        .statement => |*stmt| .{ .statement = (try self.statement(stmt)).* },
    };
    return item;
}

fn declaration(self: *VariableResolver, decl: *AST.Declaration) AllocErr!*AST.Declaration {
    decl.* = switch (decl.*) {
        .err => decl.*,
        .variable => |*variable| _var: {
            const name = variable.name.name;

            if (self.variables.getCur(name) != null)
                break :_var .{ .err = .{ .duplicate = name } };

            const unique_name = try self.makeUniqueName(name);
            std.log.debug("putting: {s} as {s}", .{ name, unique_name });
            try self.variables.put(name, unique_name);

            var init: ?AST.Expression = null;
            if (variable.init) |*_init|
                init = (try self.expression(_init)).*;

            break :_var .{ .variable = .{
                .name = variable.name.applyUniqueName(try self.allocator.dupe(u8, unique_name)),
                .init = init,
            } };
        },
        .function => |*func| _func: {
            try self.pushEnv(null);
            defer self.popEnv();

            break :_func .{ .function = .{
                .name = func.name,
                .body = (try self.block(&func.body)).*,
            } };
        },
    };
    return decl;
}

fn statement(self: *VariableResolver, stmt: *AST.Statement) AllocErr!*AST.Statement {
    stmt.* = switch (stmt.*) {
        .err, .null, .@"break", .@"continue" => stmt.*,
        .@"return" => |*expr| .{ .@"return" = (try self.expression(expr)).* },
        .expr => |*expr| .{ .expr = (try self.expression(expr)).* },
        .@"if" => |*_if| .{ .@"if" = .{
            .cond = (try self.expression(&_if.cond)).*,
            .then = try self.statement(_if.then),
            .or_else = if (_if.or_else) |s| try self.statement(s) else null,
        } },
        .goto => |target| .{ .goto = target },
        .labeled_stmt => |*ls| .{ .labeled_stmt = .{
            .label = ls.label,
            .stmt = try self.statement(ls.stmt),
        } },
        .compound => |*blk| .{ .compound = (try self.block(blk)).* },
        .@"while" => |*_while| .{ .@"while" = .{
            .cond = (try self.expression(&_while.cond)).*,
            .body = try self.statement(_while.body),
        } },
        .do_while => |*do_while| .{ .do_while = .{
            .body = try self.statement(do_while.body),
            .cond = (try self.expression(&do_while.cond)).*,
        } },
        .@"for" => |*_for| stmt: {
            try self.pushEnv(null);
            defer self.popEnv();

            break :stmt .{ .@"for" = .{
                .init = switch (_for.init) {
                    .decl => |decl| .{ .decl = try self.declaration(decl) },
                    .expr => |*expr| .{ .expr = if (expr.*) |*exp| (try self.expression(exp)).* else null },
                },
                .cond = if (_for.cond) |*cond| (try self.expression(cond)).* else null,
                .post = if (_for.post) |*post| (try self.expression(post)).* else null,
                .body = try self.statement(_for.body),
            } };
        },
        .@"switch" => |*_switch| .{ .@"switch" = .{
            .cond = (try self.expression(&_switch.cond)).*,
            .body = try self.statement(_switch.body),
        } },
        .case => |*case| .{ .case = .{
            .expr = (try self.expression(&case.expr)).*,
            .stmt = if (case.stmt) |_stmt| try self.statement(_stmt) else null,
        } },
        .default => |*default| .{ .default = .{
            .stmt = if (default.stmt) |_stmt| try self.statement(_stmt) else null,
        } },
    };
    return stmt;
}

fn expression(self: *VariableResolver, expr: *AST.Expression) AllocErr!*AST.Expression {
    expr.* = switch (expr.*) {
        .err => expr.*,
        .int_lit => |lit| .{ .int_lit = lit },
        .binary => |binary| .{ .binary = .{
            .operator = binary.operator,
            .lhs = try self.expression(binary.lhs),
            .rhs = try self.expression(binary.rhs),
        } },
        .lhs_unary => |unary| u: {
            if (oneOf(unary.operator.@"0", &.{ .plus_plus, .minus_minus }) and unary.rhs.* != .variable)
                break :u .{ .err = .invalid_lvalue };

            break :u .{ .lhs_unary = .{
                .operator = unary.operator,
                .rhs = try self.expression(unary.rhs),
            } };
        },
        .rhs_unary => |unary| u: {
            if (unary.lhs.* != .variable)
                break :u .{ .err = .invalid_lvalue };

            break :u .{ .rhs_unary = .{
                .operator = unary.operator,
                .lhs = try self.expression(unary.lhs),
            } };
        },
        .assignment => |assignment| ass: {
            if (assignment.lhs.* != .variable)
                break :ass .{ .err = .invalid_lvalue };

            break :ass .{ .assignment = .{
                .operator = assignment.operator,
                .lhs = try self.expression(assignment.lhs),
                .rhs = try self.expression(assignment.rhs),
            } };
        },
        .variable => |*_var| _v: {
            if (self.variables.getRec(_var.name)) |unique| {
                break :_v .{ .variable = _var.applyUniqueName(try self.dupe(unique)) };
            }

            break :_v .{ .err = .{ .undeclared = _var.name } };
        },
        .conditional => |conditional| .{ .conditional = .{
            .cond = try self.expression(conditional.cond),
            .if_true = try self.expression(conditional.if_true),
            .if_false = try self.expression(conditional.if_false),
        } },
    };
    return expr;
}

const onHeap = @import("../utils.zig").onHeap;

fn makeUniqueName(self: *VariableResolver, suffix: []const u8) AllocErr![]u8 {
    std.log.debug("new counter: {}", .{self.counter.*});
    defer self.counter.* += 1;
    return std.fmt.allocPrint(self.allocator, "var.{s}.{}", .{
        suffix,
        self.counter.*,
    });
}

// fn locateExpr(expr: AST.Expression) Location {
//     return switch (expr) {
//         .int_lit => |lit| lit.location,
//         .binary => |b| locateExpr(b.lhs.*),
//         .lhs_unary => |u| u.operator.@"1",
//         .rhs_unary => |u| u.operator.@"1",
//         .assignment => |ass| locateExpr(ass.lhs.*),
//         .variable => |va| va.location,
//         .conditional => |cond| locateExpr(cond.cond.*),
//     };
// }

pub fn pushEnv(self: *VariableResolver, env: ?*Environment) AllocErr!void {
    self.variables = if (env) |e| e else try .init(self.allocator, self.variables);
}

pub fn popEnv(self: *VariableResolver) void {
    if (self.variables.parent) |parent|
        self.variables = parent;
}

pub fn dupe(self: *VariableResolver, str: []const u8) AllocErr![]const u8 {
    return try self.allocator.dupe(u8, str);
}
