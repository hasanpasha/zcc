const std = @import("std");
const vir = @import("vir.zig");
const ast = @import("../ast.zig");
const Result = @import("../result.zig").Result;
const Location = @import("../location.zig").Location;
const oneOf = @import("../utils.zig").oneOf;

pub const Environment = struct {
    parent: ?*Environment = null,
    variables: std.StringHashMap([]const u8),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, parent: ?*Environment) *Environment {
        const self = allocator.create(Environment) catch @panic("OOM");
        self.* = .{
            .parent = parent,
            .variables = .init(allocator),
            .allocator = allocator,
        };
        return self;
    }

    pub fn deinit(self: *Environment) void {
        self.variables.deinit();
        self.allocator.destroy(self);
    }

    pub fn getCur(self: *Environment, name: []const u8) ?[]const u8 {
        return self.variables.get(name);
    }

    pub fn getRec(self: *Environment, name: []const u8) ?[]const u8 {
        return self.getCur(name) orelse
            return if (self.parent) |parent| parent.getRec(name) else null;
    }

    pub fn put(self: *Environment, name: []const u8, unique: []const u8) void {
        self.variables.put(name, unique) catch @panic("OOM");
    }
};

allocator: std.mem.Allocator,
variables: *Environment,
errs: std.array_list.Managed(ErrorItem),
counter: usize = 0,

const VariableResolution = @This();

const ErrorVariant = union(enum) {
    duplicate: []const u8,
    invalid_lvalue,
    undeclared: []const u8,

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self) {
            .duplicate => |name| try writer.print("duplicate variable '{s}'", .{name}),
            .invalid_lvalue => try writer.writeAll("invalid lvalue"),
            .undeclared => |name| try writer.print("undeclared variable '{s}'", .{name}),
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

pub fn resolve(in: ast.Program, allocator: std.mem.Allocator) Result(vir.Program, Error) {
    var arena = std.heap.ArenaAllocator.init(allocator);

    var self: VariableResolution = .{
        .allocator = arena.allocator(),
        .variables = .init(allocator, null),
        .errs = .init(allocator),
    };
    defer self.variables.deinit();

    const main_func = self.function(in.main_function);

    if (self.errs.items.len > 0) {
        return .Err(.{ .errs = self.errs });
    }

    return .Ok(.{ .main_function = main_func, .arena = arena });
}

fn function(self: *VariableResolution, func: ast.Function) vir.Function {
    return .{
        .name = func.name.name,
        .body = self.block(func.body),
    };
}

fn block(self: *VariableResolution, blk: ast.Block) vir.Block {
    self.variables = .init(self.allocator, self.variables);
    defer {
        const parent = self.variables.parent;
        self.variables.deinit();
        if (parent) |p|
            self.variables = p;
    }

    var items = self.allocator.alloc(vir.BlockItem, blk.items.len) catch @panic("OOM");
    for (blk.items, 0..) |item, i|
        items[i] = self.blockItem(item);
    return .{ .items = items };
}

fn blockItem(self: *VariableResolution, item: ast.BlockItem) vir.BlockItem {
    return switch (item) {
        .declaration => |decl| .{ .declaration = self.declaration(decl) },
        .statement => |stmt| .{ .statement = self.statement(stmt) },
    };
}

fn declaration(self: *VariableResolution, decl: ast.Declaration) vir.Declaration {
    return switch (decl) {
        .variable => |variable| _var: {
            const name = variable.name.name;
            const loc = variable.name.location;

            if (self.variables.getCur(name) != null)
                self.fail(.{
                    .err = .{ .duplicate = name },
                    .loc = loc,
                });

            const unique_name = self.makeUniqueName(name);
            self.variables.put(name, unique_name);

            var init: ?vir.Expression = null;
            if (variable.init) |_init|
                init = self.expression(_init);

            break :_var .{ .variable = .{
                .name = unique_name,
                .init = init,
            } };
        },
    };
}

fn statement(self: *VariableResolution, stmt: ast.Statement) vir.Statement {
    return switch (stmt) {
        .null => .null,
        .@"return" => |expr| .{ .@"return" = self.expression(expr) },
        .expr => |expr| .{ .expr = self.expression(expr) },
        .@"if" => |_if| .{
            .@"if" = .{
                .cond = self.expression(_if.cond),
                .then = self.onHeap(self.statement(_if.then.*)),
                .or_else = if (_if.or_else) |s| self.onHeap(self.statement(s.*)) else null,
            },
        },
        .goto => |target| .{ .goto = target },
        .labeled_stmt => |ls| .{ .labeled_stmt = .{
            .label = ls.label,
            .stmt = self.onHeap(self.statement(ls.stmt.*)),
        } },
        .compound => |blk| .{
            .compound = self.block(blk),
        },
    };
}

fn expression(self: *VariableResolution, expr: ast.Expression) vir.Expression {
    return switch (expr) {
        .int_lit => |lit| .{ .int_lit = lit.value },
        .binary => |binary| .{ .binary = .{
            .operator = binary.operator.@"0",
            .lhs = self.onHeap(self.expression(binary.lhs.*)),
            .rhs = self.onHeap(self.expression(binary.rhs.*)),
        } },
        .lhs_unary => |unary| u: {
            if (oneOf(unary.operator.@"0", &.{ .plus_plus, .minus_minus }) and unary.rhs.* != .variable)
                self.fail(.{
                    .err = .invalid_lvalue,
                    .loc = locateExpr(unary.rhs.*),
                });

            break :u .{ .lhs_unary = .{
                .operator = unary.operator.@"0",
                .rhs = self.onHeap(self.expression(unary.rhs.*)),
            } };
        },
        .rhs_unary => |unary| u: {
            if (unary.lhs.* != .variable)
                self.fail(.{
                    .err = .invalid_lvalue,
                    .loc = locateExpr(unary.lhs.*),
                });

            break :u .{ .rhs_unary = .{
                .operator = unary.operator.@"0",
                .lhs = self.onHeap(self.expression(unary.lhs.*)),
            } };
        },
        .assignment => |assignment| ass: {
            if (assignment.lhs.* != .variable)
                self.fail(.{
                    .err = .invalid_lvalue,
                    .loc = locateExpr(assignment.lhs.*),
                });

            break :ass .{ .assignment = .{
                .operator = assignment.operator.@"0",
                .lhs = self.onHeap(self.expression(assignment.lhs.*)),
                .rhs = self.onHeap(self.expression(assignment.rhs.*)),
            } };
        },
        .variable => |_var| _v: {
            var unique_name: []const u8 = undefined;
            if (self.variables.getRec(_var.name) == null) {
                self.fail(.{
                    .err = .{ .undeclared = _var.name },
                    .loc = _var.location,
                });
            } else {
                unique_name = self.variables.getRec(_var.name).?;
            }
            break :_v .{ .variable = unique_name };
        },
        .conditional => |conditional| .{ .conditional = .{
            .cond = self.onHeap(self.expression(conditional.cond.*)),
            .if_true = self.onHeap(self.expression(conditional.if_true.*)),
            .if_false = self.onHeap(self.expression(conditional.if_false.*)),
        } },
    };
}

const onHeap = @import("../utils.zig").onHeap;

fn fail(self: *VariableResolution, err: ErrorItem) void {
    self.errs.append(err) catch @panic("OOM");
}

fn makeUniqueName(self: *VariableResolution, suffix: []const u8) []u8 {
    defer self.counter += 1;
    return std.fmt.allocPrint(self.allocator, "var.{s}.{}", .{
        suffix,
        self.counter,
    }) catch @panic("OOM");
}

fn locateExpr(expr: ast.Expression) Location {
    return switch (expr) {
        .int_lit => |lit| lit.location,
        .binary => |b| locateExpr(b.lhs.*),
        .lhs_unary => |u| u.operator.@"1",
        .rhs_unary => |u| u.operator.@"1",
        .assignment => |ass| locateExpr(ass.lhs.*),
        .variable => |va| va.location,
        .conditional => |cond| locateExpr(cond.cond.*),
    };
}
