const std = @import("std");
const Writer = std.Io.Writer;
const WriterErr = Writer.Error;
const Allocator = std.mem.Allocator;
const AllocErr = Allocator.Error;

const onHeap = @import("utils.zig").onHeap;

pub const AST = @import("AST.zig");
pub const PIR = @import("PIR.zig");

allocator: Allocator,
writer: *Writer,
errors_count: usize = 0,

const Stripper = @This();

pub const Error = error{has_errors} || AllocErr || WriterErr;
const InternalError = AllocErr || WriterErr || error{encounterd_error};

pub fn strip(in: AST, allocator: Allocator, writer: *Writer) Error!PIR {
    var arena = std.heap.ArenaAllocator.init(allocator);

    var self = Stripper{
        .allocator = arena.allocator(),
        .writer = writer,
    };

    var decl_arr = try std.ArrayList(PIR.Declaration).initCapacity(arena.allocator(), in.declarations.len);
    for (in.declarations) |ldecl| {
        const decl = ldecl.@"0";
        const pdecl = self.declaration(decl) catch |err| {
            if (err == InternalError.encounterd_error) continue;
            return @errorCast(err);
        };
        try decl_arr.append(arena.allocator(), pdecl);
    }

    if (self.errors_count > 0) {
        try self.writer.print("encountered {} errors\n", .{self.errors_count});
        try self.writer.flush();
        return error.has_errors;
    }

    const decls = try decl_arr.toOwnedSlice(arena.allocator());

    return .{
        .declarations = decls,
        .arena = arena,
    };
}

fn handleErr(self: *Stripper, err: AST.Error) InternalError {
    self.errors_count += 1;
    switch (err) {
        .eoi => |loc| try self.writer.print("unexpected end of input at {f}", .{
            std.fmt.alt(loc, .readableFmt),
        }),
        .unexpected_token => |tk| try self.writer.print("unexpected token, expeected: {}", .{tk}),
        .expected_type => try self.writer.writeAll("expected declaration type"),
        .invalid_lvalue => try self.writer.writeAll("invalid lvalue"),
        .missing_label => try self.writer.writeAll("missing label"),
        .missing_semi => try self.writer.writeAll("missing semicolon"),
        .missing_while_part => try self.writer.writeAll("missing while part of do-while statement"),
        .duplicate => |identifier| try self.writer.print("duplicate identifier \"{s}\"", .{identifier}),
        .undeclared => |identifier| try self.writer.print("\"{s}\" is undeclared", .{identifier}),
        .lexer_error => |lerr| try self.writer.print("lexer error {f}", .{lerr}),
        .expected_operator => |kind| try self.writer.print("expected {s} expression operator", .{@tagName(kind)}),
        .break_stmt_outside_of_loop_or_switch => try self.writer.writeAll("break stmt outside of loop or switch"),
        .case_stmt_outside_of_switch => try self.writer.writeAll("case stmt outside of switch"),
        .continue_stmt_outside_of_loop => try self.writer.writeAll("continue stmt outside of loop"),
        .default_stmt_outside_of_switch => try self.writer.writeAll("default stmt outside of switch"),
        .multiple_case_stmts_have_same_constant_expr => try self.writer.writeAll("multiple case stmts have same constant expr"),
        .multiple_defaults => try self.writer.writeAll("multiple default statements"),
        .token_not_operator => |tok| try self.writer.print("token at {f} is not expression operator: {f}", .{
            std.fmt.alt(tok.span.@"0", .readableFmt),
            tok.variant,
        }),
    }
    try self.writer.writeByte('\n');
    return InternalError.encounterd_error;
}

fn declaration(self: *Stripper, decl: AST.Declaration) InternalError!PIR.Declaration {
    return switch (decl) {
        .err => |err| return self.handleErr(err),
        .function => |func| .{ .function = .{
            .name = try self.dupe(func.name.name),
            .body = try self.block(func.body),
        } },
        .variable => |_var| .{ .variable = .{
            .name = name: {
                std.log.debug("duping name: {s}", .{_var.name.unique_name});
                break :name try self.dupe(_var.name.unique_name);
            },
            .init = if (_var.init) |init| try self.expression(init) else null,
        } },
    };
}

fn block(self: *Stripper, blk: AST.Block) InternalError!PIR.Block {
    var items_arr = try std.ArrayList(PIR.BlockItem).initCapacity(self.allocator, blk.items.len);
    for (blk.items) |item| {
        const pitem = self.blockItem(item) catch |err| {
            if (err == InternalError.encounterd_error) continue;
            return @errorCast(err);
        };
        try items_arr.append(self.allocator, pitem);
    }

    const items = try items_arr.toOwnedSlice(self.allocator);

    return .{ .items = items };
}

fn blockItem(self: *Stripper, item: AST.BlockItem) InternalError!PIR.BlockItem {
    return switch (item) {
        .err => |err| return self.handleErr(err),
        .declaration => |decl| .{ .declaration = try self.onHeap(try self.declaration(decl.*)) },
        .statement => |stmt| .{ .statement = try self.statement(stmt) },
    };
}

fn statement(self: *Stripper, stmt: AST.Statement) InternalError!PIR.Statement {
    return switch (stmt) {
        .err => |err| return self.handleErr(err),
        .@"return" => |expr| .{ .@"return" = try self.expression(expr) },
        .expr => |expr| .{ .expr = try self.expression(expr) },
        .null => .null,
        .@"if" => |_if| .{ .@"if" = .{
            .cond = try self.expression(_if.cond),
            .then = try self.onHeap(try self.statement(_if.then.*)),
            .or_else = if (_if.or_else) |or_else|
                try self.onHeap(try self.statement(or_else.*))
            else
                null,
        } },
        .compound => |blk| .{ .compound = try self.block(blk) },
        .goto => |target| .{ .goto = target.unique_name },
        .labeled_stmt => |lst| .{ .labeled_stmt = .{
            .label = lst.label.unique_name,
            .stmt = try self.onHeap(try self.statement(lst.stmt.*)),
        } },
        .@"break" => |target| .{ .@"break" = target },
        .@"continue" => |target| .{ .@"continue" = target },
        .@"while" => |_while| .{ .@"while" = .{
            .cond = try self.expression(_while.cond),
            .body = try self.onHeap(try self.statement(_while.body.*)),
            .label = try self.dupe(_while.label),
        } },
        .do_while => |do_while| .{ .do_while = .{
            .cond = try self.expression(do_while.cond),
            .body = try self.onHeap(try self.statement(do_while.body.*)),
            .label = try self.dupe(do_while.label),
        } },
        .@"for" => |_for| .{ .@"for" = .{
            .init = switch (_for.init) {
                .decl => |decl| .{ .decl = try self.onHeap(try self.declaration(decl.*)) },
                .expr => |expr| .{
                    .expr = if (expr) |_expr| try self.expression(_expr) else null,
                },
            },
            .cond = if (_for.cond) |cond| try self.expression(cond) else null,
            .post = if (_for.post) |post| try self.expression(post) else null,
            .body = try self.onHeap(try self.statement(_for.body.*)),
            .label = try self.dupe(_for.label),
        } },
        .@"switch" => |_switch| _stmt: {
            var cases_arr = try std.ArrayList(PIR.Statement.Switch.CaseLabel).initCapacity(self.allocator, _switch.cases.len);
            for (_switch.cases) |case|
                try cases_arr.append(self.allocator, .{
                    .expr = try self.expression(case.expr),
                    .label = try self.dupe(case.label),
                });
            const cases = try cases_arr.toOwnedSlice(self.allocator);

            break :_stmt .{ .@"switch" = .{
                .cond = try self.expression(_switch.cond),
                .body = try self.onHeap(try self.statement(_switch.body.*)),
                .cases = cases,
                .default = if (_switch.default) |default| try self.dupe(default.label) else null,
                .label = try self.dupe(_switch.label),
            } };
        },
        .case => |case| .{ .case = .{
            .expr = try self.expression(case.expr),
            .stmt = if (case.stmt) |_stmt|
                try self.onHeap(try self.statement(_stmt.*))
            else
                null,
            .label = try self.dupe(case.label),
        } },
        .default => |default| .{ .default = .{
            .stmt = if (default.stmt) |_stmt|
                try self.onHeap(try self.statement(_stmt.*))
            else
                null,
            .label = try self.dupe(default.label),
        } },
    };
}

fn expression(self: *Stripper, expr: AST.Expression) InternalError!PIR.Expression {
    return switch (expr) {
        .err => |err| return self.handleErr(err),
        .int_lit => |int_lit| .{ .int_lit = int_lit.value },
        .lhs_unary => |unary| .{ .lhs_unary = .{
            .operator = unary.operator.@"0",
            .rhs = try self.onHeap(try self.expression(unary.rhs.*)),
        } },
        .rhs_unary => |unary| .{ .rhs_unary = .{
            .operator = unary.operator.@"0",
            .lhs = try self.onHeap(try self.expression(unary.lhs.*)),
        } },
        .binary => |binary| .{ .binary = .{
            .operator = binary.operator.@"0",
            .lhs = try self.onHeap(try self.expression(binary.lhs.*)),
            .rhs = try self.onHeap(try self.expression(binary.rhs.*)),
        } },
        .assignment => |ass| .{ .assignment = .{
            .operator = ass.operator.@"0",
            .lhs = try self.onHeap(try self.expression(ass.lhs.*)),
            .rhs = try self.onHeap(try self.expression(ass.rhs.*)),
        } },
        .conditional => |cond| .{ .conditional = .{
            .cond = try self.onHeap(try self.expression(cond.cond.*)),
            .if_true = try self.onHeap(try self.expression(cond.if_true.*)),
            .if_false = try self.onHeap(try self.expression(cond.if_false.*)),
        } },
        .variable => |_var| .{
            .variable = mame: {
                const n = try self.dupe(_var.unique_name);
                std.log.debug("stripper {s}, unique: {s}", .{ n, _var.unique_name });
                break :mame n;
            },
        },
    };
}

fn dupe(self: *Stripper, str: []const u8) InternalError![]const u8 {
    return try self.allocator.dupe(u8, str);
}
