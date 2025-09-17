const std = @import("std");
const AllocErr = std.mem.Allocator.Error;
const Writer = std.Io.Writer;
const AST = @import("../AST.zig");
const Result = @import("../result.zig").Result;
const Location = @import("../Location.zig");

pub fn resolve(in: *AST, glob_counter: *usize) AllocErr!void {
    const allocator = in.arena.allocator();

    for (in.declarations) |*ldecl| {
        var decl, _ = ldecl.*;
        if (decl == .function) {
            const labels = try GatherPass.gather(&decl.function, allocator, glob_counter);
            // defer labels.deinit();
            ApplyPass.apply(&decl.function, labels);
        }
    }
}

pub const GatherPass = struct {
    map: std.StringHashMap([]const u8),
    alloc: std.mem.Allocator,
    counter: *usize,

    pub fn gather(ast: *AST.Declaration.Function, allocator: std.mem.Allocator, counter: *usize) AllocErr!std.StringHashMap([]const u8) {
        var self = GatherPass{
            .map = .init(allocator),
            .alloc = allocator,
            .counter = counter,
        };

        try self.block(&ast.body);

        return self.map;
    }

    fn block(self: *GatherPass, blk: *AST.Block) AllocErr!void {
        for (blk.items) |*item|
            if (item.* == .statement)
                try self.statement(&item.statement);
    }

    fn statement(self: *GatherPass, stmt: *AST.Statement) AllocErr!void {
        switch (stmt.*) {
            .@"return", .expr, .goto, .null, .@"break", .@"continue", .err => {},
            .@"if" => |_if| {
                try self.statement(_if.then);
                if (_if.or_else) |s| try self.statement(s);
            },
            .labeled_stmt => |*ls| {
                if (self.map.contains(ls.label.name)) {
                    stmt.* = .{ .err = .{ .duplicate = ls.label.name } };
                    return;
                }

                const unique_name = try self.makeUniqueLabel(ls.label.name);
                try self.map.put(ls.label.name, unique_name);

                try self.statement(ls.stmt);

                var id = ls.label;
                stmt.* = .{ .labeled_stmt = .{
                    .label = id.applyUniqueName(unique_name),
                    .stmt = ls.stmt,
                } };
            },
            .compound => |*blk| try self.block(blk),
            .@"for" => |_for| try self.statement(_for.body),
            .@"while" => |_while| try self.statement(_while.body),
            .do_while => |do_while| try self.statement(do_while.body),
            .@"switch" => |_switch| try self.statement(_switch.body),
            .case => |case| if (case.stmt) |_stmt| try self.statement(_stmt),
            .default => |default| if (default.stmt) |_stmt| try self.statement(_stmt),
        }
    }

    fn makeUniqueLabel(self: *GatherPass, suffix: []const u8) AllocErr![]u8 {
        std.log.debug("label: new counter: {}", .{self.counter.*});
        defer self.counter.* += 1;
        return std.fmt.allocPrint(self.alloc, ".{s}.{}", .{
            suffix,
            self.counter.*,
        });
    }
};

pub const ApplyPass = struct {
    labels: std.StringHashMap([]const u8),

    fn apply(func: *AST.Declaration.Function, labels: std.StringHashMap([]const u8)) void {
        var self = ApplyPass{ .labels = labels };
        _ = self.block(&func.body);
    }

    fn block(self: *ApplyPass, blk: *AST.Block) *AST.Block {
        for (blk.items) |*item| {
            if (item.* == .statement) {
                _ = self.statement(&item.statement);
            }
        }
        return blk;
    }

    fn statement(self: *ApplyPass, stmt: *AST.Statement) *AST.Statement {
        stmt.* = switch (stmt.*) {
            .null, .@"return", .expr, .@"break", .@"continue", .err => stmt.*,
            .@"if" => |_if| .{
                .@"if" = .{
                    .cond = _if.cond,
                    .then = self.statement(_if.then),
                    .or_else = if (_if.or_else) |s| self.statement(s) else null,
                },
            },
            .goto => |*target| if (self.labels.get(target.name)) |unique|
                .{ .goto = target.applyUniqueName(unique) }
            else
                .{ .err = .{ .undeclared = target.name } },
            .labeled_stmt => |ls| .{ .labeled_stmt = .{
                .label = ls.label,
                .stmt = self.statement(ls.stmt),
            } },
            .compound => |*blk| .{ .compound = self.block(blk).* },
            .@"while" => |_while| .{ .@"while" = .{
                .cond = _while.cond,
                .body = self.statement(_while.body),
            } },
            .do_while => |do_while| .{ .do_while = .{
                .body = self.statement(do_while.body),
                .cond = do_while.cond,
            } },
            .@"for" => |_for| .{ .@"for" = .{
                .init = _for.init,
                .cond = _for.cond,
                .post = _for.post,
                .body = self.statement(_for.body),
            } },
            .@"switch" => |_switch| .{ .@"switch" = .{
                .cond = _switch.cond,
                .body = self.statement(_switch.body),
            } },
            .case => |case| .{ .case = .{
                .expr = case.expr,
                .stmt = if (case.stmt) |_stmt| self.statement(_stmt) else null,
            } },
            .default => |default| .{ .default = .{
                .stmt = if (default.stmt) |_stmt| self.statement(_stmt) else null,
            } },
        };

        return stmt;
    }
};
