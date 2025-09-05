const std = @import("std");

const VIR = @import("VIR.zig");
pub const BlockItemKind = VIR.BlockItemKind;
pub const StatementKind = VIR.StatementKind;
pub const DeclarationKind = VIR.DeclarationKind;
pub const Declaration = VIR.Declaration;
pub const Expression = VIR.Expression;

pub const LIR = @This();

main_function: Function,

arena: std.heap.ArenaAllocator,

pub fn free(self: LIR) void {
    self.arena.deinit();
}

pub fn format(
    self: @This(),
    writer: *std.Io.Writer,
) std.Io.Writer.Error!void {
    try writer.print("ProgramLIR{{ main_function: {f} }}", .{self.main_function});
}

pub const Block = struct {
    items: []const BlockItem,

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try writer.writeAll("Block{ ");
        for (self.items, 1..) |item, i| {
            try writer.print("{f}", .{item});
            if (i < self.items.len) {
                try writer.writeAll(", ");
            }
        }
        try writer.writeAll(" }");
    }
};

pub const Function = struct {
    name: []const u8,
    body: Block,

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try writer.print("FunctionLIR{{ name: {s}, body: {{ {f} }}", .{
            self.name,
            self.body,
        });
    }
};

pub const BlockItem = union(BlockItemKind) {
    statement: Statement,
    declaration: Declaration,

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self) {
            .statement => |stmt| try writer.print("StmtItem{{ {f} }}", .{stmt}),
            .declaration => |decl| try writer.print("DeclItem{{ {f} }}", .{decl}),
        }
    }
};

pub const Statement = union(StatementKind) {
    @"return": Expression,
    expr: Expression,
    null,
    @"if": If,
    goto: []const u8,
    labeled_stmt: LabeledStmt,
    compound: Block,

    pub const If = struct {
        cond: Expression,
        then: *Statement,
        or_else: ?*Statement,

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.print("IfStmt{{ cond: {f}, them: {f}, or_else: ", .{
                self.cond,
                self.then.*,
            });
            if (self.or_else) |stmt| {
                try writer.print("{f} }}", .{stmt});
            } else {
                try writer.writeAll("null }");
            }
        }
    };

    pub const LabeledStmt = struct {
        label: []const u8,
        stmt: *Statement,

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.print("LabeledStmtStmt{{ label: {s}, stmt: {f} }}", .{
                self.label,
                self.stmt.*,
            });
        }
    };

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self) {
            .@"return" => |expr| try writer.print("ReturnStmt{{ {f} }}", .{expr}),
            .expr => |expr| try writer.print("ExprStmt{{ {f} }}", .{expr}),
            .null => try writer.print("NullStmt", .{}),
            .goto => |target| try writer.print("GotoStmt{{ {s} }}", .{target}),
            inline else => |s| try writer.print("{f}", .{s}),
        }
    }
};
