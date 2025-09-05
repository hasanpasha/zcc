const std = @import("std");

const TokenKind = @import("../token.zig").TokenKind;

const AST = @import("../AST.zig");
pub const Identifier = AST.Identifier;
pub const BlockItemKind = AST.BlockItemKind;
pub const DeclarationKind = AST.DeclarationKind;
pub const StatementKind = AST.StatementKind;
pub const ExpressionKind = AST.ExpressionKind;

const VIR = @This();

main_function: Function,

arena: std.heap.ArenaAllocator,

pub fn free(self: VIR) void {
    self.arena.deinit();
}

pub fn format(
    self: @This(),
    writer: *std.Io.Writer,
) std.Io.Writer.Error!void {
    try writer.print("ProgramVIR{{ main_function: {f} }}", .{self.main_function});
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
        try writer.print("FunctionVIR{{ name: {s}, body: {{ {f} }}", .{
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

pub const Declaration = union(DeclarationKind) {
    variable: Variable,

    pub const Variable = struct {
        name: []const u8,
        init: ?Expression,

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.print("VarDecl{{ name: {s}, init: ", .{self.name});
            if (self.init) |init| {
                try writer.print("{f}", .{init});
            } else {
                try writer.writeAll("null");
            }
            try writer.writeAll(" }");
        }
    };

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self) {
            .variable => |variable| try writer.print("{f}", .{variable}),
        }
    }
};

pub const Statement = union(StatementKind) {
    @"return": Expression,
    expr: Expression,
    null,
    @"if": If,
    goto: Identifier,
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
        label: Identifier,
        stmt: *Statement,

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.print("LabeledStmtStmt{{ label: {f}, stmt: {f} }}", .{
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
            .goto => |target| try writer.print("GotoStmt{{ {f} }}", .{target}),
            .compound => |block| try writer.print("CompoundStmt{{ {f} }}", .{block}),
            inline else => |s| try writer.print("{f}", .{s}),
        }
    }
};

pub const Expression = union(ExpressionKind) {
    int_lit: u128,
    binary: Binary,
    lhs_unary: LhsUnary,
    rhs_unary: RhsUnary,
    variable: []const u8,
    assignment: Assignment,
    conditional: Conditional,

    pub const Binary = struct {
        operator: TokenKind,
        lhs: *Expression,
        rhs: *Expression,

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.print("BinaryExpr{{ operator: {}, lhs: {f}, rhs: {f} }}", .{
                self.operator,
                self.lhs.*,
                self.rhs.*,
            });
        }
    };

    pub const LhsUnary = struct {
        operator: TokenKind,
        rhs: *Expression,

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.print("LhsUnaryExpr{{ operator: {}, rhs: {f} }}", .{
                self.operator,
                self.rhs,
            });
        }
    };

    pub const RhsUnary = struct {
        operator: TokenKind,
        lhs: *Expression,

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.print("RhsUnaryExpr{{ operator: {}, lhs: {f} }}", .{
                self.operator,
                self.lhs,
            });
        }
    };

    pub const Assignment = struct {
        operator: TokenKind,
        lhs: *Expression,
        rhs: *Expression,

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.print("AssignmentExpr{{ operator: {}, lhs: {f}, rhs: {f} }}", .{
                self.operator,
                self.lhs.*,
                self.rhs.*,
            });
        }
    };

    pub const Conditional = struct {
        cond: *Expression,
        if_true: *Expression,
        if_false: *Expression,

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.print("CondExpr{{ cond: {f}, if_true: {f}, if_false: {f} }}", .{
                self.cond.*,
                self.if_true.*,
                self.if_false.*,
            });
        }
    };

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self) {
            .int_lit => |value| try writer.print("IntLitExpr{{ {} }}", .{value}),
            .variable => |name| try writer.print("VariableExpr{{ {s} }}", .{name}),
            inline else => |node| try writer.print("{f}", .{node}),
        }
    }
};
