const std = @import("std");
const token = @import("token.zig");
const TokenKind = token.TokenKind;
const Location = @import("Location.zig");

const AST = @This();

main_function: Function,

/// not part of the AST
arena: std.heap.ArenaAllocator,

pub fn free(self: AST) void {
    self.arena.deinit();
}

pub fn format(
    self: AST,
    writer: *std.Io.Writer,
) std.Io.Writer.Error!void {
    try writer.print("ProgramAST{{ main_function: {f} }}", .{self.main_function});
}

pub const Identifier = struct {
    name: []const u8,
    location: Location,

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try writer.print("Identifier{{ name: {s}, location: {f} }}", .{
            self.name,
            self.location,
        });
    }
};

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
    name: Identifier,
    body: Block,

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try writer.print("FunctionAST{{ name: {f}, body: {f} }}", .{ self.name, self.body });
    }
};

pub const BlockItemKind = enum {
    statement,
    declaration,
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

pub const DeclarationKind = enum {
    variable,
};

pub const Declaration = union(DeclarationKind) {
    variable: Variable,

    pub const Variable = struct {
        name: Identifier,
        init: ?Expression,

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.print("VarDecl{{ name: {f}, init: ", .{self.name});
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

pub const StatementKind = enum { @"return", expr, null, @"if", goto, labeled_stmt, compound };

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

pub const ExpressionKind = enum {
    int_lit,
    binary,
    lhs_unary,
    rhs_unary,
    variable,
    assignment,
    conditional,
};

pub const Expression = union(ExpressionKind) {
    int_lit: IntLit,
    binary: Binary,
    lhs_unary: LhsUnary,
    rhs_unary: RhsUnary,
    variable: Identifier,
    assignment: Assignment,
    conditional: Conditional,

    pub const IntLit = struct {
        value: u128,
        location: Location,

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.print("IntLit{{ value: {}, location: {f} }}", .{
                self.value,
                self.location,
            });
        }
    };

    pub const Binary = struct {
        operator: token.LocatedTokenKind,
        lhs: *Expression,
        rhs: *Expression,

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.print("BinaryExpr{{ operator: {}, lhs: {f}, rhs: {f} }}", .{
                self.operator.@"0",
                self.lhs.*,
                self.rhs.*,
            });
        }
    };

    pub const LhsUnary = struct {
        operator: token.LocatedTokenKind,
        rhs: *Expression,

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.print("LhsUnaryExpr{{ operator: {}, rhs: {f} }}", .{
                self.operator.@"0",
                self.rhs,
            });
        }
    };

    pub const RhsUnary = struct {
        operator: token.LocatedTokenKind,
        lhs: *Expression,

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.print("RhsUnaryExpr{{ operator: {}, lhs: {f} }}", .{
                self.operator.@"0",
                self.lhs,
            });
        }
    };

    pub const Assignment = struct {
        operator: token.LocatedTokenKind,
        lhs: *Expression,
        rhs: *Expression,

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.print("AssignmentExpr{{ operator: {}, lhs: {f}, rhs: {f} }}", .{
                self.operator.@"0",
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
            .variable => |iden| try writer.print("VariableExpr{{ {f} }}", .{iden}),
            inline else => |node| try writer.print("{f}", .{node}),
        }
    }
};
