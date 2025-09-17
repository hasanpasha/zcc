const std = @import("std");
const Writer = std.Io.Writer;
const TokenKind = @import("Token.zig").TokenKind;

const PIR = @This();

declarations: []Declaration,

/// not part of the PIR
arena: std.heap.ArenaAllocator,

pub fn free(self: PIR) void {
    self.arena.deinit();
}

pub fn format(self: @This(), w: *Writer) Writer.Error!void {
    try w.writeAll("PIR{ ");
    for (self.declarations, 1..) |decl, i| {
        try w.print("{f}", .{decl});
        if (i < self.declarations.len)
            try w.writeAll(", ");
    }
    try w.writeAll(" }");
}

pub const DeclarationKind = enum {
    function,
    variable,
};

pub const Declaration = union(DeclarationKind) {
    function: Function,
    variable: Variable,

    pub fn format(self: @This(), w: *Writer) Writer.Error!void {
        switch (self) {
            inline else => |d| try w.print("{f}", .{d}),
        }
    }

    pub const Variable = struct {
        name: []const u8,
        init: ?Expression,

        pub fn format(self: @This(), w: *Writer) Writer.Error!void {
            try w.print("VariableDecl{{ name: {s}, init: ", .{self.name});
            if (self.init) |init| {
                try w.print("{f}", .{init});
            } else {
                try w.writeAll("null");
            }
            try w.writeAll(" }");
        }
    };

    pub const Function = struct {
        name: []const u8,
        body: Block,

        pub fn format(self: @This(), w: *Writer) Writer.Error!void {
            try w.print("FunctionDecl{{ name: {s}, body: {f} }}", .{ self.name, self.body });
        }
    };
};

pub const Block = struct {
    items: []BlockItem,

    pub fn format(self: @This(), w: *Writer) Writer.Error!void {
        try w.writeAll("Block{ ");
        for (self.items, 1..) |item, i| {
            try w.print("{f}", .{item});
            if (i < self.items.len)
                try w.writeAll(", ");
        }
        try w.writeAll(" }");
    }
};

pub const BlockItemKind = enum {
    statement,
    declaration,
};

pub const BlockItem = union(BlockItemKind) {
    statement: Statement,
    declaration: *Declaration,

    pub fn format(self: @This(), w: *Writer) Writer.Error!void {
        switch (self) {
            .statement => |stmt| try w.print("StmtItem{{ {f} }}", .{stmt}),
            .declaration => |decl| try w.print("DeclItem{{ {f} }}", .{decl.*}),
        }
    }
};

pub const StatementKind = enum {
    @"return",
    expr,
    null,
    @"if",
    goto,
    labeled_stmt,
    compound,
    @"break",
    @"continue",
    @"while",
    do_while,
    @"for",
    @"switch",
    case,
    default,
};

pub const Statement = union(StatementKind) {
    @"return": Expression,
    expr: Expression,
    null,
    @"if": If,
    goto: []const u8,
    labeled_stmt: LabeledStmt,
    compound: Block,
    @"break": []const u8,
    @"continue": []const u8,
    @"while": While,
    do_while: DoWhile,
    @"for": For,
    @"switch": Switch,
    case: Case,
    default: Default,

    pub fn format(self: @This(), w: *Writer) Writer.Error!void {
        switch (self) {
            .@"return" => |expr| try w.print("ReturnStmt{{ {f} }}", .{expr}),
            .expr => |expr| try w.print("ExprStmt{{ {f} }}", .{expr}),
            .null => try w.writeAll("NullStmt"),
            .goto => |target| try w.print("GotoStmt{{ {s} }}", .{target}),
            .compound => |blk| try w.print("BlockStmt{{ {f} }}", .{blk}),
            .@"break" => |label| try w.print("BreakStmt{{ {s} }}", .{label}),
            .@"continue" => |label| try w.print("ContinueStmt{{ {s} }}", .{label}),
            inline else => |stmt| try w.print("{f}", .{stmt}),
        }
    }

    pub const If = struct {
        cond: Expression,
        then: *Statement,
        or_else: ?*Statement,

        pub fn format(self: @This(), w: *Writer) Writer.Error!void {
            try w.print("IfStmt{{ cond: {f}, then: {f}, or_else: ", .{ self.cond, self.then.* });
            if (self.or_else) |or_else| {
                try w.print("{f}", .{or_else.*});
            } else {
                try w.writeAll("null");
            }
            try w.writeAll(" }");
        }
    };

    pub const LabeledStmt = struct {
        label: []const u8,
        stmt: *Statement,

        pub fn format(self: @This(), w: *Writer) Writer.Error!void {
            try w.print("LabeledStmtStmt{{ label: {s}, stmt: {f} }}", .{ self.label, self.stmt.* });
        }
    };

    pub const While = struct {
        cond: Expression,
        body: *Statement,
        label: []const u8,

        pub fn format(self: @This(), w: *Writer) Writer.Error!void {
            try w.print("WhileStmt{{ cond: {f}, body: {f}, label: {s} }}", .{ self.cond, self.body.*, self.label });
        }
    };

    pub const DoWhile = struct {
        body: *Statement,
        cond: Expression,
        label: []const u8,

        pub fn format(self: @This(), w: *Writer) Writer.Error!void {
            try w.print("DoWhileStmt{{ body: {f}, cond: {f}, label: {s} }}", .{ self.body.*, self.cond, self.label });
        }
    };

    pub const For = struct {
        init: ForInit,
        cond: ?Expression,
        post: ?Expression,
        body: *Statement,
        label: []const u8,

        pub fn format(self: @This(), w: *Writer) Writer.Error!void {
            try w.print("ForStmt{{ init: {f}, cond: ", .{self.init});
            if (self.cond) |cond| {
                try w.print("{f}", .{cond});
            } else {
                try w.writeAll("null");
            }
            try w.writeAll(", post: ");
            if (self.post) |post| {
                try w.print("{f}", .{post});
            } else {
                try w.writeAll("null");
            }
            try w.print(", body: {f}, label: {s} }}", .{ self.body.*, self.label });
        }

        pub const ForInitKind = enum {
            decl,
            expr,
        };

        pub const ForInit = union(ForInitKind) {
            decl: *Declaration,
            expr: ?Expression,

            pub fn format(self: @This(), w: *Writer) Writer.Error!void {
                switch (self) {
                    .decl => |decl| try w.print("DeclForInit{{ {f} }}", .{decl.*}),
                    .expr => |oexpr| if (oexpr) |expr|
                        try w.print("ExprForInit{{ {f} }}", .{expr})
                    else
                        try w.writeAll("ExprForInit{ null }"),
                }
            }
        };
    };

    pub const Switch = struct {
        cond: Expression,
        body: *Statement,
        label: []const u8,
        cases: []const CaseLabel,
        default: ?[]const u8,

        pub fn format(self: @This(), w: *Writer) Writer.Error!void {
            try w.print("SwitchStmt{{ cond: {f}, body: {f}, label: {s}, cases: {{ ", .{ self.cond, self.body.*, self.label });
            for (self.cases, 1..) |case, i| {
                try w.print("{f}", .{case});
                if (i < self.cases.len)
                    try w.writeAll(", ");
            }
            try w.writeAll(" }, default: ");
            if (self.default) |default| {
                try w.writeAll(default);
            } else {
                try w.writeAll("null");
            }
            try w.writeAll(" }");
        }

        pub const CaseLabel = struct {
            expr: Expression,
            label: []const u8,

            pub fn format(self: @This(), w: *Writer) Writer.Error!void {
                try w.print("SwitchCaseLabel{{ expr: {f}, label: {s} }}", .{ self.expr, self.label });
            }
        };
    };

    pub const Case = struct {
        expr: Expression,
        stmt: ?*Statement,
        label: []const u8,

        pub fn format(self: @This(), w: *Writer) Writer.Error!void {
            try w.print("CaseStmt{{ expr: {f}, stmt: ", .{self.expr});
            if (self.stmt) |stmt| {
                try w.print("{f}", .{stmt.*});
            } else {
                try w.writeAll("null");
            }
            try w.print(", label: {s} }}", .{self.label});
        }
    };

    pub const Default = struct {
        stmt: ?*Statement = null,
        label: []const u8,

        pub fn format(self: @This(), w: *Writer) Writer.Error!void {
            try w.writeAll("DefaultStmt{ stmt: ");
            if (self.stmt) |stmt| {
                try w.print("{f}", .{stmt.*});
            } else {
                try w.writeAll("null");
            }
            try w.print(", label: {s} }}", .{self.label});
        }
    };
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
    int_lit: u128,
    binary: Binary,
    lhs_unary: LhsUnary,
    rhs_unary: RhsUnary,
    variable: []const u8,
    assignment: Assignment,
    conditional: Conditional,

    pub fn format(self: @This(), w: *Writer) Writer.Error!void {
        switch (self) {
            .int_lit => |value| try w.print("IntLitExpr{{ {} }}", .{value}),
            .variable => |name| try w.print("VariableExpr{{ {s} }}", .{name}),
            inline else => |expr| try w.print("{f}", .{expr}),
        }
    }

    pub const Binary = struct {
        operator: TokenKind,
        lhs: *Expression,
        rhs: *Expression,

        pub fn format(self: @This(), w: *Writer) Writer.Error!void {
            try w.print("BinaryExpr{{ operator: {s}, lhs: {f}, rhs: {f} }}", .{
                @tagName(self.operator),
                self.lhs.*,
                self.rhs.*,
            });
        }
    };

    pub const LhsUnary = struct {
        operator: TokenKind,
        rhs: *Expression,

        pub fn format(self: @This(), w: *Writer) Writer.Error!void {
            try w.print("LhsUnaryExpr{{ operator: {s}, rhs: {f} }}", .{
                @tagName(self.operator),
                self.rhs.*,
            });
        }
    };

    pub const RhsUnary = struct {
        operator: TokenKind,
        lhs: *Expression,

        pub fn format(self: @This(), w: *Writer) Writer.Error!void {
            try w.print("RhsUnaryExpr{{ operator: {s}, lhs: {f} }}", .{
                @tagName(self.operator),
                self.lhs.*,
            });
        }
    };

    pub const Assignment = struct {
        operator: TokenKind,
        lhs: *Expression,
        rhs: *Expression,

        pub fn format(self: @This(), w: *Writer) Writer.Error!void {
            try w.print("AssignmentExpr{{ operator: {s}, lhs: {f}, rhs: {f} }}", .{
                @tagName(self.operator),
                self.lhs.*,
                self.rhs.*,
            });
        }
    };

    pub const Conditional = struct {
        cond: *Expression,
        if_true: *Expression,
        if_false: *Expression,

        pub fn format(self: @This(), w: *Writer) Writer.Error!void {
            try w.print("ConditionalExpr{{ cond: {f}, if_true: {f}, if_false: {f} }}", .{
                self.cond.*,
                self.if_true.*,
                self.if_false.*,
            });
        }
    };
};
