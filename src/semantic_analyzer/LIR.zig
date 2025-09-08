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
    @"break",
    @"continue",
    @"while": While,
    do_while: DoWhile,
    @"for": For,
    @"switch": Switch,
    case: Case,
    default: Default,

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

    pub const While = struct {
        cond: Expression,
        body: *Statement,

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.print("WhileStmt{{ cond: {f}, body: {f} }}", .{
                self.cond,
                self.body.*,
            });
        }
    };

    pub const DoWhile = struct {
        body: *Statement,
        cond: Expression,

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.print("DoWhile{{ body: {f}, cond: {f} }}", .{
                self.body.*,
                self.cond,
            });
        }
    };

    pub const For = struct {
        init: ForInit,
        cond: ?Expression,
        post: ?Expression,
        body: *Statement,

        pub const ForInitKind = enum {
            decl,
            expr,
        };

        pub const ForInit = union(ForInitKind) {
            decl: *Declaration,
            expr: ?Expression,

            pub fn format(
                self: @This(),
                writer: *std.Io.Writer,
            ) std.Io.Writer.Error!void {
                switch (self) {
                    .decl => |decl| try writer.print("DeclForInit{{ {f} }}", .{decl.*}),
                    .expr => |expr| {
                        try writer.writeAll("ExprForInit{ ");
                        if (expr) |exp| {
                            try writer.print("{f} }}", .{exp});
                        } else {
                            try writer.writeAll("null }");
                        }
                    },
                }
            }
        };

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.print("ForStmt{{ init: {f}, cond: ", .{self.init});
            if (self.cond) |cond| {
                try writer.print("{f}", .{cond});
            } else {
                try writer.writeAll("null");
            }
            try writer.writeAll(", post: ");
            if (self.post) |post| {
                try writer.print("{f}", .{post});
            } else {
                try writer.writeAll("null");
            }
            try writer.print(", body: {f}, label: {s} }}", .{self.body});
        }
    };

    pub const Switch = struct {
        cond: Expression,
        body: *Statement,

        pub fn format(self: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
            try writer.print("SwitchStmt{{ cond: {f}, body: {f} }}", .{ self.cond, self.body });
        }
    };

    pub const Case = struct {
        expr: Expression,
        stmt: ?*Statement,

        pub fn format(self: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
            try writer.print("CaseStmt{{ expr: {f}, stmt: ", .{self.expr});
            if (self.stmt) |stmt| {
                try writer.print("{f} }}", .{stmt});
            } else {
                try writer.writeAll("null }");
            }
        }
    };

    pub const Default = struct {
        stmt: ?*Statement = null,

        pub fn format(self: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
            try writer.print("DefaultStmt{{ stmt: ", .{self.expr});
            if (self.stmt) |stmt| {
                try writer.print("{f} }}", .{stmt});
            } else {
                try writer.writeAll("null }");
            }
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
            .@"break" => try writer.writeAll("breakStmt"),
            .@"continue" => try writer.writeAll("continueStmt"),
            inline else => |s| try writer.print("{f}", .{s}),
        }
    }
};
