const std = @import("std");
const Lexer = @import("Lexer.zig");

const AST = @import("AST.zig");
const Statement = AST.Statement;
const Expression = AST.Expression;

const token = @import("token.zig");
const Token = token.Token;
const TokenKind = token.TokenKind;
const LocatedToken = token.LocatedToken;
const LocatedTokenKind = token.LocatedTokenKind;
const Location = @import("Location.zig");

const Result = @import("result.zig").Result;
const oneOf = @import("utils.zig").oneOf;

const Parser = @This();

lexer: Lexer,
current: ?LocatedToken = null,
peek: ?LocatedToken = null,

/// used to allocate ast nodes
allocator: std.mem.Allocator,

pub const Error = union(enum) {
    lexer_error: Lexer.Error,
    unexpected_token: UnexpectedTokenError,
    unexpected_keyword: UnexpectedKeywordError,
    unexpected_symbol: UnexpectedSymbolError,
    unexpected_end_of_token_stream: Location,
    not_expression_token: Token,

    pub const UnexpectedKeywordError = struct {
        expected: token.Keyword,
        found: token.Keyword,
        location: Location,

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.print("expected keyword {} but found {} at {f}", .{
                self.expected,
                self.found,
                std.fmt.alt(self.location, .readableFmt),
            });
        }
    };

    pub const UnexpectedSymbolError = struct {
        expected: union(enum) {
            one: token.Symbol,
            many: []const token.Symbol,
        },
        found: token.Symbol,
        location: Location,

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            switch (self.expected) {
                .one => |sym| {
                    try writer.print("expected {} but found {} at {f}", .{
                        sym,
                        self.found,
                        std.fmt.alt(self.location, .readableFmt),
                    });
                },
                .many => |syms| {
                    try writer.print("expected one of symbols {{ ", .{});
                    for (syms, 1..) |kind, i| {
                        try writer.print("{}", .{kind});
                        if (i < syms.len) {
                            try writer.writeAll(", ");
                        }
                    }
                    try writer.print(" }} but found {} at {f}", .{
                        self.found,
                        std.fmt.alt(self.location, .readableFmt),
                    });
                },
            }
        }
    };

    pub const UnexpectedTokenError = struct {
        expected: ?TokenKind,
        found: Token,
        location: Location,

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            if (self.expected) |tok| {
                try writer.print("expected token of kind {} but found {f} at {f}", .{
                    tok,
                    self.found,
                    std.fmt.alt(self.location, .readableFmt),
                });
            } else {
                try writer.print("expected no token but found {f} at {f}", .{
                    self.found,
                    std.fmt.alt(self.location, .readableFmt),
                });
            }
        }
    };

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self) {
            .lexer_error => |err| try writer.print("lexer error: {f}", .{err}),
            .unexpected_end_of_token_stream => |l| try writer.print("unexpect end of the token stream at {f}", .{
                std.fmt.alt(l, .readableFmt),
            }),
            .not_expression_token => |tok| try writer.print("{f} is not an expression token", .{tok}),
            inline else => |err| try writer.print("{f}", .{err}),
        }
    }
};

pub fn ParserResult(OkType: type) type {
    return Result(OkType, Error);
}

pub fn format(
    self: @This(),
    writer: *std.Io.Writer,
) std.Io.Writer.Error!void {
    try writer.print("Parser{{ lexer: {f}, current: ", .{self.lexer});
    if (self.current) |cur| {
        try printlocatedToken(cur, writer);
    } else {
        try writer.writeAll("null");
    }
    try writer.writeAll(", peek: ");
    if (self.peek) |peek| {
        try printlocatedToken(peek, writer);
    }
    try writer.writeAll(" }");
}

pub fn printlocatedToken(lt: LocatedToken, writer: *std.Io.Writer) std.Io.Writer.Error!void {
    const tok, const loc = lt;
    try writer.print("LocatedToken{{ token: {f}, location: {f} }}", .{ tok, loc });
}

pub fn parse(lexer: Lexer, allocator: std.mem.Allocator) ParserResult(AST) {
    var arena = std.heap.ArenaAllocator.init(allocator);

    var self = Parser{ .lexer = lexer, .allocator = arena.allocator() };

    if (self.advance().maybe_err()) |err| return .Err(err);
    if (self.advance().maybe_err()) |err| return .Err(err);

    const main_function = switch (self.function()) {
        .ok => |val| val,
        .err => |err| return .Err(err),
    };

    return .Ok(.{ .main_function = main_function, .arena = arena });
}

fn function(self: *Parser) ParserResult(AST.Function) {
    if (self.expectKeyword(.int).maybe_err()) |err| return .Err(err);

    const identifier: AST.Identifier = switch (self.expect(.identifier)) {
        .ok => |val| .{
            .name = val.@"0".identifier,
            .location = val.@"1",
        },
        .err => |err| return .Err(err),
    };

    if (self.expectSymbol(.lparen).maybe_err()) |err| return .Err(err);
    if (self.expectKeyword(.void).maybe_err()) |err| return .Err(err);
    if (self.expectSymbol(.rparen).maybe_err()) |err| return .Err(err);

    const body = switch (self.block()) {
        .ok => |val| val,
        .err => |err| return .Err(err),
    };

    if (self.current) |tok|
        return .Err(.{
            .unexpected_token = .{
                .expected = null,
                .found = tok.@"0",
                .location = tok.@"1",
            },
        });

    return .Ok(.{ .name = identifier, .body = body });
}

fn block(self: *Parser) ParserResult(AST.Block) {
    if (self.expectSymbol(.lcub).maybe_err()) |err| return .Err(err);

    var items = std.ArrayList(AST.BlockItem).initCapacity(
        self.allocator,
        100,
    ) catch @panic("OOM");
    while (self.matchSym(.rcub) == null) {
        switch (self.blockItem()) {
            .ok => |val| items.append(self.allocator, val) catch @panic("OOM"),
            .err => |err| return .Err(err),
        }
    }

    return .Ok(.{
        .items = items.toOwnedSlice(self.allocator) catch @panic("OOM"),
    });
}

fn blockItem(self: *Parser) ParserResult(AST.BlockItem) {
    const current, _ = self.current orelse return .Err(.{
        .unexpected_end_of_token_stream = self.lexer.location,
    });

    return if (current.eql(.{ .keyword = .int }))
        self.declarationItem()
    else
        self.statementItem();
}

fn declarationItem(self: *Parser) ParserResult(AST.BlockItem) {
    const decl = switch (self.variableDecl()) {
        .ok => |val| val,
        .err => |err| return .Err(err),
    };

    return .Ok(.{ .declaration = decl });
}

fn statementItem(self: *Parser) ParserResult(AST.BlockItem) {
    const stmt = switch (self.statement()) {
        .ok => |val| val,
        .err => |err| return .Err(err),
    };
    return .Ok(.{ .statement = stmt });
}

fn variableDecl(self: *Parser) ParserResult(AST.Declaration) {
    if (self.expectKeyword(.int).maybe_err()) |err| return .Err(err);

    const identifier: AST.Identifier = switch (self.expect(.identifier)) {
        .ok => |val| .{
            .name = val.@"0".identifier,
            .location = val.@"1",
        },
        .err => |err| return .Err(err),
    };

    const init = if (self.matchSym(.equals)) |_| switch (self.expression()) {
        .ok => |val| val.*,
        .err => |err| return .Err(err),
    } else null;

    if (self.expectSymbol(.semicolon).maybe_err()) |err| return .Err(err);

    return .Ok(.{ .variable = .{ .name = identifier, .init = init } });
}

fn statement(self: *Parser) ParserResult(Statement) {
    const current, _ = self.current orelse return .Err(.{
        .unexpected_end_of_token_stream = self.lexer.location,
    });

    return if (current.eql(.{ .keyword = .@"return" }))
        self.returnStmt()
    else if (current.eql(.{ .keyword = .@"if" }))
        self.ifStmt()
    else if (current.eql(.{ .keyword = .goto }))
        self.gotoStmt()
    else if (current.eql(.{ .symbol = .semicolon }))
        self.nullStmt()
    else if (current == .identifier and self.peekIs(.{ .symbol = .colon }))
        self.labeledStmtStmt()
    else if (current.eql(.{ .symbol = .lcub }))
        self.compoundStmt()
    else
        self.expressionStmt();
}

fn returnStmt(self: *Parser) ParserResult(Statement) {
    if (self.advance().maybe_err()) |err| {
        return .Err(err);
    }

    const value = switch (self.expression()) {
        .ok => |val| val.*,
        .err => |err| return .Err(err),
    };

    if (self.expectSymbol(.semicolon).maybe_err()) |err| return .Err(err);

    return .Ok(.{ .@"return" = value });
}

fn expressionStmt(self: *Parser) ParserResult(Statement) {
    const expr = switch (self.expression()) {
        .ok => |val| val.*,
        .err => |err| return .Err(err),
    };
    if (self.expectSymbol(.semicolon).maybe_err()) |err| return .Err(err);

    return .Ok(.{ .expr = expr });
}

fn nullStmt(self: *Parser) ParserResult(Statement) {
    if (self.expectSymbol(.semicolon).maybe_err()) |err| return .Err(err);
    return .Ok(.null);
}

fn ifStmt(self: *Parser) ParserResult(Statement) {
    if (self.expectKeyword(.@"if").maybe_err()) |err| return .Err(err);

    if (self.expectSymbol(.lparen).maybe_err()) |err| return .Err(err);
    const cond = switch (self.expression()) {
        .ok => |val| val.*,
        .err => |err| return .Err(err),
    };
    if (self.expectSymbol(.rparen).maybe_err()) |err| return .Err(err);

    const then = switch (self.statement()) {
        .ok => |val| self.onHeap(val),
        .err => |err| return .Err(err),
    };

    const or_else = if (self.matchKeyword(.@"else")) |_| switch (self.statement()) {
        .ok => |val| self.onHeap(val),
        .err => |err| return .Err(err),
    } else null;

    return .Ok(.{ .@"if" = .{
        .cond = cond,
        .then = then,
        .or_else = or_else,
    } });
}

fn gotoStmt(self: *Parser) ParserResult(Statement) {
    if (self.expectKeyword(.goto).maybe_err()) |err| return .Err(err);
    const target: AST.Identifier = switch (self.expect(.identifier)) {
        .ok => |val| .{
            .name = val.@"0".identifier,
            .location = val.@"1",
        },
        .err => |err| return .Err(err),
    };
    if (self.expectSymbol(.semicolon).maybe_err()) |err| return .Err(err);

    return .Ok(.{ .goto = target });
}

fn labeledStmtStmt(self: *Parser) ParserResult(Statement) {
    const label: AST.Identifier = switch (self.expect(.identifier)) {
        .ok => |val| .{
            .name = val.@"0".identifier,
            .location = val.@"1",
        },
        .err => |err| return .Err(err),
    };

    if (self.expectSymbol(.colon).maybe_err()) |err| return .Err(err);

    const stmt = switch (self.statement()) {
        .ok => |val| self.onHeap(val),
        .err => |err| return .Err(err),
    };

    return .Ok(.{ .labeled_stmt = .{
        .label = label,
        .stmt = stmt,
    } });
}

fn compoundStmt(self: *Parser) ParserResult(Statement) {
    return switch (self.block()) {
        .ok => |blk| .Ok(.{ .compound = blk }),
        .err => |err| .Err(err),
    };
}

const Precedence = enum {
    none,
    assignment, // "="|"+="|"-="|"*="|"/="|"%="|"<<="|">>="
    ternary, // "?"|":"
    logical_or, // "||"
    logical_and, // "&&"
    bitwise_or, // "|"
    bitwise_xor, // "^"
    bitwise_and, // "&"
    equality, // "=="|"!="
    comparison, // "<"|"<="|">"|">="
    shift, // "<<"|">>"
    term, // "+-"
    factor, // "*/%"
    lhs_unary, // "+"|"-"|"~"|"!"|"++"|"--"
    rhs_unary, // "++"|"--"
    primary,

    pub fn gt(self: Precedence, other: Precedence) bool {
        return @intFromEnum(self) > @intFromEnum(other);
    }

    /// lesser than or equal
    pub fn le(self: Precedence, other: Precedence) bool {
        return @intFromEnum(self) <= @intFromEnum(other);
    }

    /// return one higher precedence
    pub fn inc(self: Precedence) Precedence {
        return @enumFromInt(@intFromEnum(self) + 1);
    }
};

const PrecedenceRule = struct {
    prefix: *const fn (parser: *Parser) ParserResult(Expression) = no_unary,
    infix: *const fn (parser: *Parser, lhs: *Expression) ParserResult(Expression) = no_binary,
    precedence: Precedence = .none,

    fn no_unary(parser: *Parser) ParserResult(Expression) {
        return .Err(Error{ .not_expression_token = parser.current.?.@"0" });
    }

    fn no_binary(parser: *Parser, lhs: *Expression) ParserResult(Expression) {
        _ = lhs;
        return .Err(Error{ .not_expression_token = parser.current.?.@"0" });
    }
};

const precedence_rules: std.EnumArray(token.Symbol, PrecedenceRule) = .initDefault(.{}, .{
    .lparen = .{ .prefix = group },
    .tilde = .{ .prefix = leftUnary, .precedence = .lhs_unary },
    .minus = .{ .prefix = leftUnary, .infix = binary, .precedence = .term },
    .plus = .{ .prefix = leftUnary, .infix = binary, .precedence = .term },
    .asterisk = .{ .infix = binary, .precedence = .factor },
    .slash = .{ .infix = binary, .precedence = .factor },
    .percent = .{ .infix = binary, .precedence = .factor },
    .amp = .{ .infix = binary, .precedence = .bitwise_and },
    .verbar = .{ .infix = binary, .precedence = .bitwise_or },
    .hat = .{ .infix = binary, .precedence = .bitwise_xor },
    .lt_lt = .{ .infix = binary, .precedence = .shift },
    .gt_gt = .{ .infix = binary, .precedence = .shift },
    .excl = .{ .prefix = leftUnary, .precedence = .lhs_unary },
    .amp_amp = .{ .infix = binary, .precedence = .logical_and },
    .verbar_verbar = .{ .infix = binary, .precedence = .logical_or },
    .equals_equals = .{ .infix = binary, .precedence = .equality },
    .excl_equals = .{ .infix = binary, .precedence = .equality },
    .lt = .{ .infix = binary, .precedence = .comparison },
    .gt = .{ .infix = binary, .precedence = .comparison },
    .lt_equals = .{ .infix = binary, .precedence = .comparison },
    .gt_equals = .{ .infix = binary, .precedence = .comparison },
    .equals = .{ .infix = assignment, .precedence = .assignment },
    .plus_equals = .{ .infix = assignment, .precedence = .assignment },
    .minus_equals = .{ .infix = assignment, .precedence = .assignment },
    .asterisk_equals = .{ .infix = assignment, .precedence = .assignment },
    .slash_equals = .{ .infix = assignment, .precedence = .assignment },
    .percent_equals = .{ .infix = assignment, .precedence = .assignment },
    .amp_equals = .{ .infix = assignment, .precedence = .assignment },
    .verbar_equals = .{ .infix = assignment, .precedence = .assignment },
    .hat_equals = .{ .infix = assignment, .precedence = .assignment },
    .lt_lt_equals = .{ .infix = assignment, .precedence = .assignment },
    .gt_gt_equals = .{ .infix = assignment, .precedence = .assignment },
    .plus_plus = .{ .prefix = leftUnary, .infix = rhsUnary, .precedence = .rhs_unary },
    .minus_minus = .{ .prefix = leftUnary, .infix = rhsUnary, .precedence = .rhs_unary },
    .quest = .{ .infix = ternary, .precedence = .ternary },
});

fn peekPrecedenceRule(self: *Parser) ParserResult(PrecedenceRule) {
    const next_token, _ = self.current orelse return .Err(.{
        .unexpected_end_of_token_stream = self.lexer.location,
    });

    const rule: PrecedenceRule = switch (next_token) {
        .identifier => .{ .prefix = variable, .precedence = .primary },
        .int_lit => .{ .prefix = intLitExpr, .precedence = .primary },
        .symbol => |symbol| precedence_rules.get(symbol),
        .keyword => unreachable,
    };

    return .Ok(rule);
}

fn parsePrecedence(self: *Parser, precedence: Precedence) ParserResult(*Expression) {
    const rule = switch (self.peekPrecedenceRule()) {
        .ok => |val| val,
        .err => |err| return .Err(err),
    };

    var lhs: *Expression = switch (rule.prefix(self)) {
        .ok => |val| self.onHeap(val),
        .err => |err| return .Err(err),
    };

    while (true) {
        const rule_next = switch (self.peekPrecedenceRule()) {
            .ok => |val| val,
            .err => |err| return .Err(err),
        };

        if (precedence.gt(rule_next.precedence)) break;

        lhs = switch (rule_next.infix(self, lhs)) {
            .ok => |val| self.onHeap(val),
            .err => |err| return .Err(err),
        };
    }

    return .Ok(lhs);
}

fn expression(self: *Parser) ParserResult(*Expression) {
    return self.parsePrecedence(@enumFromInt(1));
}

fn group(self: *Parser) ParserResult(Expression) {
    if (self.expectSymbol(.lparen).maybe_err()) |err| return .Err(err);
    const expr = switch (self.expression()) {
        .ok => |val| val.*,
        .err => |err| return .Err(err),
    };
    if (self.expectSymbol(.rparen).maybe_err()) |err| return .Err(err);
    return .Ok(expr);
}

fn intLitExpr(self: *Parser) ParserResult(Expression) {
    const int: AST.Expression.IntLit = switch (self.expect(.int_lit)) {
        .ok => |val| .{
            .value = val.@"0".int_lit,
            .location = val.@"1",
        },
        .err => |err| return .Err(err),
    };

    return .Ok(.{ .int_lit = int });
}

fn leftUnary(self: *Parser) ParserResult(Expression) {
    const operator = switch (self.expectSymbols(&.{
        .tilde,
        .plus,
        .minus,
        .excl,
        .plus_plus,
        .minus_minus,
    })) {
        .ok => |val| val,
        .err => |err| return .Err(err),
    };

    const rhs = switch (self.parsePrecedence(.lhs_unary)) {
        .ok => |val| val,
        .err => |err| return .Err(err),
    };

    return .Ok(.{ .lhs_unary = .{
        .operator = operator,
        .rhs = rhs,
    } });
}

fn rhsUnary(self: *Parser, lhs: *Expression) ParserResult(Expression) {
    const operator = switch (self.expectSymbols(&.{
        .plus_plus,
        .minus_minus,
    })) {
        .ok => |val| val,
        .err => |err| return .Err(err),
    };

    return .Ok(.{ .rhs_unary = .{
        .operator = operator,
        .lhs = lhs,
    } });
}

fn binary(self: *Parser, lhs: *Expression) ParserResult(Expression) {
    const operator = switch (self.expectSymbols(&.{
        .minus,
        .plus,
        .asterisk,
        .slash,
        .percent,
        .amp,
        .verbar,
        .hat,
        .lt_lt,
        .gt_gt,
        .amp_amp,
        .verbar_verbar,
        .equals_equals,
        .excl_equals,
        .lt,
        .gt,
        .lt_equals,
        .gt_equals,
        .equals,
    })) {
        .ok => |val| val,
        .err => |err| return .Err(err),
    };

    const prec = precedence_rules.get(operator.@"0").precedence.inc();
    const rhs = switch (self.parsePrecedence(prec)) {
        .ok => |val| val,
        .err => |err| return .Err(err),
    };

    return .Ok(.{ .binary = .{
        .operator = operator,
        .lhs = lhs,
        .rhs = rhs,
    } });
}

fn assignment(self: *Parser, lhs: *Expression) ParserResult(Expression) {
    const operator = switch (self.expectSymbols(&.{
        .equals,
        .plus_equals,
        .minus_equals,
        .asterisk_equals,
        .slash_equals,
        .percent_equals,
        .amp_equals,
        .verbar_equals,
        .hat_equals,
        .lt_lt_equals,
        .gt_gt_equals,
    })) {
        .ok => |val| val,
        .err => |err| return .Err(err),
    };

    const prec = precedence_rules.get(operator.@"0").precedence;
    const rhs = switch (self.parsePrecedence(prec)) {
        .ok => |val| val,
        .err => |err| return .Err(err),
    };

    return .Ok(.{ .assignment = .{
        .operator = operator,
        .lhs = lhs,
        .rhs = rhs,
    } });
}

fn variable(self: *Parser) ParserResult(Expression) {
    const identifier: AST.Identifier = switch (self.expect(.identifier)) {
        .ok => |val| .{
            .name = val.@"0".identifier,
            .location = val.@"1",
        },
        .err => |err| return .Err(err),
    };
    return .Ok(.{ .variable = identifier });
}

fn ternary(self: *Parser, cond: *Expression) ParserResult(Expression) {
    if (self.expectSymbol(.quest).maybe_err()) |err| return .Err(err);

    const left = switch (self.expression()) {
        .ok => |val| val,
        .err => |err| return .Err(err),
    };

    if (self.expectSymbol(.colon).maybe_err()) |err| return .Err(err);

    const right = switch (self.parsePrecedence(.ternary)) {
        .ok => |val| val,
        .err => |err| return .Err(err),
    };

    return .Ok(.{ .conditional = .{
        .cond = cond,
        .if_true = left,
        .if_false = right,
    } });
}

fn expectSymbols(self: *Parser, symbols: []const token.Symbol) ParserResult(token.LocatedSymbol) {
    const symbol, const loc = switch (self.expect(.symbol)) {
        .ok => |val| .{
            val.@"0".symbol,
            val.@"1",
        },
        .err => |err| return .Err(err),
    };

    if (!oneOf(symbol, symbols)) return .Err(.{
        .unexpected_symbol = .{
            .expected = .{ .many = symbols },
            .found = symbol,
            .location = loc,
        },
    });

    return .Ok(.{ symbol, loc });
}

fn expectSymbol(self: *Parser, symbol: token.Symbol) ParserResult(token.LocatedSymbol) {
    const cur_symbol, const loc = switch (self.expect(.symbol)) {
        .ok => |val| .{
            val.@"0".symbol,
            val.@"1",
        },
        .err => |err| return .Err(err),
    };

    if (cur_symbol != symbol) return .Err(.{
        .unexpected_symbol = .{
            .expected = .{ .one = symbol },
            .found = symbol,
            .location = loc,
        },
    });

    return .Ok(.{ symbol, loc });
}

fn expectKeyword(self: *Parser, keyword: token.Keyword) ParserResult(token.LocatedKeyword) {
    const cur_keyword, const loc = switch (self.expect(.keyword)) {
        .ok => |val| .{
            val.@"0".keyword,
            val.@"1",
        },
        .err => |err| return .Err(err),
    };

    if (cur_keyword != keyword) return .Err(.{
        .unexpected_keyword = .{
            .expected = keyword,
            .found = cur_keyword,
            .location = loc,
        },
    });

    return .Ok(.{ keyword, loc });
}

fn expect(self: *Parser, kind: TokenKind) ParserResult(LocatedToken) {
    const current, const loc = self.current orelse return .Err(.{
        .unexpected_end_of_token_stream = self.lexer.location,
    });

    if (current != kind)
        return .Err(.{
            .unexpected_token = .{
                .expected = kind,
                .found = current,
                .location = loc,
            },
        });

    if (self.advance().maybe_err()) |err| return .Err(err);
    return .Ok(.{ current, loc });
}

fn advance(self: *Parser) ParserResult(?LocatedToken) {
    const current = self.current;

    self.current = self.peek;
    self.peek = switch (self.lexerNext()) {
        .ok => |val| val,
        .err => |err| return .Err(err),
    };

    return .Ok(current);
}

fn matchKeyword(self: *Parser, keyword: token.Keyword) ?token.LocatedKeyword {
    if (self.current) |cur| {
        const tok, const loc = cur;
        if (tok.eql(.{ .keyword = keyword })) {
            _ = self.advance().unwrap();
            return .{ keyword, loc };
        }
    }
    return null;
}

fn matchSym(self: *Parser, sym: token.Symbol) ?token.LocatedSymbol {
    if (self.current) |cur| {
        const tok, const loc = cur;
        if (tok.eql(.{ .symbol = sym })) {
            _ = self.advance().unwrap();
            return .{ sym, loc };
        }
    }
    return null;
}

fn mapLexerError(err: Lexer.Error) Parser.Error {
    return .{ .lexer_error = err };
}

fn lexerNext(self: *Parser) ParserResult(?LocatedToken) {
    return self.lexer.next().map_err(mapLexerError);
}

fn peekIs(self: *Parser, tok: Token) bool {
    if (self.peek) |peek| {
        return peek.@"0".eql(tok);
    }
    return false;
}

const onHeap = @import("utils.zig").onHeap;
