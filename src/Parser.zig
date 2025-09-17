const std = @import("std");
const AllocErr = std.mem.Allocator.Error;
const Writer = std.Io.Writer;

const Lexer = @import("Lexer.zig");

const AST = @import("AST.zig");
const Block = AST.Block;
const BlockItem = AST.BlockItem;
const Decl = AST.Declaration;
const LDecl = AST.LDeclaration;
const Stmt = AST.Statement;
const Expr = AST.Expression;

const Token = @import("Token.zig");
const TokenVariant = Token.TokenVariant;
const TokenKind = Token.TokenKind;
const LocatedTokenKind = AST.LocatedTokenKind;
const Location = @import("Location.zig");

const oneOf = @import("utils.zig").oneOf;

const Parser = @This();

lexer: Lexer,
current: ?Token = null,
peek: ?Token = null,

/// used to allocate ast nodes
allocator: std.mem.Allocator,

pub fn parse(lexer: Lexer, allocator: std.mem.Allocator) AllocErr!AST {
    var arena = std.heap.ArenaAllocator.init(allocator);

    var self = Parser{ .lexer = lexer, .allocator = arena.allocator() };

    _ = self.advance();
    _ = self.advance();

    var decls_arr = try std.ArrayList(LDecl).initCapacity(self.allocator, 10);
    while (self.current != null) {
        const start_loc = self.current.?.span.@"0";
        const decl = try self.declaration();
        const end_loc = if (self.current) |cu| cu.span.@"1" else self.lexer.current_location;
        try decls_arr.append(self.allocator, .{ decl, .{ start_loc, end_loc } });
        if (decl == .err) {
            if (decl.err == .eoi) break;
            self.seek_until_one_of(&.{.int});
        }
    }

    const decls = try decls_arr.toOwnedSlice(allocator);
    return .{ .declarations = decls, .arena = arena };
}

fn err(self: *Parser, T: type, _err: Error, ast_err: AST.Error) T {
    return switch (_err) {
        Error.eoi => self.eoi(T),
        Error.lexer_error => @unionInit(T, "err", .{ .lexer_error = self.current.?.variant.err }),
        else => @unionInit(T, "err", ast_err),
    };
}

fn eoi(self: *Parser, T: type) T {
    return @unionInit(T, "err", .{ .eoi = self.lastLocation() });
}

fn declaration(self: *Parser) AllocErr!Decl {
    const t = self.expect(.int) catch |_err| return self.err(Decl, _err, .expected_type);

    return switch (self.peekKind() orelse return self.eoi(Decl)) {
        .lparen => try self.functionDecl(t),
        else => try self.variableDecl(t),
    };
}

fn functionDecl(self: *Parser, t: Token) AllocErr!Decl {
    _ = t;

    const name = self.expectIdentifier() catch |_err| {
        if (_err == AllocErr.OutOfMemory) return @errorCast(_err);
        return self.err(Decl, @errorCast(_err), .{ .unexpected_token = .identifier });
    };

    self.consume(.lparen) catch |_err| return self.err(Decl, _err, .{ .unexpected_token = .lparen });
    self.consume(.void) catch |_err| return self.err(Decl, _err, .{ .unexpected_token = .void });
    self.consume(.rparen) catch |_err| return self.err(Decl, _err, .{ .unexpected_token = .rparen });

    const body = try self.block();

    return .{ .function = .{
        .name = name,
        .body = body,
    } };
}

fn variableDecl(self: *Parser, t: Token) AllocErr!Decl {
    _ = t;

    const identifier = self.expectIdentifier() catch |_err| {
        if (_err == AllocErr.OutOfMemory) return @errorCast(_err);
        return self.err(Decl, @errorCast(_err), .{ .unexpected_token = .identifier });
    };

    const init = if (self.match(.equals)) |_| try self.expression() else null;

    const decl = Decl{ .variable = .{
        .name = identifier,
        .init = init,
    } };

    self.consume(.semicolon) catch |_err| return self.err(Decl, _err, .missing_semi);

    return decl;
}

fn block(self: *Parser) AllocErr!Block {
    self.consume(.lcub) catch @panic("LCUB");

    var items = try std.ArrayList(BlockItem).initCapacity(self.allocator, 4);
    while (self.match(.rcub) == null) {
        const item = try self.blockItem();
        try items.append(self.allocator, item);
        if (item == .err and item.err == .eoi) break;
    }

    return .{ .items = try items.toOwnedSlice(self.allocator) };
}

fn blockItem(self: *Parser) AllocErr!BlockItem {
    const current = self.current orelse return self.eoi(BlockItem);

    return switch (current.variant) {
        .int => try self.declarationItem(),
        else => try self.statementItem(),
    };
}

fn declarationItem(self: *Parser) AllocErr!BlockItem {
    return .{ .declaration = try self.onHeap(try self.declaration()) };
}

fn statementItem(self: *Parser) AllocErr!BlockItem {
    return .{ .statement = try self.statement() };
}

fn statement(self: *Parser) AllocErr!Stmt {
    const current = self.current orelse return self.eoi(Stmt);

    const stmt = switch (current.variant) {
        .@"return" => try self.returnStmt(),
        .semicolon => self.nullStmt(),
        .@"if" => try self.ifStmt(),
        .goto => try self.gotoStmt(),
        .identifier => if (self.peekKind() == .colon) try self.labeledStmtStmt() else try self.expressionStmt(),
        .lcub => try self.compoundStmt(),
        .@"break" => self.breakStmt(),
        .@"continue" => self.continueStmt(),
        .@"while" => try self.whileStmt(),
        .do => try self.doWhileStmt(),
        .@"for" => try self.forStmt(),
        .@"switch" => try self.switchStmt(),
        .case => try self.caseStmt(),
        .default => try self.defaultStmt(),
        else => try self.expressionStmt(),
    };

    if (stmt == .err)
        self.seek_until_one_of(&.{
            .@"return",
            .semicolon,
            .@"if",
            .goto,
            .identifier,
            .lcub,
            .@"break",
            .@"continue",
            .@"while",
            .do,
            .@"for",
            .@"switch",
            .case,
            .default,
            .lparen,
            .int_lit,
            .plus,
            .plus_plus,
            .minus,
            .minus_minus,
            .tilde,
        });

    return stmt;
}

fn seek_until_one_of(self: *Parser, kinds: []const TokenKind) void {
    while (self.current) |tok| {
        if (oneOf(tok.variant.kind(), kinds)) break;
        _ = self.advance();
    }
}

fn returnStmt(self: *Parser) AllocErr!Stmt {
    self.consume(.@"return") catch @panic("EOI");

    const value = try self.expression();

    self.consume(.semicolon) catch |_err| return self.err(Stmt, _err, .missing_semi);

    return .{ .@"return" = value };
}

fn expressionStmt(self: *Parser) AllocErr!Stmt {
    const expr = try self.expression();
    self.consume(.semicolon) catch return .{ .err = .missing_semi };

    return .{ .expr = expr };
}

fn nullStmt(self: *Parser) Stmt {
    self.consume(.semicolon) catch @panic("SEMICOLON");
    return .null;
}

fn ifStmt(self: *Parser) AllocErr!Stmt {
    self.consume(.@"if") catch @panic("IF");

    self.consume(.lparen) catch |_err| return self.err(Stmt, _err, .{ .unexpected_token = .lparen });
    const cond = try self.expression();
    self.consume(.rparen) catch |_err| return self.err(Stmt, _err, .{ .unexpected_token = .rparen });

    const then = try self.onHeap(try self.statement());

    const or_else = if (self.match(.@"else")) |_| try self.onHeap(try self.statement()) else null;

    return .{ .@"if" = .{
        .cond = cond,
        .then = then,
        .or_else = or_else,
    } };
}

fn gotoStmt(self: *Parser) AllocErr!Stmt {
    self.consume(.goto) catch @panic("GOTO");
    const target = self.expectIdentifier() catch |_err| {
        if (_err == AllocErr.OutOfMemory) return @errorCast(_err);
        return self.err(Stmt, @errorCast(_err), .missing_label);
    };

    self.consume(.semicolon) catch |_err| return self.err(Stmt, _err, .missing_semi);

    return .{ .goto = target };
}

fn labeledStmtStmt(self: *Parser) AllocErr!Stmt {
    const label = self.expectIdentifier() catch |_err| {
        if (_err == AllocErr.OutOfMemory) return @errorCast(_err);
        return self.err(Stmt, @errorCast(_err), .missing_label);
    };

    self.consume(.colon) catch |_err| return self.err(Stmt, _err, .{ .unexpected_token = .colon });

    const stmt = try self.onHeap(try self.statement());

    return .{ .labeled_stmt = .{
        .label = label,
        .stmt = stmt,
    } };
}

fn compoundStmt(self: *Parser) AllocErr!Stmt {
    return .{ .compound = try self.block() };
}

fn breakStmt(self: *Parser) Stmt {
    self.consume(.@"break") catch @panic("BREAK");
    self.consume(.semicolon) catch |_err| return self.err(Stmt, _err, .missing_semi);
    return .{ .@"break" = "" };
}

fn continueStmt(self: *Parser) Stmt {
    self.consume(.@"continue") catch @panic("CONTINUE");
    self.consume(.semicolon) catch |_err| return self.err(Stmt, _err, .missing_semi);
    return .{ .@"continue" = "" };
}

fn whileStmt(self: *Parser) AllocErr!Stmt {
    self.consume(.@"while") catch @panic("WHILE");

    self.consume(.lparen) catch |_err| return self.err(Stmt, _err, .{ .unexpected_token = .lparen });
    const cond = try self.expression();
    self.consume(.rparen) catch |_err| return self.err(Stmt, _err, .{ .unexpected_token = .rparen });

    const body = try self.onHeap(try self.statement());

    return .{ .@"while" = .{
        .cond = cond,
        .body = body,
    } };
}

fn doWhileStmt(self: *Parser) AllocErr!Stmt {
    self.consume(.do) catch @panic("DO");

    const body = try self.onHeap(try self.statement());

    self.consume(.@"while") catch |_err| return self.err(Stmt, _err, .missing_while_part);

    self.consume(.lparen) catch |_err| return self.err(Stmt, _err, .{ .unexpected_token = .lparen });
    const cond = try self.expression();
    self.consume(.rparen) catch |_err| return self.err(Stmt, _err, .{ .unexpected_token = .rparen });
    self.consume(.semicolon) catch |_err| return self.err(Stmt, _err, .missing_semi);

    return .{ .do_while = .{
        .cond = cond,
        .body = body,
    } };
}

fn forStmt(self: *Parser) AllocErr!Stmt {
    self.consume(.@"for") catch @panic("FOR");

    self.consume(.lparen) catch |_err| return self.err(Stmt, _err, .{ .unexpected_token = .lparen });

    const init: AST.Statement.For.ForInit = if (self.curKind()) |kind| switch (kind) {
        .int => .{ .decl = try self.onHeap(try self.declaration()) },
        .semicolon => .{ .expr = null },
        else => .{ .expr = try self.expression() },
    } else return self.eoi(Stmt);

    if (init != .decl) self.consume(.semicolon) catch return .{ .err = .missing_semi };

    const cond = if (self.curKind()) |kind| switch (kind) {
        .semicolon => null,
        else => try self.expression(),
    } else return self.eoi(Stmt);

    self.consume(.semicolon) catch |_err| return self.err(Stmt, _err, .missing_semi);

    const post = if (self.curKind()) |kind| switch (kind) {
        .rparen => null,
        else => try self.expression(),
    } else return self.eoi(Stmt);

    self.consume(.rparen) catch |_err| return self.err(Stmt, _err, .{ .unexpected_token = .rparen });

    const body = try self.onHeap(try self.statement());

    return .{ .@"for" = .{
        .init = init,
        .cond = cond,
        .post = post,
        .body = body,
    } };
}

fn switchStmt(self: *Parser) AllocErr!Stmt {
    self.consume(.@"switch") catch @panic("SWITCH");

    self.consume(.lparen) catch |_err| return self.err(Stmt, _err, .{ .unexpected_token = .lparen });
    const cond = try self.expression();
    self.consume(.rparen) catch |_err| return self.err(Stmt, _err, .{ .unexpected_token = .rparen });

    const body = try self.onHeap(try self.statement());

    return .{ .@"switch" = .{
        .cond = cond,
        .body = body,
    } };
}

fn caseStmt(self: *Parser) AllocErr!Stmt {
    self.consume(.case) catch @panic("CASE");

    const expr = try self.intLitExpr();
    self.consume(.colon) catch |_err| return self.err(Stmt, _err, .{ .unexpected_token = .colon });

    const kind = self.curKind() orelse return self.eoi(Stmt);
    const stmt = if (!oneOf(kind, &.{ .rcub, .default, .case })) try self.onHeap(try self.statement()) else null;

    return .{ .case = .{
        .expr = expr,
        .stmt = stmt,
    } };
}

fn defaultStmt(self: *Parser) AllocErr!Stmt {
    self.consume(.default) catch @panic("DEFAULT");
    self.consume(.colon) catch |_err| return self.err(Stmt, _err, .{ .unexpected_token = .colon });

    const kind = self.curKind() orelse return self.eoi(Stmt);
    const stmt = if (!oneOf(kind, &.{ .rcub, .default, .case })) try self.onHeap(try self.statement()) else null;

    return .{ .default = .{ .stmt = stmt } };
}

const Precedence = enum {
    none,
    assignment, // = += -= *= /= %= <<= >>=
    ternary, //  ? :
    logical_or, // |
    logical_and, // &&
    bitwise_or, // ||
    bitwise_xor, // ^
    bitwise_and, // &
    equality, // == !=
    comparison, // < <= > >=
    shift, // << >>
    term, // + -
    factor, // * / %
    lhs_unary, // + - ~ ! ++ --
    rhs_unary, // ++ --
    primary,

    pub fn gt(self: Precedence, other: Precedence) bool {
        return @intFromEnum(self) > @intFromEnum(other);
    }

    /// return one higher precedence
    pub fn inc(self: Precedence) Precedence {
        return @enumFromInt(@intFromEnum(self) + 1);
    }
};

const PrecedenceRule = struct {
    prefix: *const fn (parser: *Parser) AllocErr!Expr = no_unary,
    infix: *const fn (parser: *Parser, lhs: *Expr) AllocErr!Expr = no_binary,
    prec: Precedence = .none,

    fn no_unary(parser: *Parser) AllocErr!Expr {
        return .{ .err = .{ .token_not_operator = parser.current.? } };
    }

    fn no_binary(parser: *Parser, lhs: *Expr) AllocErr!Expr {
        _ = lhs;
        return .{ .err = .{ .token_not_operator = parser.current.? } };
    }
};

const precedence_rules = std.EnumArray(TokenKind, PrecedenceRule).initDefault(.{}, .{
    .lparen = .{ .prefix = group },
    .int_lit = .{ .prefix = intLitExpr, .prec = .primary },
    .identifier = .{ .prefix = variable, .prec = .primary },
    .tilde = .{ .prefix = leftUnary, .prec = .lhs_unary },
    .minus = .{ .prefix = leftUnary, .infix = binary, .prec = .term },
    .plus = .{ .prefix = leftUnary, .infix = binary, .prec = .term },
    .asterisk = .{ .infix = binary, .prec = .factor },
    .slash = .{ .infix = binary, .prec = .factor },
    .percent = .{ .infix = binary, .prec = .factor },
    .amp = .{ .infix = binary, .prec = .bitwise_and },
    .verbar = .{ .infix = binary, .prec = .bitwise_or },
    .hat = .{ .infix = binary, .prec = .bitwise_xor },
    .lt_lt = .{ .infix = binary, .prec = .shift },
    .gt_gt = .{ .infix = binary, .prec = .shift },
    .excl = .{ .prefix = leftUnary, .prec = .lhs_unary },
    .amp_amp = .{ .infix = binary, .prec = .logical_and },
    .verbar_verbar = .{ .infix = binary, .prec = .logical_or },
    .equals_equals = .{ .infix = binary, .prec = .equality },
    .excl_equals = .{ .infix = binary, .prec = .equality },
    .lt = .{ .infix = binary, .prec = .comparison },
    .gt = .{ .infix = binary, .prec = .comparison },
    .lt_equals = .{ .infix = binary, .prec = .comparison },
    .gt_equals = .{ .infix = binary, .prec = .comparison },
    .equals = .{ .infix = assignment, .prec = .assignment },
    .plus_equals = .{ .infix = assignment, .prec = .assignment },
    .minus_equals = .{ .infix = assignment, .prec = .assignment },
    .asterisk_equals = .{ .infix = assignment, .prec = .assignment },
    .slash_equals = .{ .infix = assignment, .prec = .assignment },
    .percent_equals = .{ .infix = assignment, .prec = .assignment },
    .amp_equals = .{ .infix = assignment, .prec = .assignment },
    .verbar_equals = .{ .infix = assignment, .prec = .assignment },
    .hat_equals = .{ .infix = assignment, .prec = .assignment },
    .lt_lt_equals = .{ .infix = assignment, .prec = .assignment },
    .gt_gt_equals = .{ .infix = assignment, .prec = .assignment },
    .plus_plus = .{ .prefix = leftUnary, .infix = rhsUnary, .prec = .rhs_unary },
    .minus_minus = .{ .prefix = leftUnary, .infix = rhsUnary, .prec = .rhs_unary },
    .quest = .{ .infix = ternary, .prec = .ternary },
});

fn peekPrecedenceRule(self: *Parser) Error!PrecedenceRule {
    const tok = self.current orelse return Error.eoi;
    return precedence_rules.get(tok.variant.kind());
}

fn lastLocation(self: *Parser) Location {
    return self.lexer.current_location;
}

fn parsePrecedence(self: *Parser, precedence: Precedence) AllocErr!Expr {
    const rule = self.peekPrecedenceRule() catch return self.eoi(Expr);

    var lhs = try rule.prefix(self);
    if (lhs == .err) return lhs;

    while (true) {
        const rule_next = self.peekPrecedenceRule() catch return self.eoi(Expr);

        if (precedence.gt(rule_next.prec)) break;

        lhs = try rule_next.infix(self, try self.onHeap(lhs));
        if (lhs == .err) return lhs;
    }

    return lhs;
}

fn expression(self: *Parser) AllocErr!Expr {
    return try self.parsePrecedence(@enumFromInt(1));
}

fn group(self: *Parser) AllocErr!Expr {
    self.consume(.lparen) catch @panic("LPAREN");

    const expr = try self.expression();

    self.consume(.rparen) catch |_err| return self.err(Expr, _err, .{ .unexpected_token = .rparen });

    return expr;
}

fn intLitExpr(self: *Parser) AllocErr!Expr {
    const tok = self.expect(.int_lit) catch @panic("INT_LIT");

    return .{ .int_lit = .{
        .value = tok.variant.int_lit,
        .span = tok.span,
    } };
}

fn leftUnary(self: *Parser) AllocErr!Expr {
    const operator = self.expectOperator(&.{
        .tilde,
        .plus,
        .minus,
        .excl,
        .plus_plus,
        .minus_minus,
    }) catch |_err| return self.err(Expr, _err, .{ .expected_operator = .prefix_unary });

    const rhs = try self.onHeap(try self.parsePrecedence(.lhs_unary));

    return .{ .lhs_unary = .{
        .operator = operator,
        .rhs = rhs,
    } };
}

fn rhsUnary(self: *Parser, lhs: *Expr) AllocErr!Expr {
    const operator = self.expectOperator(&.{
        .plus_plus,
        .minus_minus,
    }) catch |_err| return self.err(Expr, _err, .{ .expected_operator = .suffix_unary });

    return .{ .rhs_unary = .{
        .operator = operator,
        .lhs = lhs,
    } };
}

fn binary(self: *Parser, lhs: *Expr) AllocErr!Expr {
    const operator = self.expectOperator(&.{
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
    }) catch |_err| return self.err(Expr, _err, .{ .expected_operator = .binary });

    const prec = precedence_rules.get(operator.@"0").prec.inc();
    const rhs = try self.onHeap(try self.parsePrecedence(prec));

    return .{ .binary = .{
        .operator = operator,
        .lhs = lhs,
        .rhs = rhs,
    } };
}

fn assignment(self: *Parser, lhs: *Expr) AllocErr!Expr {
    const operator = self.expectOperator(&.{
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
    }) catch |_err| return self.err(Expr, _err, .{ .expected_operator = .assignment });

    const prec = precedence_rules.get(operator.@"0").prec;
    const rhs = try self.onHeap(try self.parsePrecedence(prec));

    return .{ .assignment = .{
        .operator = operator,
        .lhs = lhs,
        .rhs = rhs,
    } };
}

fn variable(self: *Parser) AllocErr!Expr {
    const name = self.expectIdentifier() catch |_err| {
        if (_err == AllocErr.OutOfMemory) return @errorCast(_err);
        @panic("VAR_NAME");
    };
    return .{ .variable = name };
}

fn ternary(self: *Parser, cond: *Expr) AllocErr!Expr {
    self.consume(.quest) catch @panic("?");

    const left = try self.onHeap(try self.expression());

    self.consume(.colon) catch |_err| return self.err(Expr, _err, .{ .unexpected_token = .colon });

    const right = try self.onHeap(try self.parsePrecedence(.ternary));

    return .{ .conditional = .{
        .cond = cond,
        .if_true = left,
        .if_false = right,
    } };
}

pub const Error = error{
    unexpected,
    lexer_error,
    eoi,
};

fn expectOperator(self: *Parser, kinds: []const TokenKind) Error!LocatedTokenKind {
    const tok = self.current orelse return Error.eoi;
    if (tok.variant == .err) return Error.lexer_error;

    if (!oneOf(tok.variant.kind(), kinds)) return Error.unexpected;
    _ = self.advance();

    return .{ tok.variant.kind(), tok.span };
}

fn expect(self: *Parser, kind: TokenKind) Error!Token {
    const tok = self.current orelse return Error.eoi;

    if (tok.variant != kind) return Error.unexpected;

    return self.advance().?;
}

fn expectIdentifier(self: *Parser) (Error || AllocErr)!AST.Identifier {
    const tok = try self.expect(.identifier);
    const lexeme = try self.allocator.dupe(u8, tok.variant.identifier);
    return .{
        .name = lexeme,
        .span = tok.span,
    };
}

fn consume(self: *Parser, kind: TokenKind) Error!void {
    _ = try self.expect(kind);
}

fn advance(self: *Parser) ?Token {
    const current = self.current;

    self.current = self.peek;
    self.peek = self.lexer.next();

    return current;
}

fn match(self: *Parser, kind: TokenKind) ?Token {
    if (self.current) |cur|
        if (cur.variant == kind)
            return self.advance();

    return null;
}

fn curKind(self: *Parser) ?TokenKind {
    return if (self.current) |current| current.variant.kind() else null;
}

fn peekKind(self: *Parser) ?TokenKind {
    return if (self.peek) |peek| peek.variant.kind() else null;
}

const onHeap = @import("utils.zig").onHeap;
