const std = @import("std");
const token = @import("token.zig");
const TokenKind = token.TokenKind;
const Token = token.Token;
const Location = @import("Location.zig");

const Result = @import("result.zig").Result;

const oneOf = @import("utils.zig").oneOf;

src: []const u8,
position: usize = 0,
location: Location = .start,
ch: ?u8 = null,

const Lexer = @This();

pub const Error = union(enum) {
    unrecognized_character: UnrecognizedCharacterError,
    invalid_number: []const u8,

    pub const UnrecognizedCharacterError = struct {
        char: u8,
        location: Location,
    };

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self) {
            .unrecognized_character => |err| try writer.print("unrecognized character: '{c}' at {f}", .{
                err.char,
                err.location,
            }),
            .invalid_number => |lexeme| try writer.print("invalid number: '{s}'", .{lexeme}),
        }
    }
};

pub fn LexerResult(OkType: type) type {
    return Result(OkType, Error);
}

pub fn init(source_code: []const u8) Lexer {
    var self = Lexer{ .src = source_code };

    if (self.src.len > 0)
        self.ch = self.src[self.position];

    return self;
}

pub fn next(self: *Lexer) LexerResult(?token.LocatedToken) {
    const ch = self.ch orelse return .Ok(null);

    const start_loc = self.location;

    const tok_result = switch (ch) {
        '(' => self.advanceWith(.lparen),
        ')' => self.advanceWith(.rparen),
        '{' => self.advanceWith(.lcub),
        '}' => self.advanceWith(.rcub),
        ';' => self.advanceWith(.semicolon),
        '~' => self.advanceWith(.tilde),
        '-' => self.advanceWith(if (self.match('-')) .minus_minus else if (self.match('=')) .minus_equals else .minus),
        '+' => self.advanceWith(if (self.match('+')) .plus_plus else if (self.match('=')) .plus_equals else .plus),
        '*' => self.advanceWith(if (self.match('=')) .asterisk_equals else .asterisk),
        '/' => self.advanceWith(if (self.match('=')) .slash_equals else .slash),
        '%' => self.advanceWith(if (self.match('=')) .percent_equals else .percent),
        '<' => self.advanceWith(if (self.match('<')) if (self.match('=')) .lt_lt_equals else .lt_lt else if (self.match('=')) .lt_equals else .lt),
        '>' => self.advanceWith(if (self.match('>')) if (self.match('=')) .gt_gt_equals else .gt_gt else if (self.match('=')) .gt_equals else .gt),
        '&' => self.advanceWith(if (self.match('&')) .amp_amp else if (self.match('=')) .amp_equals else .amp),
        '|' => self.advanceWith(if (self.match('|')) .verbar_verbar else if (self.match('=')) .verbar_equals else .verbar),
        '^' => self.advanceWith(if (self.match('=')) .hat_equals else .hat),
        '!' => self.advanceWith(if (self.match('=')) .excl_equals else .excl),
        '=' => self.advanceWith(if (self.match('=')) .equals_equals else .equals),
        ':' => self.advanceWith(.colon),
        '?' => self.advanceWith(.quest),
        else => token: {
            if (std.ascii.isWhitespace(ch)) {
                self.skipWhitespace();
                return self.next();
            }

            if (isIdentifierStart(ch))
                break :token self.lexIdentifier();

            if (std.ascii.isDigit(ch))
                break :token self.lexNumber();

            break :token LexerResult(Token).Err(.{
                .unrecognized_character = .{
                    .char = ch,
                    .location = self.location,
                },
            });
        },
    };

    return switch (tok_result) {
        .ok => |tok| .Ok(.{ tok, start_loc }),
        .err => |err| .Err(err),
    };
}

pub fn format(
    self: @This(),
    writer: *std.Io.Writer,
) std.Io.Writer.Error!void {
    try writer.print("Lexer{{ src: \"{s}\", position: {}, location: {f}, ch: '{c}' }}", .{
        self.src,
        self.position,
        self.location,
        self.ch orelse 0,
    });
}

fn lexNumber(self: *Lexer) LexerResult(Token) {
    const result = if (self.ch == '0' and self.peekChar() != null)
        switch (self.peekChar().?) {
            'x', 'X' => self.lexHexadecimalNumber(),
            'b', 'B' => self.lexBinaryNumber(),
            else => if (std.ascii.isDigit(self.peekChar().?)) self.lexOctalNumber() else self.lexDecimalNumber(),
        }
    else
        self.lexDecimalNumber();

    const tok = switch (result) {
        .ok => |val| val,
        .err => |err| return .Err(err),
    };

    if (oneOf(self.ch.?, "%^&*()-+=<>{}[];:,") or std.ascii.isWhitespace(self.ch.?)) {
        return .Ok(tok);
    } else {
        return .Err(.{ .invalid_number = "<invalid suffix>" });
    }
}

fn lexRadixNumber(self: *Lexer, radix: u8, predicate: fn (u8) bool) LexerResult(Token) {
    const lexeme = self.readWhile(predicate);

    if (lexeme.len == 0)
        return .Err(.{ .invalid_number = "<missing digits>" });

    const number = std.fmt.parseUnsigned(u128, lexeme, radix) catch {
        return .Err(.{ .invalid_number = lexeme });
    };
    return .Ok(.{ .int_lit = number });
}

fn lexHexadecimalNumber(self: *Lexer) LexerResult(Token) {
    self.advance(); // skip '0'
    self.advance(); // skip 'x'|'X'
    return self.lexRadixNumber(16, std.ascii.isHex);
}

fn lexOctalNumber(self: *Lexer) LexerResult(Token) {
    self.advance(); // skip '0'
    return self.lexRadixNumber(8, struct {
        pub fn cb(c: u8) bool {
            return c >= '0' and c <= '7';
        }
    }.cb);
}

fn lexBinaryNumber(self: *Lexer) LexerResult(Token) {
    self.advance(); // skip '0'
    self.advance(); // skip 'b'|'B'
    return self.lexRadixNumber(2, struct {
        pub fn cb(c: u8) bool {
            return c == '0' or c == '1';
        }
    }.cb);
}

fn lexDecimalNumber(self: *Lexer) LexerResult(Token) {
    const lexeme = self.readWhile(std.ascii.isDigit);
    const number = std.fmt.parseUnsigned(u128, lexeme, 10) catch {
        return .Err(.{ .invalid_number = lexeme });
    };
    return .Ok(.{ .int_lit = number });
}

const keywordsMap = std.StaticStringMap(Token).initComptime(.{
    .{ "int", .int },
    .{ "void", .void },
    .{ "return", .@"return" },
    .{ "if", .@"if" },
    .{ "else", .@"else" },
    .{ "goto", .goto },
    .{ "do", .do },
    .{ "while", .@"while" },
    .{ "for", .@"for" },
    .{ "break", .@"break" },
    .{ "continue", .@"continue" },
    .{ "switch", .@"switch" },
    .{ "case", .case },
    .{ "default", .default },
});

fn lexIdentifier(self: *Lexer) LexerResult(Token) {
    const lexeme = self.readWhile(isIdentifier);
    const tok = keywordsMap.get(lexeme) orelse Token{ .identifier = lexeme };
    return .Ok(tok);
}

fn isIdentifierStart(ch: u8) bool {
    return std.ascii.isAlphabetic(ch) or ch == '_';
}

fn isIdentifier(ch: u8) bool {
    return isIdentifierStart(ch) or std.ascii.isAlphanumeric(ch);
}

fn readWhile(self: *Lexer, pred: fn (u8) bool) []const u8 {
    const start_pos = self.position;
    self.seek(pred);
    return self.src[start_pos..self.position];
}

fn skipWhitespace(self: *Lexer) void {
    self.seek(std.ascii.isWhitespace);
}

fn advanceWith(self: *Lexer, tok: Token) LexerResult(Token) {
    self.advance();
    return .Ok(tok);
}

fn seek(self: *Lexer, pred: fn (u8) bool) void {
    while (self.ch) |ch| {
        if (!pred(ch))
            break;

        self.advance();
    }
}

fn advance(self: *Lexer) void {
    if (self.ch == null)
        return;

    if (self.ch.? == '\n') {
        self.location.line += 1;
        self.location.column = 1;
    } else {
        self.location.column += 1;
    }

    self.ch = self.peekChar();
    self.position += 1;
}

fn peekChar(self: Lexer) ?u8 {
    if (self.position + 1 >= self.src.len)
        return null;

    return self.src[self.position + 1];
}

fn match(self: *Lexer, ch: u8) bool {
    if (self.peekChar() == ch) {
        self.advance();
        return true;
    }
    return false;
}

test Lexer {
    const expected_toks = [_]Token{
        .{ .identifier = "main" }, .{ .int_lit = 43 },
        .lparen,        .rparen,     .lcub,          .rcub,          .semicolon,       .tilde,        .minus,          .plus, //
        .asterisk,      .slash,      .percent,       .amp,           .verbar,          .hat,          .lt_lt,          .gt_gt,
        .excl,          .amp_amp,    .verbar_verbar, .equals_equals, .excl_equals,     .lt,           .gt,             .lt_equals,
        .gt_equals,     .equals,     .plus_equals,   .minus_equals,  .asterisk_equals, .slash_equals, .percent_equals, .amp_equals,
        .verbar_equals, .hat_equals, .lt_lt_equals,  .gt_gt_equals,  .plus_plus,       .minus_minus,  .colon,          .quest,
        .int,           .void,       .@"return",     .@"if",         .@"else",         .goto,         .do,             .@"while",
        .@"for",        .@"break",   .@"continue",   .@"switch",     .case,            .default,
    };

    const code =
        \\main 43
        \\(){};~-+*/%&|^<<>>!&&||==!=<><=>==+=-=*=/=%=&=|=^=<<=>>=++--:? 
        \\int void return if else goto do while for break continue switch case default
    ;

    var lexer = init(code);

    var i: usize = 0;
    while (try lexer.next().toErrorUnion(error.lexer_error)) |ltoken| : (i += 1) {
        const this_tok, _ = ltoken;
        try std.testing.expect(i < expected_toks.len);

        const expected_tok = expected_toks[i];
        try std.testing.expectEqual(expected_tok.kind(), this_tok.kind());
        switch (this_tok) {
            .identifier => |iden| try std.testing.expectEqualStrings(expected_tok.identifier, iden),
            .int_lit => |val| try std.testing.expectEqual(expected_tok.int_lit, val),
            else => try std.testing.expectEqual(expected_toks[i], this_tok),
        }
    }
    try std.testing.expect(i == expected_toks.len);
}
