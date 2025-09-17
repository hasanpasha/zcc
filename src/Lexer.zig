const std = @import("std");
const Writer = std.Io.Writer;
const Token = @import("Token.zig");
const TokenVariant = Token.TokenVariant;
const TokenKind = Token.TokenKind;
const Location = @import("Location.zig");

const oneOf = @import("utils.zig").oneOf;

src: []const u8,
start_position: usize = 0,
current_position: usize = 0,
start_location: Location = .start,
current_location: Location = .start,
ch: ?u8 = null,
allocator: ?std.mem.Allocator = null,

const Lexer = @This();

pub fn new(filepath: []const u8, allocator: std.mem.Allocator) !Lexer {
    const file = try std.fs.cwd().openFile(filepath, .{});
    defer file.close();

    const source_code = try file.readToEndAlloc(allocator, 65000);

    var self = Lexer{
        .src = source_code,
        .allocator = allocator,
    };

    self.init();

    return self;
}

fn init(self: *Lexer) void {
    if (self.src.len > 0)
        self.ch = self.src[self.current_position];
}

pub fn deinit(self: Lexer) void {
    if (self.allocator) |alloc| {
        alloc.free(self.src);
    }
}

pub fn next(self: *Lexer) ?Token {
    self.start_position = self.current_position;
    self.start_location = self.current_location;
    defer {
        self.start_position = self.current_position;
        self.start_location = self.current_location;
    }

    const ch = self.ch orelse return null;

    const variant = switch (ch) {
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
        else => variant: {
            if (std.ascii.isWhitespace(ch)) {
                self.skipWhitespace();
                return self.next();
            }

            if (isIdentifierStart(ch))
                break :variant self.lexIdentifier();

            if (std.ascii.isDigit(ch))
                break :variant self.lexNumber();

            break :variant self.advanceWith(.{ .err = .{ .unrecognized_char = ch } });
        },
    };

    return .{
        .variant = variant,
        .lexeme = self.currentLexeme(),
        .span = .{
            self.start_location,
            self.current_location,
        },
    };
}

fn currentLexeme(self: *Lexer) []const u8 {
    return self.src[self.start_position..self.current_position];
}

fn lexNumber(self: *Lexer) TokenVariant {
    const lexeme = self.readWhile(std.ascii.isDigit);
    const number = std.fmt.parseUnsigned(u128, lexeme, 10) catch
        return .{ .err = .{ .large_number = lexeme } };

    const suffix = self.readWhile(std.ascii.isAlphanumeric);
    if (suffix.len > 0)
        return .{ .err = .{ .invalid_suffix = suffix } };

    return .{ .int_lit = number };
}

const keywordsMap = std.StaticStringMap(TokenVariant).initComptime(.{
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

fn lexIdentifier(self: *Lexer) TokenVariant {
    const lexeme = self.readWhile(isIdentifier);
    return keywordsMap.get(lexeme) orelse .{ .identifier = lexeme };
}

fn isIdentifierStart(ch: u8) bool {
    return std.ascii.isAlphabetic(ch) or ch == '_';
}

fn isIdentifier(ch: u8) bool {
    return isIdentifierStart(ch) or std.ascii.isAlphanumeric(ch);
}

fn readWhile(self: *Lexer, pred: fn (u8) bool) []const u8 {
    const start_pos = self.current_position;
    self.seek(pred);
    return self.src[start_pos..self.current_position];
}

fn skipWhitespace(self: *Lexer) void {
    self.seek(std.ascii.isWhitespace);
}

fn advanceWith(self: *Lexer, tok: TokenVariant) TokenVariant {
    self.advance();
    return tok;
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
        self.current_location.line += 1;
        self.current_location.column = 1;
    } else {
        self.current_location.column += 1;
    }

    self.ch = self.peekChar();
    self.current_position += 1;
}

fn peekChar(self: Lexer) ?u8 {
    if (self.current_position + 1 >= self.src.len)
        return null;

    return self.src[self.current_position + 1];
}

fn match(self: *Lexer, ch: u8) bool {
    if (self.peekChar() == ch) {
        self.advance();
        return true;
    }
    return false;
}

test Lexer {
    const expected_toks = [_]TokenVariant{
        .{ .err = .{ .large_number = "10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" } },
        .{ .int_lit = 0 },
        .{ .err = .{ .unrecognized_char = '`' } },
        .{ .int_lit = 0 },
        .{ .err = .{ .invalid_suffix = "x1010" } },
        .{ .identifier = "main" }, .{ .identifier = "__name__" }, .{ .int_lit = 43 }, //
        .lparen,        .rparen,     .lcub,          .rcub,          .semicolon,       .tilde,        .minus,          .plus, //
        .asterisk,      .slash,      .percent,       .amp,           .verbar,          .hat,          .lt_lt,          .gt_gt,
        .excl,          .amp_amp,    .verbar_verbar, .equals_equals, .excl_equals,     .lt,           .gt,             .lt_equals,
        .gt_equals,     .equals,     .plus_equals,   .minus_equals,  .asterisk_equals, .slash_equals, .percent_equals, .amp_equals,
        .verbar_equals, .hat_equals, .lt_lt_equals,  .gt_gt_equals,  .plus_plus,       .minus_minus,  .colon,          .quest,
        .int,           .void,       .@"return",     .@"if",         .@"else",         .goto,         .do,             .@"while",
        .@"for",        .@"break",   .@"continue",   .@"switch",     .case,            .default,
    };

    const code =
        \\ 10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
        \\ 000000000000000000000000000000000000000000
        \\ ` 00 00x1010
        \\main __name__ 43
        \\(){};~-+*/%&|^<<>>!&&||==!=<><=>==+=-=*=/=%=&=|=^=<<=>>=++--:? 
        \\int void return if else goto do while for break continue switch case default
    ;

    var lexer = Lexer{ .src = code };
    lexer.init();

    var i: usize = 0;
    while (lexer.next()) |ltoken| : (i += 1) {
        const this_tok = ltoken.variant;
        try std.testing.expect(i < expected_toks.len);

        const expected_tok = expected_toks[i];
        try std.testing.expectEqual(expected_tok.kind(), this_tok.kind());
        switch (this_tok) {
            .identifier => |iden| try std.testing.expectEqualStrings(expected_tok.identifier, iden),
            .int_lit => |val| try std.testing.expectEqual(expected_tok.int_lit, val),
            .err => |err| {
                const expected_err = expected_tok.err;
                try std.testing.expectEqual(std.meta.activeTag(err), std.meta.activeTag(expected_err));
                switch (err) {
                    .invalid_suffix => |suffix| try std.testing.expectEqualStrings(expected_err.invalid_suffix, suffix),
                    .large_number => |number| try std.testing.expectEqualStrings(expected_err.large_number, number),
                    .unrecognized_char => |char| try std.testing.expectEqual(expected_err.unrecognized_char, char),
                }
            },
            else => try std.testing.expectEqual(expected_toks[i], this_tok),
        }
    }
    try std.testing.expect(i == expected_toks.len);
}
