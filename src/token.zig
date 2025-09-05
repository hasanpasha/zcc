const std = @import("std");

const Location = @import("Location.zig");

pub const LocatedToken = struct { Token, Location };

pub const LocatedSymbol = struct { Symbol, Location };

pub const LocatedKeyword = struct { Keyword, Location };

pub const Keyword = enum {
    int,
    void,
    @"return",
    @"if",
    @"else",
    goto,
};

pub const Symbol = enum {
    lparen,
    rparen,
    lcub,
    rcub,
    semicolon,
    tilde,
    minus,
    plus,
    asterisk,
    slash,
    percent,
    amp,
    verbar,
    hat,
    lt_lt,
    gt_gt,
    excl,
    amp_amp,
    verbar_verbar,
    equals_equals,
    excl_equals,
    lt,
    gt,
    lt_equals,
    gt_equals,
    equals,
    plus_equals,
    minus_equals,
    asterisk_equals,
    slash_equals,
    percent_equals,
    amp_equals,
    verbar_equals,
    hat_equals,
    lt_lt_equals,
    gt_gt_equals,
    plus_plus,
    minus_minus,
    colon,
    quest,

    pub fn string(self: Symbol) []const u8 {
        return switch (self) {
            .lparen => "(",
            .rparen => ")",
            .lcub => "{",
            .rcub => "}",
            .semicolon => ";",
            .tilde => "~",
            .minus => "-",
            .plus => "+",
            .asterisk => "*",
            .slash => "/",
            .percent => "%",
            .amp => "&",
            .verbar => "|",
            .hat => "^",
            .lt_lt => "<<",
            .gt_gt => ">>",
            .excl => "!",
            .amp_amp => "&&",
            .verbar_verbar => "||",
            .equals_equals => "==",
            .excl_equals => "!=",
            .lt => "<",
            .gt => ">",
            .lt_equals => "<=",
            .gt_equals => ">=",
            .equals => "=",
            .plus_equals => "+=",
            .minus_equals => "-=",
            .asterisk_equals => "*=",
            .slash_equals => "/=",
            .percent_equals => "%=",
            .amp_equals => "&=",
            .verbar_equals => "|=",
            .hat_equals => "^=",
            .lt_lt_equals => "<<=",
            .gt_gt_equals => ">>=",
            .plus_plus => "++",
            .minus_minus => "--",
            .colon => ":",
            .quest => "?",
        };
    }
};

pub const TokenKind = enum {
    identifier,
    int_lit,
    symbol,
    keyword,
};

pub const Token = union(TokenKind) {
    identifier: []const u8,
    int_lit: u128,
    symbol: Symbol,
    keyword: Keyword,

    pub fn kind(self: Token) TokenKind {
        return std.meta.activeTag(self);
    }

    pub fn eql(self: Token, other: Token) bool {
        return std.meta.eql(self, other);
    }

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self) {
            .identifier => |string| try writer.print("identifier(\"{s}\")", .{string}),
            .int_lit => |value| try writer.print("int_lit({})", .{value}),
            .symbol => |symbol| try writer.print("symbol({s})", .{@tagName(symbol)}),
            .keyword => |keyword| try writer.print("keyword({s})", .{@tagName(keyword)}),
        }
    }
};
