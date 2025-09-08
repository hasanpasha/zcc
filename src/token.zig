const std = @import("std");
const Writer = std.Io.Writer;

const Location = @import("Location.zig");

pub const LocatedToken = struct { Token, Location };

pub const LocatedTokenKind = struct { TokenKind, Location };

pub const TokenKind = enum {
    // identifiers
    identifier,
    int_lit,

    // symbols
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
    @"switch",
    case,
    default,

    // keywords
    int,
    void,
    @"return",
    @"if",
    @"else",
    goto,
    do,
    @"while",
    @"for",
    @"break",
    @"continue",
};

pub const Token = union(TokenKind) {
    identifier: []const u8,
    int_lit: u128,

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
    @"switch",
    case,
    default,

    int,
    void,
    @"return",
    @"if",
    @"else",
    goto,
    do,
    @"while",
    @"for",
    @"break",
    @"continue",

    pub fn kind(self: Token) TokenKind {
        return std.meta.activeTag(self);
    }

    pub fn format(self: @This(), writer: *Writer) Writer.Error!void {
        switch (self) {
            .identifier => |string| try writer.print("identifier(\"{s}\")", .{string}),
            .int_lit => |value| try writer.print("int_lit({})", .{value}),
            inline else => |_, tag| try writer.print("{s}", .{@tagName(tag)}),
        }
    }
};
