const std = @import("std");
const Writer = std.Io.Writer;

const Location = @import("Location.zig");

variant: TokenVariant,
lexeme: []const u8,
span: Span,

pub const Span = struct {
    Location,
    Location,
};

pub const TokenKind = enum {
    // error
    err,

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

pub const Error = union(enum) {
    large_number: []const u8,
    invalid_suffix: []const u8,
    unrecognized_char: u8,

    pub fn format(self: @This(), writer: *Writer) Writer.Error!void {
        switch (self) {
            .large_number => |number| try writer.print("large_number({s})", .{number}),
            .invalid_suffix => |suffix| try writer.print("invalid_suffix({s})", .{suffix}),
            .unrecognized_char => |char| try writer.print("unrecognized_char({c})", .{char}),
        }
    }
};

pub const TokenVariant = union(TokenKind) {
    // error
    err: Error,

    // identifiers
    identifier: []const u8,
    int_lit: u128,

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

    pub fn kind(self: TokenVariant) TokenKind {
        return std.meta.activeTag(self);
    }

    pub fn format(self: @This(), writer: *Writer) Writer.Error!void {
        switch (self) {
            .identifier => |string| try writer.print("identifier(\"{s}\")", .{string}),
            .int_lit => |value| try writer.print("int_lit({})", .{value}),
            .err => |err| try writer.print("err({f})", .{err}),
            inline else => |_, tag| try writer.print("{s}", .{@tagName(tag)}),
        }
    }
};
