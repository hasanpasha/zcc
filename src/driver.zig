const std = @import("std");

const Lexer = @import("Lexer.zig");
const AST = @import("AST.zig");
const Parser = @import("Parser.zig");
const semantic_analyzer = @import("semantic_analyzer/root.zig");
const PIR = @import("PIR.zig");
const Stripper = @import("Stripper.zig");
const TackyIR = @import("TackyIR.zig");
const TackyIRGenerator = @import("TackyIRGenerator.zig");
const asm_generator = @import("asm_generator.zig");
const AIR = asm_generator.AIR;
const Arch = asm_generator.Arch;

const verbose = @import("main.zig").verbose;

pub fn replaceExtension(str: []const u8, ext: []const u8, allocator: std.mem.Allocator) std.mem.Allocator.Error![]u8 {
    var output = std.array_list.Managed(u8).init(allocator);

    var iter = std.mem.splitScalar(u8, str, '.');

    while (iter.next()) |part| {
        if (iter.peek() == null) {
            try output.appendSlice(ext);
        } else {
            try output.appendSlice(part);
        }
    }

    return try output.toOwnedSlice();
}

const Error = error{non_zero};

pub fn system(argv: []const []const u8, allocator: std.mem.Allocator) !void {
    var pre_child = std.process.Child.init(argv, allocator);
    const prepocess_res = try pre_child.spawnAndWait();
    switch (prepocess_res) {
        .Exited => |code| if (code != 0) return Error.non_zero,
        inline else => |code| std.process.exit(@truncate(code)),
    }
}

pub const CFile = struct {
    path: []const u8,

    pub fn toTranslationUnit(self: CFile, arch: Arch, allocator: std.mem.Allocator) !TranslationUnit {
        const output = try replaceExtension(self.path, ".cc", allocator);
        try system(&.{ "gcc", "-E", "-P", self.path, "-o", output }, allocator);

        return .{ .path = output, .arch = arch };
    }
};

pub fn Deleter(T: type) type {
    return struct {
        pub fn delete(self: *@This()) std.fs.Dir.DeleteFileError!void {
            const parent: *T = @alignCast(@fieldParentPtr("deleter", self));
            try std.fs.cwd().deleteFile(parent.path);
        }
    };
}

pub const TranslationUnit = struct {
    path: []const u8,
    arch: Arch,
    deleter: Deleter(@This()) = .{},

    pub fn lex(self: TranslationUnit, allocator: std.mem.Allocator) !Lexer {
        return try Lexer.new(self.path, allocator);
    }

    pub fn strip(ast: AST, allocator: std.mem.Allocator) !PIR {
        var buffer: [1024]u8 = undefined;
        const stdout_file = std.fs.File.stderr();
        var stdout_writer = stdout_file.writer(&buffer);
        const stdout = &stdout_writer.interface;

        const pir = Stripper.strip(ast, allocator, stdout) catch |err| {
            if (err == Stripper.Error.has_errors) std.process.exit(1);
            return err;
        };

        return pir;
    }

    pub fn parse(self: TranslationUnit, allocator: std.mem.Allocator) !AST {
        const lexer = try self.lex(allocator);
        // defer lexer.deinit();

        return try Parser.parse(lexer, allocator);
    }

    pub fn validate(self: TranslationUnit, allocator: std.mem.Allocator) !struct { AST, struct { usize, usize } } {
        var ast = try self.parse(allocator);
        const counters = try semantic_analyzer.analyze(&ast);
        return .{ ast, counters };
    }

    pub fn tacky(self: TranslationUnit, allocator: std.mem.Allocator) !TackyIR {
        const ast, const counter = try self.validate(allocator);
        // defer ast.free();

        const pir = try strip(ast, allocator);

        return TackyIRGenerator.lower(pir, allocator, counter);
    }

    pub fn codegen(self: TranslationUnit, allocator: std.mem.Allocator) !AIR {
        const tir = try self.tacky(allocator);
        // defer tir.free();

        return asm_generator.lower(tir, self.arch, allocator);
    }

    pub fn toAsmFile(self: TranslationUnit, allocator: std.mem.Allocator) !AsmFile {
        const air = try self.codegen(allocator);
        // defer air.free();

        const output = try replaceExtension(self.path, ".s", allocator);

        try air.emitToFile(output);

        return switch (self.arch) {
            .x86_64 => .{ .x86_64 = .{ .path = output } },
        };
    }
};

pub const AsmFile = union(Arch) {
    x86_64: X8664AsmFile,

    pub fn toObjectFile(self: AsmFile, allocator: std.mem.Allocator) !ObjectFile {
        return try switch (self) {
            inline else => |file| file.toObjectFile(allocator),
        };
    }

    pub fn delete(self: *AsmFile) !void {
        try switch (self.*) {
            inline else => |*file| file.deleter.delete(),
        };
    }
};

pub const X8664AsmFile = struct {
    path: []const u8,
    deleter: Deleter(@This()) = .{},

    pub fn toObjectFile(self: X8664AsmFile, allocator: std.mem.Allocator) !ObjectFile {
        const output = try replaceExtension(self.path, ".o", allocator);

        try system(&.{ "gcc", "-c", self.path, "-o", output }, allocator);

        return .{ .path = output, .arch = .x86_64 };
    }
};

pub const ObjectFile = struct {
    path: []const u8,
    arch: Arch,
    deleter: Deleter(@This()) = .{},
};

pub fn link(objects: []const ObjectFile, output: []const u8, allocator: std.mem.Allocator) !void {
    var argv = try std.ArrayList([]const u8).initCapacity(allocator, 128);
    defer argv.deinit(allocator);

    try argv.append(allocator, "gcc");
    for (objects) |object|
        try argv.append(allocator, object.path);
    try argv.appendSlice(allocator, &.{ "-o", output });

    try system(try argv.toOwnedSlice(allocator), allocator);
}
