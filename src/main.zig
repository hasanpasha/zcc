const std = @import("std");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const TackyIRGenerator = @import("TackyIRGenerator.zig");
const semantic_analyzer = @import("semantic_analyzer/root.zig");
const asm_generator = @import("asm_generator.zig");

const exit = std.process.exit;

const clap = @import("clap");

const VERSION = "0.0.1";

const Options = struct {
    verbose: ?bool = null,
    lex: bool = false,
    parse: bool = false,
    validate: bool = false,
    tacky: bool = false,
    codegen: bool = false,
    filepath: []const u8,

    pub fn get(allocator: std.mem.Allocator) Options {
        const params = comptime clap.parseParamsComptime(
            \\-h, --help            Display this help and exit.
            \\--version             Print version and exit.
            \\-v, --verbose         An optiona to log all infos.
            \\--lex                 Lex only and exit.
            \\--parse               Parse only and exit.
            \\--validate            Validate the parsed program and exit.
            \\--tacky               Generate tacky ir and exit.
            \\--codegen             Generate asm and exit without emiting file.
            \\<FILE>
        );

        const parsers = comptime .{
            .FILE = clap.parsers.string,
        };

        var diag = clap.Diagnostic{};
        var res = clap.parse(clap.Help, &params, parsers, .{
            .diagnostic = &diag,
            .allocator = allocator,
        }) catch |err| {
            diag.reportToFile(.stderr(), err) catch {};
            exit(1);
        };
        defer res.deinit();

        var self: Options = undefined;

        if (res.args.help != 0) help(params, null);
        if (res.args.version != 0) version();

        if (res.args.verbose != 0) self.verbose = true;
        if (res.args.lex != 0) self.lex = true;
        if (res.args.parse != 0) self.parse = true;
        if (res.args.validate != 0) self.validate = true;
        if (res.args.tacky != 0) self.tacky = true;
        if (res.args.codegen != 0) self.codegen = true;

        self.filepath = res.positionals[0] orelse help(params, "no input");

        return self;
    }

    pub fn version() noreturn {
        std.fs.File.stdout().writeAll(VERSION) catch {};
        std.fs.File.stdout().writeAll("\n") catch {};
        std.process.exit(0);
    }

    fn help(params: anytype, error_message: ?[]const u8) noreturn {
        if (error_message) |msg| {
            clap.helpToFile(.stderr(), clap.Help, &params, .{ .indent = 0 }) catch {};
            std.fs.File.stderr().writeAll(msg) catch {};
            std.fs.File.stderr().writeAll("\n") catch {};
            std.process.exit(1);
        }

        clap.helpToFile(.stdout(), clap.Help, &params, .{ .indent = 0 }) catch {};
        std.process.exit(0);
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer if (gpa.deinit() == .leak) @panic("memory leak");

    const allocator = gpa.allocator();

    const options = Options.get(allocator);

    const processed_file = try replaceExtension(options.filepath, ".cc", allocator);
    defer allocator.free(processed_file);

    var pre_child = std.process.Child.init(&.{ "gcc", "-E", "-P", options.filepath, "-o", processed_file }, allocator);
    const prepocess_res = try pre_child.spawnAndWait();
    switch (prepocess_res) {
        .Exited => |code| if (code != 0) exit(code),
        inline else => |code| exit(@truncate(code)),
    }

    const file = try std.fs.cwd().openFile(processed_file, .{});
    defer file.close();
    const source = try file.readToEndAlloc(allocator, 65000);
    defer allocator.free(source);

    var lexer = Lexer.init(source);

    if (options.lex) {
        while (lexer.next().unwrap()) |ltok| {
            const tok, const loc = ltok;
            std.log.debug("{s}:{}:{} {f}", .{ processed_file, loc.line, loc.column, tok });
        }
        exit(0);
    }

    var ast = Parser.parse(lexer, allocator).unwrap();
    defer ast.free();

    if (options.parse) {
        std.log.info("{f}", .{std.fmt.alt(ast, .prettyFmt)});
        exit(0);
    }

    var validated_ast = semantic_analyzer.analyze(ast, allocator).unwrap();
    defer validated_ast.free();

    if (options.validate) {
        std.log.info("{f}", .{validated_ast});
        exit(0);
    }

    var tir = TackyIRGenerator.lower(validated_ast, allocator);
    defer tir.free();

    if (options.tacky) {
        std.log.info("{f}", .{std.fmt.alt(tir, .prettyFmt)});
        exit(0);
    }

    var air = asm_generator.lower(tir, .x86_64, allocator);
    defer air.free();

    if (options.codegen) {
        std.log.info("{f}", .{air});
        exit(0);
    }

    const asm_file = try replaceExtension(options.filepath, ".s", allocator);
    defer allocator.free(asm_file);
    try air.emitToFile(asm_file);

    const exe_file = try replaceExtension(options.filepath, "", allocator);
    defer allocator.free(exe_file);

    var child = std.process.Child.init(&.{ "gcc", asm_file, "-o", exe_file }, allocator);
    const result = try child.spawnAndWait();
    switch (result) {
        .Exited => |code| exit(code),
        inline else => |_| {},
    }
}

fn replaceExtension(str: []const u8, ext: []const u8, allocator: std.mem.Allocator) std.mem.Allocator.Error![]u8 {
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
