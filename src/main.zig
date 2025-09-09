const std = @import("std");

const driver = @import("driver.zig");
const CFile = driver.CFile;
const TranslationUnit = driver.TranslationUnit;
const AsmFile = driver.AsmFile;
const ObjectFile = driver.ObjectFile;

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
    compile: bool = false,
    assemble: bool = false,
    output: ?[]const u8 = null,
    cfiles: []const CFile,

    arena: std.heap.ArenaAllocator,

    pub fn deinit(self: Options) void {
        self.arena.deinit();
    }

    pub fn get(allocator: std.mem.Allocator) !Options {
        const params = comptime clap.parseParamsComptime(
            \\-h, --help                Display this help and exit.
            \\--version                 Print version and exit.
            \\-v, --verbose             An optiona to log all infos.
            \\--lex                     Lex only and exit.
            \\--parse                   Parse only and exit.
            \\--validate                Validate the parsed program and exit.
            \\--tacky                   Generate tacky ir and exit.
            \\--codegen                 Generate asm and exit without emiting file.
            \\-c, --compile             Compile only without linking
            \\-S, --assemble            Assemble only and exit
            \\-o, --output <FILE>       Specify output path
            \\<FILE>...
        );

        const parsers = comptime .{
            .FILE = clap.parsers.string,
        };

        var arena = std.heap.ArenaAllocator.init(allocator);

        var diag = clap.Diagnostic{};
        const res = clap.parse(clap.Help, &params, parsers, .{
            .diagnostic = &diag,
            .allocator = arena.allocator(),
        }) catch |err| {
            diag.reportToFile(.stderr(), err) catch {};
            exit(1);
        };

        var self: Options = undefined;

        self.arena = arena;

        if (res.args.help != 0) help(params, null);
        if (res.args.version != 0) version();

        if (res.args.verbose != 0) self.verbose = true;
        if (res.args.lex != 0) self.lex = true;
        if (res.args.parse != 0) self.parse = true;
        if (res.args.validate != 0) self.validate = true;
        if (res.args.tacky != 0) self.tacky = true;
        if (res.args.compile != 0) self.compile = true;
        if (res.args.assemble != 0) self.assemble = true;

        self.output = res.args.output;

        if (res.args.codegen != 0) self.codegen = true;

        const paths: []const []const u8 = res.positionals.@"0";

        var cfiles = try std.ArrayList(CFile).initCapacity(arena.allocator(), 128);
        for (paths) |path|
            try cfiles.append(allocator, .{ .path = path });

        self.cfiles = try cfiles.toOwnedSlice(arena.allocator());

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

    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();

    const allocator = arena.allocator();

    const options = try Options.get(allocator);
    defer options.deinit();

    var units = try allocator.alloc(TranslationUnit, options.cfiles.len);
    defer for (units) |*unit| unit.deleter.delete() catch @panic("Can't delete");

    for (options.cfiles, 0..) |cfile, i|
        units[i] = try cfile.toTranslationUnit(.x86_64, allocator);

    var asm_files = try allocator.alloc(AsmFile, units.len);
    defer if (!options.assemble) for (asm_files) |*asm_file|
        asm_file.delete() catch @panic("Can't delete");

    for (units, 0..) |unit, i| {
        if (options.lex) {
            var lexer = try unit.lex(allocator);
            defer lexer.deinit();

            while (lexer.next().unwrap()) |ltok| {
                const tok, const loc = ltok;
                std.debug.print("{s}:{}:{}: {f}\n", .{
                    unit.path,
                    loc.line,
                    loc.column,
                    tok,
                });
            }
            return;
        }

        if (options.parse) {
            var ast = try unit.parse(allocator);
            defer ast.free();
            std.debug.print("{f}\n", .{ast});
            return;
        }

        if (options.validate) {
            var pir = try unit.validate(allocator);
            defer pir.free();
            std.debug.print("{f}\n", .{pir});
            return;
        }

        if (options.tacky) {
            var tir = try unit.tacky(allocator);
            defer tir.free();
            std.debug.print("{f}\n", .{tir});
            return;
        }

        if (options.codegen) {
            var pir = try unit.codegen(allocator);
            defer pir.free();
            std.debug.print("{f}\n", .{pir});
            return;
        }

        asm_files[i] = try unit.toAsmFile(allocator);
    }

    if (options.assemble) return;

    var objects = try allocator.alloc(ObjectFile, asm_files.len);
    defer if (!options.compile) for (objects) |*object|
        object.deleter.delete() catch @panic("Can't delete");

    for (asm_files, 0..) |asm_file, i|
        objects[i] = try asm_file.toObjectFile(allocator);

    if (options.compile) return;

    const exe_output = options.output orelse try driver.replaceExtension(options.cfiles[0].path, "", allocator);
    defer if (options.output == null) allocator.free(exe_output);

    try driver.link(objects, exe_output, allocator);
}
