const std = @import("std");

const driver = @import("driver.zig");
const CFile = driver.CFile;
const TranslationUnit = driver.TranslationUnit;
const AsmFile = driver.AsmFile;
const ObjectFile = driver.ObjectFile;
const Stripper = @import("Stripper.zig");

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
    // var gpa = std.heap.GeneralPurposeAllocator(.{ .verbose_log = false }).init;
    // defer if (gpa.deinit() == .leak) @panic("memory leak");

    // var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    // defer arena.deinit();

    // const allocator = arena.allocator();
    // const allocator = gpa.allocator();
    const allocator = std.heap.page_allocator;

    const options = try Options.get(allocator);
    defer options.deinit();

    var units = try allocator.alloc(TranslationUnit, options.cfiles.len);
    // defer for (units) |*unit| unit.deleter.delete() catch @panic("Can't delete");

    for (options.cfiles, 0..) |cfile, i|
        units[i] = try cfile.toTranslationUnit(.x86_64, allocator);

    var asm_files = try allocator.alloc(AsmFile, units.len);
    // defer if (!options.assemble) for (asm_files) |*asm_file|
    //     asm_file.delete() catch @panic("Can't delete");

    for (units, 0..) |unit, i| {
        if (options.lex) {
            var lexer = try unit.lex(allocator);
            defer lexer.deinit();

            while (lexer.next()) |tok| {
                std.debug.print("{s}:{}:{}: {f}\n", .{
                    unit.path,
                    tok.span.@"0".line,
                    tok.span.@"1".column,
                    tok.variant,
                });
            }
            return;
        }

        if (options.parse) {
            const ast = try unit.parse(allocator);
            // defer ast.free();

            std.log.debug("{f}", .{ast});

            _ = try TranslationUnit.strip(ast, allocator);
            // std.log.debug("{f}", .{pir});
            return;
        }

        if (options.validate) {
            const ast, _ = try unit.validate(allocator);
            // defer ast.free();

            std.log.debug("{f}", .{ast});

            const pir = try TranslationUnit.strip(ast, allocator);
            std.log.debug("{f}", .{pir});
            return;
        }

        if (options.tacky) {
            const tir = try unit.tacky(allocator);
            // defer tir.free();
            std.debug.print("{f}\n", .{tir});
            return;
        }

        if (options.codegen) {
            const air = try unit.codegen(allocator);
            // defer pir.free();
            std.debug.print("{f}\n", .{air});
            return;
        }

        asm_files[i] = try unit.toAsmFile(allocator);
    }

    if (options.assemble) return;

    var objects = try allocator.alloc(ObjectFile, asm_files.len);
    // defer if (!options.compile) for (objects) |*object|
    //     object.deleter.delete() catch @panic("Can't delete");

    for (asm_files, 0..) |asm_file, i|
        objects[i] = try asm_file.toObjectFile(allocator);

    if (options.compile) return;

    const exe_output = options.output orelse try driver.replaceExtension(options.cfiles[0].path, "", allocator);
    defer if (options.output == null) allocator.free(exe_output);

    try driver.link(objects, exe_output, allocator);
}

// const AST = @import("AST.zig");
// fn hasErrors(ast: AST) bool {
//     var has = false;
//     for (ast.declarations) |ldecl| {
//         const decl, const span = ldecl;
//         std.log.debug("start: {}:{}, end: {}:{}", .{
//             span.@"0".line,
//             span.@"0".column,
//             span.@"1".line,
//             span.@"1".column,
//         });
//         if (declaration(decl)) {
//             has = true;
//             std.log.err("decl error: start at: {}:{} which ends at {}:{}", .{
//                 span.@"0".line,
//                 span.@"0".column,
//                 span.@"1".line,
//                 span.@"1".column,
//             });
//         }
//     }
//     return has;
// }

// fn function(func: AST.Declaration.Function) bool {
//     const has = block(func.body);
//     if (has) std.log.err("error in {s} function", .{func.name.name});
//     return has;
// }

// fn block(b: AST.Block) bool {
//     var has = false;
//     var num: usize = 0;
//     for (b.items) |item| {
//         const this_has = block_item(item);
//         if (this_has) {
//             has = true;
//             num += 1;
//         }
//     }
//     if (has) std.log.err("error in {} items", .{num});
//     return has;
// }

// fn block_item(b: AST.BlockItem) bool {
//     switch (b) {
//         .err => |err| {
//             std.log.err("{}", .{err});
//             return true;
//         },
//         .declaration => |decl| return declaration(decl.*),
//         .statement => |stmt| return statement(stmt),
//     }
// }

// fn declaration(d: AST.Declaration) bool {
//     switch (d) {
//         .err => |err| {
//             std.log.err("{}", .{err});
//             return true;
//         },
//         .function => |func| return function(func),
//         .variable => |_var| if (_var.init) |init| return expression(init),
//     }
//     return false;
// }

// fn statement(s: AST.Statement) bool {
//     return switch (s) {
//         .err => |err| {
//             std.log.err("{}", .{err});
//             return true;
//         },
//         .@"return" => |val| expression(val),
//         .@"if" => |_if| {
//             var has = false;
//             if (expression(_if.cond)) has = true;
//             if (statement(_if.then.*)) has = true;
//             if (_if.or_else) |or_else| if (statement(or_else.*)) {
//                 has = true;
//             };
//             return has;
//         },
//         .null => false,
//         .labeled_stmt => |stmt| statement(stmt.stmt.*),
//         .goto => false,
//         .expr => |e| expression(e),
//         .compound => |b| block(b),
//         .@"break" => false,
//         .@"continue" => false,
//         .@"while" => |_while| {
//             var has = false;
//             if (expression(_while.cond)) has = true;
//             if (statement(_while.body.*)) has = true;
//             return has;
//         },
//         .do_while => |do| {
//             var has = false;
//             if (statement(do.body.*)) has = true;
//             if (expression(do.cond)) has = true;
//             return has;
//         },
//         .@"for" => |_for| {
//             var has = false;
//             switch (_for.init) {
//                 .decl => |decl| if (declaration(decl.*)) {
//                     has = true;
//                 },
//                 .expr => |ex| if (ex) |e| if (expression(e)) {
//                     has = true;
//                 },
//             }
//             if (_for.cond) |cond| if (expression(cond)) {
//                 has = true;
//             };
//             if (_for.post) |post| if (expression(post)) {
//                 has = true;
//             };
//             if (statement(_for.body.*)) has = true;
//             return has;
//         },
//         .@"switch" => |_switch| {
//             var has = false;
//             if (expression(_switch.cond)) has = true;
//             if (statement(_switch.body.*)) has = true;
//             return has;
//         },
//         .case => |case| {
//             var has = false;
//             if (expression(case.expr)) has = true;
//             if (case.stmt) |cs| if (statement(cs.*)) {
//                 has = true;
//             };
//             return has;
//         },
//         .default => |def| {
//             if (def.stmt) |or_else| if (statement(or_else.*)) {
//                 return true;
//             };
//             return false;
//         },
//     };
// }

// fn expression(e: AST.Expression) bool {
//     return switch (e) {
//         .err => |err| {
//             std.log.err("{}", .{err});
//             return true;
//         },
//         .int_lit => false,
//         .lhs_unary => |unary| expression(unary.rhs.*),
//         .rhs_unary => |unary| expression(unary.lhs.*),
//         .binary => |binary| {
//             var has = false;
//             if (expression(binary.lhs.*)) has = true;
//             if (expression(binary.rhs.*)) has = true;
//             return has;
//         },
//         .assignment => |assign| {
//             var has = false;
//             if (expression(assign.lhs.*)) has = true;
//             if (expression(assign.rhs.*)) has = true;
//             return has;
//         },
//         .conditional => |cond| {
//             var has = false;
//             if (expression(cond.cond.*)) has = true;
//             if (expression(cond.if_true.*)) has = true;
//             if (expression(cond.if_false.*)) has = true;
//             return has;
//         },
//         .variable => false,
//     };
// }
