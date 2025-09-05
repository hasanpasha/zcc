const std = @import("std");

const tacky_ir = @import("../tacky_ir.zig");
const as = @import("asm.zig");

const oneOf = @import("../utils.zig").oneOf;

allocator: std.mem.Allocator,

instructions: ?std.array_list.Managed(as.Instruction) = null,

const Convertor = @This();

pub fn convert(ir: tacky_ir.ProgramTackyIR, allocator: std.mem.Allocator) as.Program {
    var arena = std.heap.ArenaAllocator.init(allocator);

    var self = Convertor{ .allocator = arena.allocator() };

    const main_subroutine = self.subroutine(ir.main);

    return .{ .main_subroutine = main_subroutine, .arena = arena };
}

fn subroutine(self: *Convertor, function: tacky_ir.FunctionTackyIR) as.Subroutine {
    const previous_instrs = self.instructions;

    self.instructions = .init(self.allocator);
    defer self.instructions = previous_instrs;

    for (function.instructions) |instr| {
        self.instruction(instr);
    }

    const as_instrs = self.instructions.?.toOwnedSlice() catch @panic("OOM");
    return .{ .name = function.identifier, .instructions = as_instrs };
}

fn instruction(self: *Convertor, instr: tacky_ir.Instruction) void {
    switch (instr) {
        .ret => |val| self.addInstrs(&.{
            .{ .mov = .{
                .src = self.operand(val),
                .dst = .{ .reg = .eax },
            } },
            .ret,
        }),
        .unary => |unary| {
            const src = self.operand(unary.src);
            const dst = self.operand(unary.dst);

            if (unary.operator == .not) {
                self.addInstrs(&.{
                    .{ .cmp = .{ .src1 = .{ .imm = 0 }, .src2 = src } },
                    .{ .mov = .{ .src = .{ .imm = 0 }, .dst = dst } },
                    .{ .set_cc = .{ .cond_code = .e, .dst = dst } },
                });
            } else {
                const operator: as.Instruction.Unary.Operator = switch (unary.operator) {
                    .complement => .not,
                    .negate => .neg,
                    else => unreachable,
                };

                self.addInstrs(&.{
                    .{ .mov = .{
                        .src = src,
                        .dst = dst,
                    } },
                    .{ .unary = .{
                        .operator = operator,
                        .operand = dst,
                    } },
                });
            }
        },
        .binary => |binary| {
            const src1 = self.operand(binary.src1);
            const src2 = self.operand(binary.src2);
            const dst = self.operand(binary.dst);

            if (oneOf(binary.operator, &.{ .divide, .remainder })) {
                self.addInstrs(&.{
                    .{ .mov = .{ .src = src1, .dst = .{ .reg = .eax } } },
                    .cdq,
                    .{ .idiv = src2 },
                    .{ .mov = .{
                        .src = .{ .reg = if (binary.operator == .divide) .eax else .edx },
                        .dst = dst,
                    } },
                });
            } else if (oneOf(binary.operator, &.{
                .equal,
                .not_equal,
                .less,
                .less_equal,
                .greater,
                .greater_equal,
            })) {
                const code: as.Instruction.CondCode = switch (binary.operator) {
                    .equal => .e,
                    .not_equal => .ne,
                    .less => .l,
                    .less_equal => .le,
                    .greater => .g,
                    .greater_equal => .ge,
                    else => unreachable,
                };

                self.addInstrs(&.{
                    .{ .cmp = .{ .src1 = src2, .src2 = src1 } },
                    .{ .mov = .{ .src = .{ .imm = 0 }, .dst = dst } },
                    .{ .set_cc = .{ .cond_code = code, .dst = dst } },
                });
            } else {
                const operator: as.Instruction.Binary.Operator = switch (binary.operator) {
                    .add => .add,
                    .subtract => .sub,
                    .multiply => .imul,
                    .bitwise_and => .@"and",
                    .bitwise_or => .@"or",
                    .bitwise_xor => .xor,
                    .shift_left => .sal,
                    .shift_right => .sar,
                    else => unreachable,
                };

                self.addInstrs(&.{
                    .{ .mov = .{
                        .src = src1,
                        .dst = dst,
                    } },
                    .{ .binary = .{
                        .operator = operator,
                        .src1 = src2,
                        .src2 = dst,
                    } },
                });
            }
        },
        .copy => |copy| self.addInstr(.{ .mov = .{
            .src = self.operand(copy.src),
            .dst = self.operand(copy.dst),
        } }),
        .jump => |target| self.addInstr(.{ .jmp = target }),
        .jump_if_zero => |jump| self.addInstrs(&.{
            .{ .cmp = .{
                .src1 = self.operand(jump.condition),
                .src2 = .{ .imm = 0 },
            } },
            .{ .jmp_cc = .{
                .cond_code = .e,
                .target = jump.target,
            } },
        }),
        .jump_if_not_zero => |jump| self.addInstrs(&.{
            .{ .cmp = .{
                .src1 = self.operand(jump.condition),
                .src2 = .{ .imm = 0 },
            } },
            .{ .jmp_cc = .{
                .cond_code = .ne,
                .target = jump.target,
            } },
        }),
        .label => |label| self.addInstr(.{ .label = label }),
    }
}

fn operand(self: *Convertor, value: tacky_ir.Value) as.Operand {
    _ = self;
    return switch (value) {
        .constant => |constant| .{ .imm = @truncate(constant) },
        .variable => |name| .{ .pseudo = name },
    };
}

fn addInstr(self: *Convertor, instr: as.Instruction) void {
    self.instructions.?.append(instr) catch @panic("OOM");
}

fn addInstrs(self: *Convertor, instr: []const as.Instruction) void {
    self.instructions.?.appendSlice(instr) catch @panic("OOM");
}
