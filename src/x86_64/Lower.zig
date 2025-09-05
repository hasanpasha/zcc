const std = @import("std");

const TackyIR = @import("../TackyIR.zig");
const AIR = @import("AIR.zig");

const oneOf = @import("../utils.zig").oneOf;

allocator: std.mem.Allocator,

instructions: ?std.array_list.Managed(AIR.Instruction) = null,

const Lower = @This();

pub fn lower(ir: TackyIR, allocator: std.mem.Allocator) AIR {
    var arena = std.heap.ArenaAllocator.init(allocator);

    var self = Lower{ .allocator = arena.allocator() };

    const main_subroutine = self.subroutine(ir.main);

    return .{ .main_subroutine = main_subroutine, .arena = arena };
}

fn subroutine(self: *Lower, function: TackyIR.Function) AIR.Subroutine {
    const previous_instrs = self.instructions;

    self.instructions = .init(self.allocator);
    defer self.instructions = previous_instrs;

    for (function.instructions) |instr| {
        self.instruction(instr);
    }

    const as_instrs = self.instructions.?.toOwnedSlice() catch @panic("OOM");
    return .{ .name = function.identifier, .instructions = as_instrs };
}

fn instruction(self: *Lower, instr: TackyIR.Instruction) void {
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
                const operator: AIR.Instruction.Unary.Operator = switch (unary.operator) {
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
                const code: AIR.Instruction.CondCode = switch (binary.operator) {
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
                const operator: AIR.Instruction.Binary.Operator = switch (binary.operator) {
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

fn operand(self: *Lower, value: TackyIR.Value) AIR.Operand {
    _ = self;
    return switch (value) {
        .constant => |constant| .{ .imm = @truncate(constant) },
        .variable => |name| .{ .pseudo = name },
    };
}

fn addInstr(self: *Lower, instr: AIR.Instruction) void {
    self.instructions.?.append(instr) catch @panic("OOM");
}

fn addInstrs(self: *Lower, instr: []const AIR.Instruction) void {
    self.instructions.?.appendSlice(instr) catch @panic("OOM");
}
