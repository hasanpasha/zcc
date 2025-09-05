const std = @import("std");

pub const AIR = @import("AIR.zig");

const oneOf = @import("../utils.zig").oneOf;

allocator: std.mem.Allocator,

instructions: ?std.array_list.Managed(AIR.Instruction) = null,

const InstructionFixer = @This();

pub fn fix(old: AIR, allocator: std.mem.Allocator) AIR {
    var arena = std.heap.ArenaAllocator.init(allocator);

    var self: InstructionFixer = .{ .allocator = arena.allocator() };

    const new_subroutine = self.subroutine(old.main_subroutine);

    return .{ .main_subroutine = new_subroutine, .arena = arena };
}

fn subroutine(self: *InstructionFixer, sub: AIR.Subroutine) AIR.Subroutine {
    const previous_instrs = self.instructions;

    self.instructions = .init(self.allocator);
    defer self.instructions = previous_instrs;

    for (sub.instructions) |instr| {
        self.instruction(instr);
    }

    const instrs = self.instructions.?.toOwnedSlice() catch @panic("OOM");
    return .{ .name = sub.name, .instructions = instrs };
}

fn instruction(self: *InstructionFixer, instr: AIR.Instruction) void {
    switch (instr) {
        .allocate_stack, .cdq, .ret, .unary => self.addInstr(instr),
        .idiv => |div| self.addInstrs(&.{
            .{ .mov = .{
                .src = div,
                .dst = .{ .reg = .r10d },
            } },
            .{ .idiv = .{ .reg = .r10d } },
        }),
        .mov => |mov| {
            if (mov.src == .stack and mov.dst == .stack) {
                const temp = AIR.Operand{ .reg = .r10d };
                self.addInstrs(&.{
                    .{ .mov = .{ .src = mov.src, .dst = temp } },
                    .{ .mov = .{ .src = temp, .dst = mov.dst } },
                });
            } else {
                self.addInstr(instr);
            }
        },
        .binary => |binary| {
            if ((oneOf(binary.operator, &.{ .add, .sub }) and binary.src1 == .stack and binary.src2 == .stack) or
                (binary.operator == .imul and binary.src2 == .stack) or
                (oneOf(binary.operator, &.{ .@"and", .@"or", .xor }) and binary.src2 == .stack))
            {
                const r10 = AIR.Operand{ .reg = .r10d };
                self.addInstrs(&.{
                    .{ .mov = .{ .src = binary.src2, .dst = r10 } },
                    .{ .binary = .{ .operator = binary.operator, .src1 = binary.src1, .src2 = r10 } },
                    .{ .mov = .{ .src = r10, .dst = binary.src2 } },
                });
            } else if (oneOf(binary.operator, &.{ .shl, .sal, .shr, .sar }) and binary.src1 != .imm and !std.meta.eql(binary.src1, .{ .reg = .cl })) {
                self.addInstrs(&.{
                    .{ .mov = .{ .src = binary.src1, .dst = .{ .reg = .ecx } } },
                    .{ .binary = .{ .operator = binary.operator, .src1 = .{ .reg = .cl }, .src2 = binary.src2 } },
                });
            } else {
                self.addInstr(instr);
            }
        },
        .cmp => |cmp| {
            if (cmp.src1 == .stack and cmp.src2 == .stack) {
                const r10 = AIR.Operand{ .reg = .r10d };
                self.addInstrs(&.{
                    .{ .mov = .{ .src = cmp.src1, .dst = r10 } },
                    .{ .cmp = .{ .src1 = r10, .src2 = cmp.src2 } },
                });
            } else if (cmp.src2 == .imm) {
                const r11 = AIR.Operand{ .reg = .r11d };
                self.addInstrs(&.{
                    .{ .mov = .{ .src = cmp.src2, .dst = r11 } },
                    .{ .cmp = .{ .src1 = cmp.src1, .src2 = r11 } },
                });
            } else {
                self.addInstr(instr);
            }
        },
        inline else => self.addInstr(instr),
    }
}

fn addInstr(self: *InstructionFixer, instr: AIR.Instruction) void {
    self.instructions.?.append(instr) catch @panic("OOM");
}

fn addInstrs(self: *InstructionFixer, instrs: []const AIR.Instruction) void {
    for (instrs) |instr| {
        self.addInstr(instr);
    }
}
