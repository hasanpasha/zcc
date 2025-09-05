//! replace all pseudo operands with stack operands
const std = @import("std");

const as = @import("asm.zig");

allocator: std.mem.Allocator,

size: isize = 0,
pseudo_stack_map: std.StringHashMap(isize),

const PseudoEliminator = @This();

pub fn eliminate(program: as.Program, allocator: std.mem.Allocator) as.Program {
    var arena = std.heap.ArenaAllocator.init(allocator);

    var self = PseudoEliminator{
        .allocator = arena.allocator(),
        .pseudo_stack_map = .init(allocator),
    };
    defer self.pseudo_stack_map.deinit();

    const main_subroutine = self.subroutine(program.main_subroutine);

    return .{ .main_subroutine = main_subroutine, .arena = arena };
}

fn subroutine(self: *PseudoEliminator, sub: as.Subroutine) as.Subroutine {
    var instrs = std.array_list.Managed(as.Instruction).initCapacity(
        self.allocator,
        sub.instructions.len + 1,
    ) catch @panic("OOM");
    for (sub.instructions) |instr| {
        switch (instr) {
            inline else => |_instr, tag| {
                const T = @TypeOf(_instr);
                var _instr_fixed: T = _instr;
                if (T == as.Operand) {
                    _instr_fixed = self.operand(_instr);
                } else if (@typeInfo(T) == .@"struct") {
                    inline for (std.meta.fields(T)) |field| {
                        if (field.type == as.Operand)
                            @field(_instr_fixed, field.name) = self.operand(@field(_instr, field.name));
                    }
                }

                instrs.appendAssumeCapacity(@unionInit(as.Instruction, @tagName(tag), _instr_fixed));
            },
        }
    }

    instrs.insertAssumeCapacity(0, .{ .allocate_stack = @abs(self.size) });

    const new_instrs = instrs.toOwnedSlice() catch @panic("OOM");
    return .{ .name = sub.name, .instructions = new_instrs };
}

fn operand(self: *PseudoEliminator, op: as.Operand) as.Operand {
    return switch (op) {
        .pseudo => |name| new_operand: {
            const index = self.pseudo_stack_map.get(name) orelse new_index: {
                self.size -= 4;
                self.pseudo_stack_map.put(name, self.size) catch @panic("OOM");
                break :new_index self.size;
            };
            break :new_operand .{ .stack = index };
        },
        inline else => |_operand, tag| @unionInit(as.Operand, @tagName(tag), _operand),
    };
}
