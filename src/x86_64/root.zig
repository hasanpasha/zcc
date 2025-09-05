const std = @import("std");

pub const AIR = @import("AIR.zig");
const TackyIR = @import("../TackyIR.zig");

const Lower = @import("Lower.zig");
const PseudoEliminator = @import("PseudoEliminator.zig");
const InstructionFixer = @import("InstructionFixer.zig");

pub fn lower(ir: TackyIR, allocator: std.mem.Allocator) AIR {
    var initial = Lower.lower(ir, allocator);
    defer initial.free();

    var eliminated_pseudo = PseudoEliminator.eliminate(initial, allocator);
    defer eliminated_pseudo.free();

    const fixed_asm = InstructionFixer.fix(eliminated_pseudo, allocator);

    return fixed_asm;
}
