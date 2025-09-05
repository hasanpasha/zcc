pub const as = @import("asm.zig");

const std = @import("std");
const tacky_ir = @import("../tacky_ir.zig");
const Convertor = @import("Convertor.zig");
const PseudoEliminator = @import("PseudoEliminator.zig");
const InstructionFixer = @import("InstructionFixer.zig");

pub fn generate(ir: tacky_ir.ProgramTackyIR, allocator: std.mem.Allocator) as.Program {
    var initial = Convertor.convert(ir, allocator);
    defer initial.free();

    var eliminated_pseudo = PseudoEliminator.eliminate(initial, allocator);
    defer eliminated_pseudo.free();

    const fixed_asm = InstructionFixer.fix(eliminated_pseudo, allocator);

    return fixed_asm;
}
