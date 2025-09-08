const std = @import("std");

const PIR = @import("semantic_analyzer/PIR.zig");

const TackyIR = @import("TackyIR.zig");
const Function = TackyIR.Function;
const Instruction = TackyIR.Instruction;
const Value = TackyIR.Value;

const oneOf = @import("utils.zig").oneOf;

instrs: std.array_list.Managed(Instruction),
allocator: std.mem.Allocator,
var_counter: usize = 0,
label_counter: usize = 0,

const TackyIRGenerator = @This();

pub fn lower(ast: PIR, allocator: std.mem.Allocator) TackyIR {
    var arena = std.heap.ArenaAllocator.init(allocator);

    var self = TackyIRGenerator{
        .instrs = .init(arena.allocator()),
        .allocator = arena.allocator(),
    };

    const func = self.function(ast.main_function);

    return .{ .main = func, .arena = arena };
}

fn function(self: *TackyIRGenerator, ast_func: PIR.Function) Function {
    const previous_instrs = self.instrs;

    self.instrs = .init(self.allocator);
    defer self.instrs = previous_instrs;

    for (ast_func.body.items) |item|
        self.instruction(item);

    // add gurading return instruction
    self.addInstr(.{ .ret = .{ .constant = 0 } });

    const instrs = self.instrs.toOwnedSlice() catch @panic("OOM");

    return .{ .identifier = ast_func.name, .instructions = instrs };
}

fn instruction(self: *TackyIRGenerator, item: PIR.BlockItem) void {
    switch (item) {
        .declaration => |decl| self.declInstruction(decl),
        .statement => |stmt| self.stmtInstruction(stmt),
    }
}

fn declInstruction(self: *TackyIRGenerator, decl: PIR.Declaration) void {
    switch (decl) {
        .variable => |variable| {
            if (variable.init) |init| {
                const val = self.value(init);
                const dst = Value{ .variable = variable.name };
                self.addInstr(.{ .copy = .{ .src = val, .dst = dst } });
            }
        },
    }
}

fn stmtInstruction(self: *TackyIRGenerator, stmt: PIR.Statement) void {
    switch (stmt) {
        .@"return" => |expr| {
            const val = self.value(expr);
            self.addInstr(.{ .ret = val });
        },
        .expr => |expr| _ = self.value(expr),
        .null => {},
        .@"if" => |_if| {
            const cond = self.value(_if.cond);

            const else_label = if (_if.or_else != null) self.makeLabel("else") else null;
            const end_label = self.makeLabel("end");

            self.addInstr(.{ .jump_if_zero = .{
                .condition = cond,
                .target = else_label orelse end_label,
            } });
            self.stmtInstruction(_if.then.*);
            self.addInstr(.{ .jump = end_label });
            if (_if.or_else) |else_stmt| {
                self.addInstr(.{ .label = else_label.? });
                self.stmtInstruction(else_stmt.*);
                self.addInstr(.{ .jump = end_label });
            }
            self.addInstr(.{ .label = end_label });
        },
        .goto => |target| self.addInstr(.{ .jump = target }),
        .labeled_stmt => |ls| {
            self.addInstr(.{ .label = ls.label });
            self.stmtInstruction(ls.stmt.*);
        },
        .compound => |compound| for (compound.items) |item|
            self.instruction(item),
        .@"break" => |lbl| self.addInstr(.{ .jump = self.breakLabel(lbl) }),
        .@"continue" => |lbl| self.addInstr(.{ .jump = self.continueLabel(lbl) }),
        .@"while" => |_while| {
            const break_label = self.breakLabel(_while.label);
            const continue_label = self.continueLabel(_while.label);

            self.addInstr(.{ .label = continue_label });
            const cond = self.value(_while.cond);
            self.addInstr(.{ .jump_if_zero = .{ .condition = cond, .target = break_label } });
            self.stmtInstruction(_while.body.*);
            self.addInstrs(&.{
                .{ .jump = continue_label },
                .{ .label = break_label },
            });
        },
        .do_while => |do_while| {
            const start_label = self.makeLabel(do_while.label);
            const break_label = self.breakLabel(do_while.label);
            const continue_label = self.continueLabel(do_while.label);

            self.addInstr(.{ .label = start_label });
            self.stmtInstruction(do_while.body.*);
            self.addInstr(.{ .label = continue_label });
            const cond = self.value(do_while.cond);
            self.addInstrs(&.{
                .{ .jump_if_not_zero = .{ .condition = cond, .target = start_label } },
                .{ .label = break_label },
            });
        },
        .@"for" => |_for| {
            const start_label = self.makeLabel(_for.label);
            const break_label = self.breakLabel(_for.label);
            const continue_label = self.continueLabel(_for.label);

            switch (_for.init) {
                .decl => |decl| self.declInstruction(decl.*),
                .expr => |expr| if (expr) |exp| {
                    _ = self.value(exp);
                },
            }
            self.addInstr(.{ .label = start_label });
            const cond = if (_for.cond) |cond| self.value(cond) else null;
            if (cond) |c| {
                self.addInstr(.{ .jump_if_zero = .{ .condition = c, .target = break_label } });
            }
            self.stmtInstruction(_for.body.*);
            self.addInstr(.{ .label = continue_label });
            if (_for.post) |post|
                _ = self.value(post);
            self.addInstrs(&.{
                .{ .jump = start_label },
                .{ .label = break_label },
            });
        },
        .@"switch" => |_switch| {
            const break_label = self.breakLabel(_switch.label);

            const expr = self.value(_switch.cond);
            for (_switch.cases) |case| {
                const constant = self.value(case.expr);
                const cond = self.makeTempVar();
                self.addInstrs(&.{
                    .{ .binary = .{
                        .operator = .equal,
                        .src1 = expr,
                        .src2 = constant,
                        .dst = cond,
                    } },
                    .{ .jump_if_not_zero = .{
                        .condition = cond,
                        .target = case.label,
                    } },
                });
            }

            if (_switch.default) |default| {
                self.addInstr(.{ .jump = default.label });
            } else {
                self.addInstr(.{ .jump = break_label });
            }

            self.stmtInstruction(_switch.body.*);
            self.addInstr(.{ .label = break_label });
        },
        .case => |case| {
            self.addInstr(.{ .label = case.label });
            if (case.stmt) |_stmt| self.stmtInstruction(_stmt.*);
        },
        .default => |default| {
            self.addInstr(.{ .label = default.label });
            if (default.stmt) |_stmt| self.stmtInstruction(_stmt.*);
        },
    }
}

fn value(self: *TackyIRGenerator, expr: PIR.Expression) Value {
    return switch (expr) {
        .int_lit => |int| .{ .constant = int },
        .binary => |binary| result: {
            const dst = self.makeTempVar();

            if (binary.operator == .amp_amp) {
                const false_label = self.makeLabel("false");
                const end_label = self.makeLabel("end");

                const src1 = self.value(binary.lhs.*);
                self.addInstr(.{ .jump_if_zero = .{ .condition = src1, .target = false_label } });
                const src2 = self.value(binary.rhs.*);
                self.addInstrs(&.{
                    .{ .jump_if_zero = .{ .condition = src2, .target = false_label } },
                    .{ .copy = .{ .src = .{ .constant = 1 }, .dst = dst } },
                    .{ .jump = end_label },
                    .{ .label = false_label },
                    .{ .copy = .{ .src = .{ .constant = 0 }, .dst = dst } },
                    .{ .label = end_label },
                });
            } else if (binary.operator == .verbar_verbar) {
                const true_label = self.makeLabel("true");
                const end_label = self.makeLabel("end");

                const src1 = self.value(binary.lhs.*);
                self.addInstr(.{ .jump_if_not_zero = .{ .condition = src1, .target = true_label } });
                const src2 = self.value(binary.rhs.*);
                self.addInstrs(&.{
                    .{ .jump_if_not_zero = .{ .condition = src2, .target = true_label } },
                    .{ .copy = .{ .src = .{ .constant = 0 }, .dst = dst } },
                    .{ .jump = end_label },
                    .{ .label = true_label },
                    .{ .copy = .{ .src = .{ .constant = 1 }, .dst = dst } },
                    .{ .label = end_label },
                });
            } else {
                const src1 = self.value(binary.lhs.*);
                const src2 = self.value(binary.rhs.*);

                const operator: Instruction.Binary.Operator = switch (binary.operator) {
                    .plus => .add,
                    .minus => .subtract,
                    .asterisk => .multiply,
                    .slash => .divide,
                    .percent => .remainder,
                    .amp => .bitwise_and,
                    .verbar => .bitwise_or,
                    .hat => .bitwise_xor,
                    .lt_lt => .shift_left,
                    .gt_gt => .shift_right,
                    .lt => .less,
                    .gt => .greater,
                    .lt_equals => .less_equal,
                    .gt_equals => .greater_equal,
                    .equals_equals => .equal,
                    .excl_equals => .not_equal,
                    else => unreachable,
                };

                self.addInstr(.{ .binary = .{
                    .operator = operator,
                    .src1 = src1,
                    .src2 = src2,
                    .dst = dst,
                } });
            }

            break :result dst;
        },
        .lhs_unary => |unary| result: {
            const src = self.value(unary.rhs.*);

            if (unary.operator == .plus)
                break :result src;

            const dst = self.makeTempVar();

            if (oneOf(unary.operator, &.{ .plus_plus, .minus_minus })) {
                const operator: Instruction.Binary.Operator = switch (unary.operator) {
                    .plus_plus => .add,
                    .minus_minus => .subtract,
                    else => unreachable,
                };

                self.addInstrs(&.{ .{ .binary = .{
                    .operator = operator,
                    .src1 = src,
                    .src2 = .{ .constant = 1 },
                    .dst = src,
                } }, .{ .copy = .{
                    .src = src,
                    .dst = dst,
                } } });
            } else {
                const operator: Instruction.Unary.Operator = switch (unary.operator) {
                    .tilde => .complement,
                    .minus => .negate,
                    .excl => .not,
                    else => unreachable,
                };

                self.addInstr(.{ .unary = .{
                    .operator = operator,
                    .src = src,
                    .dst = dst,
                } });
            }

            break :result dst;
        },
        .rhs_unary => |unary| result: {
            const src = self.value(unary.lhs.*);
            const dst = self.makeTempVar();

            const operator: Instruction.Binary.Operator = switch (unary.operator) {
                .plus_plus => .add,
                .minus_minus => .subtract,
                else => unreachable,
            };

            self.addInstrs(&.{
                .{ .copy = .{
                    .src = src,
                    .dst = dst,
                } },
                .{ .binary = .{
                    .operator = operator,
                    .src1 = src,
                    .src2 = .{ .constant = 1 },
                    .dst = src,
                } },
            });

            break :result dst;
        },
        .variable => |name| .{ .variable = name },
        .assignment => |assignment| result: {
            const rhs = self.value(assignment.rhs.*);
            const dst = self.value(assignment.lhs.*);

            if (assignment.operator == .equals) {
                self.addInstr(.{ .copy = .{ .src = rhs, .dst = dst } });
            } else {
                const operator: TackyIR.Instruction.Binary.Operator = switch (assignment.operator) {
                    .plus_equals => .add,
                    .minus_equals => .subtract,
                    .asterisk_equals => .multiply,
                    .slash_equals => .divide,
                    .percent_equals => .remainder,
                    .amp_equals => .bitwise_and,
                    .verbar_equals => .bitwise_or,
                    .hat_equals => .bitwise_xor,
                    .lt_lt_equals => .shift_left,
                    .gt_gt_equals => .shift_right,
                    else => unreachable,
                };
                self.addInstr(.{ .binary = .{
                    .operator = operator,
                    .src1 = dst,
                    .src2 = rhs,
                    .dst = dst,
                } });
            }

            break :result dst;
        },
        .conditional => |conditionl| result: {
            const dst = self.makeTempVar();

            const else_label = self.makeLabel("else");
            const end_label = self.makeLabel("end");

            const cond = self.value(conditionl.cond.*);
            self.addInstr(
                .{ .jump_if_zero = .{ .condition = cond, .target = else_label } },
            );

            const true_value = self.value(conditionl.if_true.*);
            self.addInstrs(&.{
                .{ .copy = .{ .src = true_value, .dst = dst } },
                .{ .jump = end_label },
                .{ .label = else_label },
            });

            const false_value = self.value(conditionl.if_false.*);
            self.addInstrs(&.{
                .{ .copy = .{ .src = false_value, .dst = dst } },
                .{ .label = end_label },
            });

            break :result dst;
        },
    };
}

fn makeTempVar(self: *TackyIRGenerator) Value {
    defer self.var_counter += 1;
    const name = self.sprint("tmp.{}", .{self.var_counter});
    return .{ .variable = name };
}

fn makeLabel(self: *TackyIRGenerator, prefix: []const u8) []u8 {
    defer self.label_counter += 1;
    return self.sprint(".tir.{s}.{}", .{ prefix, self.label_counter });
}

fn breakLabel(self: *TackyIRGenerator, loopLabel: []const u8) []u8 {
    return self.sprint(".break.{s}", .{loopLabel});
}

fn continueLabel(self: *TackyIRGenerator, loopLabel: []const u8) []u8 {
    return self.sprint(".continue.{s}", .{loopLabel});
}

fn sprint(self: *TackyIRGenerator, comptime fmt: []const u8, args: anytype) []u8 {
    return std.fmt.allocPrint(self.allocator, fmt, args) catch @panic("OOM");
}

fn addInstr(self: *TackyIRGenerator, instr: Instruction) void {
    self.instrs.append(instr) catch @panic("OOM");
}

fn addInstrs(self: *TackyIRGenerator, instrs: []const Instruction) void {
    for (instrs) |instr| self.addInstr(instr);
}
