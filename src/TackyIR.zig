const std = @import("std");

const TackyIR = @This();

main: Function,

arena: std.heap.ArenaAllocator,

pub fn free(self: TackyIR) void {
    self.arena.deinit();
}

pub fn format(
    self: @This(),
    writer: *std.Io.Writer,
) std.Io.Writer.Error!void {
    try writer.print("TackyIR{{ main: {f} }}", .{self.main});
}

pub fn prettyFmt(self: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
    try writer.print("{f}", .{PrettyPrint.pretty(self)});
}

pub const Function = struct {
    identifier: []const u8,
    instructions: []const Instruction,

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try writer.print("FunctionTackyIR{{ identifier: {s}, instructions: {{ ", .{self.identifier});
        for (self.instructions, 1..) |instr, i| {
            try writer.print("{f}", .{instr});
            if (i != self.instructions.len) {
                try writer.writeAll(", ");
            }
        }
        try writer.writeAll(" } }");
    }
};

pub const InstructionKind = enum {
    ret,
    unary,
    binary,
    copy,
    jump,
    jump_if_zero,
    jump_if_not_zero,
    label,
};

pub const Instruction = union(InstructionKind) {
    ret: Value,
    unary: Unary,
    binary: Binary,
    copy: Copy,
    jump: []const u8,
    jump_if_zero: JumpIfZero,
    jump_if_not_zero: JumpIfNotZero,
    label: []const u8,

    pub const Unary = struct {
        operator: Operator,
        src: Value,
        dst: Value,

        pub const Operator = enum {
            complement,
            negate,
            not,

            pub fn string(self: Operator) []const u8 {
                return switch (self) {
                    .complement => "~",
                    .negate => "-",
                    .not => "!",
                };
            }
        };

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.print("UnaryInstr{{ operator: {}, src: {f}, dst: {f} }}", .{
                self.operator,
                self.src,
                self.dst,
            });
        }
    };

    pub const Binary = struct {
        operator: Operator,
        src1: Value,
        src2: Value,
        dst: Value,

        pub const Operator = enum {
            add,
            subtract,
            multiply,
            divide,
            remainder,
            bitwise_and,
            bitwise_or,
            bitwise_xor,
            shift_left,
            shift_right,
            less,
            greater,
            less_equal,
            greater_equal,
            equal,
            not_equal,

            pub fn string(self: Operator) []const u8 {
                return switch (self) {
                    .add => "+",
                    .subtract => "-",
                    .multiply => "*",
                    .divide => "/",
                    .remainder => "%",
                    .bitwise_and => "&",
                    .bitwise_or => "|",
                    .bitwise_xor => "^",
                    .shift_left => "<<",
                    .shift_right => ">>",
                    .less => "<",
                    .greater => ">",
                    .less_equal => "<=",
                    .greater_equal => ">=",
                    .equal => "==",
                    .not_equal => "!=",
                };
            }
        };

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.print("BinaryInstr{{ operator: {}, src1: {f}, src2: {f}, dst: {f} }}", .{
                self.operator,
                self.src1,
                self.src2,
                self.dst,
            });
        }
    };

    pub const Copy = struct {
        src: Value,
        dst: Value,

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.print("CopyInstr{{ src: {f}, dst: {f} }}", .{
                self.src,
                self.dst,
            });
        }
    };

    pub const JumpIfZero = struct {
        condition: Value,
        target: []const u8,

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.print("JumpIfZeroInstr{{ condition: {f}, target: {s} }}", .{
                self.condition,
                self.target,
            });
        }
    };

    pub const JumpIfNotZero = struct {
        condition: Value,
        target: []const u8,

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.print("JumpIfNotZeroInstr{{ condition: {f}, target: {s} }}", .{
                self.condition,
                self.target,
            });
        }
    };

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self) {
            .ret => |val| try writer.print("RetInstr{{ {f} }}", .{val}),
            .jump => |target| try writer.print("JumpInstr{{ {s} }}", .{target}),
            .label => |target| try writer.print("LabelInstr{{ {s} }}", .{target}),
            inline else => |instr| try writer.print("{f}", .{instr}),
        }
    }
};

pub const ValueKind = enum {
    constant,
    variable,
};

pub const Value = union(ValueKind) {
    constant: u128,
    variable: []const u8,

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self) {
            .constant => |int| try writer.print("ConstantVal{{ {} }}", .{int}),
            .variable => |v| try writer.print("VarVal{{ {s} }}", .{v}),
        }
    }
};

pub const PrettyPrint = struct {
    const Writer = std.Io.Writer;
    const Error = Writer.Error;
    const Alt = std.fmt.Alt;

    fn pretty(p: TackyIR) Alt(TackyIR, programFmt) {
        return .{ .data = p };
    }

    fn programFmt(p: TackyIR, writer: *Writer) Error!void {
        try writer.print("{f}", .{function(p.main)});
    }

    fn function(f: Function) Alt(Function, functionFmt) {
        return .{ .data = f };
    }

    fn functionFmt(f: Function, w: *Writer) Error!void {
        try w.print("fun {s}\n", .{f.identifier});
        try w.print("{{\n", .{});
        for (f.instructions) |instr| {
            try w.print("{f}\n", .{instruction(instr)});
        }
        try w.print("}}\n", .{});
    }

    fn instruction(instr: Instruction) Alt(Instruction, instructionFmt) {
        return .{ .data = instr };
    }

    fn instructionFmt(instr: Instruction, w: *Writer) Error!void {
        switch (instr) {
            .ret => |val| try w.print("return {f}", .{value(val)}),
            .unary => |unary| try w.print("{f} = {s}{f}", .{
                value(unary.dst),
                unary.operator.string(),
                value(unary.src),
            }),
            .binary => |binary| try w.print("{f} = {f} {s} {f}", .{
                value(binary.dst),
                value(binary.src1),
                binary.operator.string(),
                value(binary.src2),
            }),
            .copy => |copy| try w.print("{f} = {f}", .{ value(copy.dst), value(copy.src) }),
            .jump => |target| try w.print("jump {s}", .{target}),
            .jump_if_zero => |jump| try w.print("if({f} == 0) jump {s}", .{ value(jump.condition), jump.target }),
            .jump_if_not_zero => |jump| try w.print("if({f} != 0) jump {s}", .{ value(jump.condition), jump.target }),
            .label => |label| try w.print("{s}:", .{label}),
        }
    }

    fn value(val: Value) Alt(Value, valueFmt) {
        return .{ .data = val };
    }

    fn valueFmt(val: Value, w: *Writer) Error!void {
        switch (val) {
            .constant => |c| try w.print("{}", .{c}),
            .variable => |name| try w.print("{s}", .{name}),
        }
    }
};
