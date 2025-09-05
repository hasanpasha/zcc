const std = @import("std");
const Emitter = @import("Emitter.zig");

pub const Program = struct {
    main_subroutine: Subroutine,

    arena: std.heap.ArenaAllocator,

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try writer.print("X86_64Program{{ main_subroutine: {f} }}", .{self.main_subroutine});
    }

    pub fn emit(self: Program, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try Emitter.emit(self, writer);
    }

    pub fn free(self: Program) void {
        self.arena.deinit();
    }
};

pub const Subroutine = struct {
    name: []const u8,
    instructions: []const Instruction,

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try writer.writeAll("Subroutine{ instructions: { ");
        for (self.instructions, 1..) |instr, i| {
            try writer.print("{f}", .{instr});
            if (i < self.instructions.len) {
                try writer.writeAll(", ");
            }
        }
        try writer.writeAll("} }");
    }
};

pub const InstructionKind = enum {
    mov,
    unary,
    binary,
    cmp,
    idiv,
    cdq,
    allocate_stack,
    ret,
    jmp,
    jmp_cc,
    set_cc,
    label,
};

pub const Instruction = union(InstructionKind) {
    mov: Mov,
    unary: Unary,
    binary: Binary,
    cmp: Cmp,
    idiv: Operand,
    cdq,
    allocate_stack: usize,
    ret,
    jmp: []const u8,
    jmp_cc: JmpCC,
    set_cc: SetCC,
    label: []const u8,

    pub const Mov = struct {
        src: Operand,
        dst: Operand,

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.print("Mov{{ src: {f}, dst: {f} }}", .{
                self.src,
                self.dst,
            });
        }
    };

    pub const Unary = struct {
        operator: Operator,
        operand: Operand,

        pub const Operator = enum {
            neg,
            not,
        };

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.print("Unary{{ operator: {}, operand: {f} }}", .{
                self.operator,
                self.operand,
            });
        }
    };

    pub const Binary = struct {
        operator: Operator,
        src1: Operand,
        src2: Operand,

        pub const Operator = enum {
            add,
            sub,
            imul,
            @"and",
            @"or",
            xor,
            shl,
            sal,
            shr,
            sar,
        };

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.print("Binary{{ operator: {}, src1: {f}, src2: {f} }}", .{
                self.operator,
                self.src1,
                self.src2,
            });
        }
    };

    pub const Cmp = struct {
        src1: Operand,
        src2: Operand,

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.print("Cmp{{ src1: {f}, src2: {f} }}", .{
                self.src1,
                self.src2,
            });
        }
    };

    pub const CondCode = enum {
        e,
        ne,
        g,
        ge,
        l,
        le,
    };

    pub const JmpCC = struct {
        cond_code: CondCode,
        target: []const u8,

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.print("JmpCC{{ cond_code: {}, target: {s} }}", .{
                self.cond_code,
                self.target,
            });
        }
    };

    pub const SetCC = struct {
        cond_code: CondCode,
        dst: Operand,

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.print("SetCC{{ cond_code: {}, dst: {f} }}", .{
                self.cond_code,
                self.dst,
            });
        }
    };

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self) {
            .idiv => |operand| try writer.print("Idiv{{ {f} }}", .{operand}),
            .allocate_stack => |size| try writer.print("AllocateStack{{ {} }}", .{size}),
            .cdq => try writer.writeAll("Cdq"),
            .ret => try writer.writeAll("Ret"),
            .jmp => |target| try writer.print("Jump{{ {s} }}", .{target}),
            .label => |label| try writer.print("Label{{ {s} }}", .{label}),
            inline else => |ins| try writer.print("{f}", .{ins}),
        }
    }
};

pub const OperandKind = enum {
    imm,
    reg,
    pseudo,
    stack,
};

pub const Operand = union(OperandKind) {
    imm: u32, // only support 'u32' for now
    reg: Register,
    pseudo: []const u8,
    stack: isize,

    pub const Register = struct {
        base: Base,
        part: Part,

        pub const Part = enum {
            /// low byte
            lb,
            /// high byte
            hb,
            /// word
            w,
            /// double word
            dw,
            /// quad word
            qw,

            pub fn size(self: Part) usize {
                return switch (self) {
                    .lb, .hb => 8,
                    .w => 16,
                    .dw => 32,
                    .qw => 64,
                };
            }
        };

        pub const Base = enum {
            ax,
            dx,
            r10,
            r11,
            cx,
        };

        fn R(comptime base: Base, comptime part: Part) Register {
            return .{ .base = base, .part = part };
        }

        pub const al: Register = .R(.ax, .lb);
        pub const ah: Register = .R(.ax, .hb);
        pub const ax: Register = .R(.ax, .w);
        pub const eax: Register = .R(.ax, .dw);
        pub const rax: Register = .R(.ax, .qw);

        pub const dl: Register = .R(.dx, .lb);
        pub const dh: Register = .R(.dx, .hb);
        pub const dx: Register = .R(.dx, .w);
        pub const edx: Register = .R(.dx, .dw);
        pub const rdx: Register = .R(.dx, .qw);

        pub const r10b: Register = .R(.r10, .lb);
        pub const r10w: Register = .R(.r10, .w);
        pub const r10d: Register = .R(.r10, .dw);
        pub const r10: Register = .R(.r10, .qw);

        pub const r11b: Register = .R(.r11, .lb);
        pub const r11w: Register = .R(.r11, .w);
        pub const r11d: Register = .R(.r11, .dw);
        pub const r11: Register = .R(.r11, .qw);

        pub const cl: Register = .R(.cx, .lb);
        pub const ch: Register = .R(.cx, .hb);
        pub const cx: Register = .R(.cx, .w);
        pub const ecx: Register = .R(.cx, .dw);
        pub const rcx: Register = .R(.cx, .qw);

        const register_names = [_]std.meta.Tuple(&.{ Register, []const u8 }){
            .{ al, "al" },
            .{ ah, "ah" },
            .{ ax, "ax" },
            .{ eax, "eax" },
            .{ rax, "rax" },
            .{ dl, "dl" },
            .{ dh, "dh" },
            .{ dx, "dx" },
            .{ edx, "edx" },
            .{ rdx, "rdx" },
            .{ r10b, "r10b" },
            .{ r10w, "r10w" },
            .{ r10d, "r10d" },
            .{ r10, "r10" },
            .{ r11b, "r11b" },
            .{ r11w, "r11w" },
            .{ r11d, "r11d" },
            .{ r11, "r11" },
            .{ cl, "cl" },
            .{ ch, "ch" },
            .{ cx, "cx" },
            .{ ecx, "ecx" },
            .{ rcx, "rcx" },
        };

        pub fn name(self: Register) []const u8 {
            inline for (register_names) |entry|
                if (std.meta.eql(entry.@"0", self)) return entry.@"1";

            @panic("Can't find name for this register, please check the implementation.");
        }

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.print("Reg{{ base: {}, part: {} }}", .{
                self.base,
                self.part,
            });
        }
    };

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self) {
            .imm => |value| try writer.print("Imm{{ {} }}", .{value}),
            .reg => |reg| try writer.print("{any}", .{reg}),
            .pseudo => |name| try writer.print("Pseudo{{ {s} }}", .{name}),
            .stack => |size| try writer.print("Stack{{ {} }}", .{size}),
        }
    }
};
