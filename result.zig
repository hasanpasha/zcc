const std = @import("std");

pub fn Result(T: type, E: type) type {
    return union(enum) {
        ok: OkType,
        err: ErrType,

        pub const OkType = T;
        pub const ErrType = E;

        const Self = @This();

        pub fn Ok(v: OkType) Self {
            return .{ .ok = v };
        }

        pub fn Err(e: ErrType) Self {
            return .{ .err = e };
        }

        fn MapResultType(mapper: anytype) type {
            const mapper_type = @TypeOf(mapper);
            const type_info = @typeInfo(mapper_type);

            if (type_info != .@"fn")
                @compileError("expected a function, but found " ++ @typeName(mapper_type));

            const mapper_fn_info = type_info.@"fn";

            if (mapper_fn_info.return_type == null)
                @compileError("mapper function should specify return type");

            const mapper_return_type = mapper_fn_info.return_type.?;

            if (mapper_fn_info.params.len != 1)
                @compileError("mapper function should accept one argument");

            const mapper_parameter = mapper_fn_info.params[0];

            if (mapper_parameter.type == null)
                @compileError("mapper function parameter should have a type");

            const mapper_parameter_type = mapper_parameter.type.?;

            if (mapper_parameter_type == OkType) {
                return Result(mapper_return_type, ErrType);
            } else if (mapper_parameter_type == ErrType) {
                return Result(OkType, mapper_return_type);
            } else {
                @compileError("mapper function parameter type should be either '" ++ @typeName(OkType) ++ "' or '" ++ @typeName(ErrType) ++ "', but found '" ++ @typeName(mapper_parameter_type) ++ "'.");
            }
        }

        pub fn map_ok(self: Self, mapper: anytype) MapResultType(mapper) {
            return switch (self) {
                .ok => |val| .Ok(mapper(val)),
                .err => |err| .Err(err),
            };
        }

        pub fn map_err(self: Self, mapper: anytype) MapResultType(mapper) {
            return switch (self) {
                .ok => |val| .Ok(val),
                .err => |err| .Err(mapper(err)),
            };
        }

        pub fn maybe_ok(self: Self) ?OkType {
            if (self == .ok) return self.ok;
            return null;
        }

        pub fn maybe_err(self: Self) ?ErrType {
            if (self == .err) return self.err;
            return null;
        }

        pub fn panic(err: anytype) noreturn {
            std.log.err("{f}", .{err});
            std.process.exit(1);
        }

        pub fn unwrap(self: Self) OkType {
            return switch (self) {
                .ok => |val| val,
                .err => |err| panic(err),
            };
        }

        pub fn toErrorUnion(self: Self, err: anyerror) anyerror!OkType {
            return switch (self) {
                .ok => |val| val,
                .err => |_| err,
            };
        }
    };
}
