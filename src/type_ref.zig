const std = @import("std");

pub const TypeVar = union(enum) {
    id: TypeRef,
    ptr: *TypeVar,
    int, // TODO: add optional signed constraint
    float,
    inferred: struct {
        name: []const u8,
        position: usize,
    },
    unset,
    dirty,

    pub fn isPartial(va: TypeVar) bool {
        return switch (va.dealiasConst().*) {
            .int,
            .float,
            .inferred => true,
            else => false,
        };
    }

    pub fn dealias(va_: *TypeVar) *TypeVar {
        var va = va_;
        return again: switch (va.*) {
            .ptr => |ptr| {
                va = ptr;
                continue :again va.*;
            },
            else => va,
        };
    }

    pub fn dealiasConst(va_: *const TypeVar) *const TypeVar {
        var va = va_;
        return again: switch (va.*) {
            .ptr => |ptr| {
                va = ptr;
                continue :again va.*;
            },
            else => va,
        };
    }

    pub fn canon(va: TypeVar) ?TypeRef {
        return switch (va.dealiasConst().*) {
            .id => |id| id,
            else => null,
        };
    }

    pub const dont_walk = true;
};

pub const TypeRef = enum(u32) {
    unset,
    // Used as a sentinel value by early type checking/name resolution passes
    // to not report spurious errors
    dirty,
    u8,
    s8,
    u16,
    s16,
    u32,
    s32,
    u64,
    s64,
    f32,
    f64,
    str,
    type,
    unit,
    _,

    pub fn reserved() u8 {
        return std.meta.fields(TypeRef).len;
    }

    pub fn isInteger(tr: TypeRef) bool {
        return switch (tr) {
            .u8,
            .s8,
            .u16,
            .s16,
            .u32,
            .s32,
            .u64,
            .s64 => true,
            else => false,
        };
    }

    pub fn isFloatingPoint(tr: TypeRef) bool {
        return switch (tr) {
            .f32,
            .f64 => true,
            else => false,
        };
    }

    pub fn format(
        tr: TypeRef,
        w: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        inline for (std.meta.tags(TypeRef)) |tag| {
            if (tr == tag) {
                try w.print("{t}", .{tr});
                return;
            }
        }
        try w.print("{d}", .{@intFromEnum(tr)});
    }
};
