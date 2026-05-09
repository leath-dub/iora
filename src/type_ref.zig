const std = @import("std");

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
    unit,
    _,

    pub fn reserved() u8 {
        return std.meta.fields(TypeRef).len;
    }

    pub fn isNumeric(tr: TypeRef) bool {
        return tr.isInteger() or
            tr.isFloatingPoint();
    }

    pub fn isInteger(tr: TypeRef) bool {
        return switch (tr) {
            .u8, .s8, .u16, .s16, .u32, .s32, .u64, .s64 => true,
            else => false,
        };
    }

    pub fn isFloatingPoint(tr: TypeRef) bool {
        return switch (tr) {
            .f32, .f64 => true,
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
