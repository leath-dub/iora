const std = @import("std");

pub fn isArrayList(comptime T: type) bool {
    if (@typeInfo(T) != .@"struct" or !@hasDecl(T, "Slice")) {
        return false;
    }

    const slice_info = @typeInfo(T.Slice);
    const ptr_info = if (slice_info != .pointer) {
        return false;
    } else slice_info.pointer;

    return T == std.ArrayList(ptr_info.child);
}

pub fn AutoHashMapUnmanagedKVTuple(comptime T: type) ?std.meta.Tuple(&.{ type, type }) {
    if (@typeInfo(T) != .@"struct" or
        !@hasDecl(T, "KV") or
        @typeInfo(T.KV) != .@"struct")
    {
        return null;
    }

    if (!@hasField(T.KV, "key") or !@hasField(T.KV, "value")) {
        return null;
    }

    const K = @FieldType(T.KV, "key");
    const V = @FieldType(T.KV, "value");

    return .{ K, V };
}

pub fn isAutoHashMapUnmanaged(comptime T: type) bool {
    if (AutoHashMapUnmanagedKVTuple(T)) |r| {
        const K, const V = r;
        return T == std.AutoArrayHashMapUnmanaged(K, V);
    }
    return false;
}

pub fn Unwrap(comptime T: type) type {
    var Unwrapped = T;
    while (@typeInfo(Unwrapped) == .optional) {
        Unwrapped = @typeInfo(Unwrapped).optional.child;
    }
    return Unwrapped;
}
