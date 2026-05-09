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
        return T == std.AutoHashMapUnmanaged(K, V);
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

pub fn ChunkedStack(comptime T: type) type {
    return struct {
        const chunk_size: usize = 1024;

        const Chunk = struct {
            data: [chunk_size]T = undefined,
            len: usize = 0,
            node: std.SinglyLinkedList.Node = .{},
        };

        chunks: std.SinglyLinkedList = .{},

        // The pool is used for allocating linked list nodes
        pool: std.heap.memory_pool.Managed(Chunk),

        pub fn init(allocator: std.mem.Allocator) @This() {
            return .{
                .pool = .init(allocator),
            };
        }

        pub fn deinit(s: *@This()) void {
            s.pool.deinit();
        }

        pub fn createNewChunk(s: *@This()) !*Chunk {
            const chunk = try s.pool.create();
            chunk.* = .{};
            return chunk;
        }

        pub fn push(s: *@This(), value: T) !void {
            if (s.chunks.len() == 0) {
                // First allocation
                const chunk = try s.createNewChunk();
                s.chunks.prepend(&chunk.node);
            }

            var chunk: *Chunk = @fieldParentPtr("node", s.chunks.first.?);
            if (chunk_size - chunk.len <= 0) {
                // No room in this chunk
                chunk = try s.createNewChunk();
                s.chunks.prepend(&chunk.node);
            }

            chunk.data[chunk.len] = value;
            chunk.len += 1;
        }

        pub fn pop(s: *@This()) T {
            std.debug.assert(s.chunks.len() != 0);
            const chunk: *Chunk = @fieldParentPtr("node", s.chunks.first.?);
            std.debug.assert(chunk.len != 0);
            chunk.len -= 1;
            defer {
                if (chunk.len == 0) {
                    const dead_chunk: *Chunk =
                        @fieldParentPtr("node", s.chunks.popFirst().?);
                    std.debug.assert(dead_chunk == chunk);
                    s.pool.destroy(dead_chunk);
                }
            }
            return chunk.data[chunk.len];
        }

        pub fn topPtrConst(s: *const @This()) ?*const T {
            if (s.chunks.len() == 0) {
                return null;
            }
            const chunk: *Chunk = @fieldParentPtr("node", s.chunks.first.?);
            if (chunk.len == 0) {
                return null;
            }
            return &chunk.data[chunk.len - 1];
        }

        pub fn topPtr(s: *@This()) ?*T {
            return @constCast(s.topPtrConst());
        }

        pub fn top(s: @This()) ?T {
            if (s.topPtrConst()) |ptr| {
                return ptr.*;
            }
            return null;
        }
    };
}
