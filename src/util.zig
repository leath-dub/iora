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
        const Chunk = struct {
            data: []T,
            len: usize,
            cap: usize,
            node: std.SinglyLinkedList.Node = .{},
        };

        chunks: std.SinglyLinkedList = .{},
        last_capacity: usize = 1024,

        // The arena is used for allocating data buffers
        arena: std.heap.ArenaAllocator,
        // The pool is used for allocating linked list nodes
        pool: std.heap.memory_pool.Managed(Chunk),

        pub fn init(allocator: std.mem.Allocator) @This() {
            return .{
                .arena = .init(allocator),
                .pool = .init(allocator),
            };
        }

        pub fn deinit(s: *@This()) void {
            s.arena.deinit();
            s.pool.deinit();
        }

        fn createNewChunk(s: *@This(), capacity: usize) !*Chunk {
            const data = try s.arena.allocator().alloc(T, capacity);
            const chunk = try s.pool.create();
            chunk.* = .{
                .data = data,
                .cap = capacity,
                .len = 0,
                .node = .{},
            };
            return chunk;
        }

        pub fn push(s: *@This(), value: T) !void {
            if (s.chunks.len() == 0) {
                // First allocation
                const chunk = try s.createNewChunk(s.last_capacity);
                s.chunks.prepend(&chunk.node);
            }

            var chunk: *Chunk = @fieldParentPtr("node", s.chunks.first.?);
            if (chunk.cap - chunk.len <= 0) {
                // No room in this chunk
                const capacity = s.last_capacity * 2;
                chunk = try s.createNewChunk(capacity);
                s.last_capacity = capacity;
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
