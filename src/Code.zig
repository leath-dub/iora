const std = @import("std");

const mem = std.mem;
const heap = std.heap;

text: []const u8,
lines: []usize,

const Self = @This();

pub fn init(arena: *heap.ArenaAllocator, text: []const u8) !Self {
    var scratch_alloc = heap.DebugAllocator(.{}){};
    defer std.debug.assert(scratch_alloc.deinit() != .leak);
    const alloc = scratch_alloc.allocator();

    var lines: std.ArrayList(usize) = .{};

    for (text, 0..) |c, i| {
        if (c == '\n') {
            try lines.append(alloc, i + 1);
        }
    }

    defer lines.deinit(alloc);

    return .{
        .text = text,
        .lines = try arena.allocator().dupe(usize, lines.items),
    };
}
