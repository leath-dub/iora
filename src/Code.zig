const std = @import("std");
const Io = std.Io;

pub const Offset = usize;

const mem = std.mem;
const heap = std.heap;

path: []const u8,
text: []const u8,
lines: []usize,

const Self = @This();

pub fn init(arena: *heap.ArenaAllocator, path: []const u8, text: []const u8) !Self {
    var scratch_alloc = heap.DebugAllocator(.{}){};
    defer std.debug.assert(scratch_alloc.deinit() != .leak);
    const alloc = scratch_alloc.allocator();

    var lines: std.ArrayList(usize) = .{};

    // Always have initial line 0
    try lines.append(alloc, 0);

    for (text, 0..) |c, i| {
        if (c == '\n') {
            try lines.append(alloc, i + 1);
        }
    }

    defer lines.deinit(alloc);

    return .{
        .path = path,
        .text = text,
        .lines = try arena.allocator().dupe(usize, lines.items),
    };
}

inline fn tween(low: usize, high: usize) usize {
    std.debug.assert(low <= high);
    return low + (high - low) / 2;
}

inline fn lineHasOffset(code: Self, line: usize, at: Offset) bool {
    const last_line = line == code.lines.len - 1;
    if (last_line) {
        std.debug.assert(at < code.text.len);
        return code.lines[line] <= at;
    }
    return code.lines[line] <= at and at < code.lines[line + 1];
}

pub fn lineAndColumn(code: Self, at: Offset) std.meta.Tuple(&.{ usize, usize }) {
    // if 'at' is out of range it means we want to point at EOF
    if (at >= code.text.len) {
        const last_line = code.lines.len - 1;
        return .{ last_line, code.text.len - code.lines[last_line] };
    }

    var low: usize = 0;
    var high = code.lines.len - 1;
    var middle: usize = undefined;

    while (low <= high) {
        middle = tween(low, high);
        if (code.lineHasOffset(middle, at)) {
            return .{ middle, at - code.lines[middle] };
        }
        if (at < code.lines[middle]) {
            high = middle;
        }
        if (at > code.lines[middle + 1]) {
            low = middle + 1;
        }
    }

    std.log.err("offset: {d} in text (length = {d})", .{ at, code.text.len });
    @panic("offset out of range");
}

pub fn lineText(code: Self, line: usize) []const u8 {
    const start = code.lines[line];
    const end = if (line == code.lines.len - 1)
        code.text.len
    else
        // minus 1 as we don't want to include the '\n' itself
        code.lines[line + 1] - 1;
    return code.text[start .. end];
}

pub fn raise(code: Self, w: *Io.Writer, at: Offset, comptime fmt: []const u8, args: anytype) !void {
    const line, const column = code.lineAndColumn(at);
    try w.print("[{s}:{d}:{d}] ", .{ code.path, line + 1, column + 1 });
    try w.print(fmt ++ "\n", args);
    try w.print("{s}\n", .{code.lineText(line)});
    for (0..column) |_| {
        try w.print(" ", .{});
    }
    try w.print("^\n", .{});
}

test "lineAndColumn" {
    var arena = heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const testCase = struct {
        fn testCase(a: *heap.ArenaAllocator, line: usize, column: usize, text: []const u8) !void {
            const code = try Self.init(a, "<test input>", text);
            const res = code.lineAndColumn(std.mem.indexOfScalar(u8, code.text, '!').?);
            try std.testing.expectEqual(line, res.@"0");
            try std.testing.expectEqual(column, res.@"1");
        }
    }.testCase;

    try testCase(&arena, 3, 6,
        \\ line one
        \\ fkldjslk
        \\ ----
        \\ line !wo
        \\ fjdskljf
        \\ line three with this
    );
    try testCase(&arena, 0, 0,
        \\!line one
        \\ fkldjslk
        \\ ----
    );
    try testCase(&arena, 2, 5,
        \\ line one
        \\ fkldjslk
        \\ ----!
    );
    try testCase(&arena, 0, 0, "!");
    try testCase(&arena, 0, 1, " !");

    try testCase(&arena, 1, 88,
        \\ const testCase = struct {
        \\     fn testCase(a: *heap.ArenaAllocator, line: usize, column: usize, text: []const u8) !void {
        \\         const code = try Self.init(a, text);
        \\         const res = code.lineAndColumn(std.mem.indexOfScalar(u8, code.text, '!').?);
        \\         try std.testing.expectEqual(line, res.@"0");
        \\         try std.testing.expectEqual(column, res.@"1");
        \\     }
        \\ }.testCase;
    );
}
