const std = @import("std");
const fs = std.fs;
const heap = std.heap;
const mem = std.mem;
const fmt = std.fmt;

const unicode = @import("unicode_types.zig");
const unicode_data = @embedFile("UnicodeData.txt");

pub fn main() !void {
    var alloc: heap.DebugAllocator(.{}) = .{};
    defer {
        _ = alloc.detectLeaks();
        _ = alloc.deinit();
    }
    const a = alloc.allocator();

    var args = std.process.args();
    _ = args.next();
    const file_name = args.next().?;

    var f = try fs.cwd().createFile(file_name, .{});
    defer f.close();
    var buf: [4096]u8 = undefined;
    var wtr = f.writer(&buf);
    defer wtr.interface.flush() catch unreachable;

    var gc_to_codep = std.EnumArray(unicode.GeneralCategory, std.ArrayList(u32)).initFill(.{});

    var it = mem.splitScalar(u8, unicode_data, '\n');
    while (it.next()) |ln| {
        var fields = mem.splitScalar(u8, ln, ';');
        const codep = std.fmt.parseInt(u32, fields.first(), 16) catch {
            continue;
        };
        _ = fields.next().?;
        const gc = std.meta.stringToEnum(unicode.GeneralCategory, fields.next().?).?;
        try gc_to_codep.getPtr(gc).append(a, codep);
    }

    var final_data: std.ArrayList(unicode.CodepointGroup) = .{};
    defer final_data.deinit(a);

    var gc_it = gc_to_codep.iterator();
    while (gc_it.next()) |*entry| {
        const codeps = entry.value;

        std.mem.sort(u32, codeps.items, {}, std.sort.asc(u32));

        if (codeps.items.len != 0) {
            var group: unicode.CodepointGroup = .{
                .start = codeps.items[0],
                .end = codeps.items[0],
                .category = entry.key,
            };
            for (codeps.items[1..], 1..) |codep, i| {
                const prev = codeps.items[i - 1];
                const curr = codep;
                if (curr - prev == 1) {
                    group.end = curr;
                } else {
                    try final_data.append(a, group);
                    group.start = curr;
                    group.end = group.start;
                }
            }
        }

        codeps.deinit(a);
    }

    mem.sort(unicode.CodepointGroup, final_data.items, {}, unicode.CodepointGroup.lessThan);

    try wtr.interface.print("const unicode = @import(\"unicode_types\");\n\n", .{});
    try wtr.interface.print("pub const data = [_]unicode.CodepointGroup{{\n", .{});

    for (final_data.items) |grp| {
        try wtr.interface.print(
            "    .{{ .start = {any}, .end = {any}, .category = .{s} }},\n",
            .{
            grp.start,
            grp.end, @tagName(grp.category) });
    }

    try wtr.interface.print("}};\n", .{});
}
