const std = @import("std");
const node = @import("node.zig");

pub fn unqualTypeName(comptime T: type) []const u8 {
    const qualName = @typeName(T);
    if (std.mem.lastIndexOfScalar(u8, qualName, '.')) |index| {
        return qualName[index + 1 ..];
    }
    return qualName;
}

pub fn resolveLocal(scope: *const node.Scope, id: *const node.Ident) ?node.Symbol {
    return scope.get(id.text());
}

pub fn resolve(scope_: *const node.Scope, id: *const node.Ident) ?node.Symbol {
    var scope: ?*const node.Scope = scope_;
    var item: ?node.Symbol = null;
    while (item == null and scope != null) {
        item = resolveLocal(scope.?, id);
        scope = scope.?.parent;
    }
    return item;
}

pub const LookupMode = enum {
    local,
    lexical,
};

pub const ResolveConfig = struct {
    symbol: ?node.Symbol = null,
    lookup_mode: LookupMode = .lexical,
};

pub var index_name_buf: [4096]u8 = undefined;

pub fn indexName(buf: []u8, index: usize) []const u8 {
    return std.fmt.bufPrint(buf, "{d}", .{index}) catch @panic("format error");
}

pub fn todoNoReturn(comptime fmt: []const u8, args: anytype) noreturn {
    std.log.scoped(.todo).debug(fmt, args);
    std.process.exit(1);
}

pub fn todo(cond: bool, comptime fmt: []const u8, args: anytype) void {
    if (cond) return;
    std.log.scoped(.todo).debug(fmt, args);
    std.process.exit(1);
}

pub fn resolveSelector(symbol: node.Symbol, field: *const node.Ident, out: *?node.Symbol) void {
    var final: ?node.Symbol = symbol;
    defer out.* = final;

    if (final) |s| {
        again: switch (s.data) {
            .type_decl => |td| {
                // First try the type scope
                final = resolveLocal(&td.scope, field);

                // Next try local sub-scope
                if (final == null) {
                    const fallback_scope = switch (td.type) {
                        .tuple => |*tup| &tup.scope,
                        .sum => |*sum| &sum.scope,
                        .@"enum" => |*en| &en.scope,
                        .selector => |*sel| blk: {
                            if (sel.resolves_to) |ss| {
                                continue :again ss.data;
                            }
                            break :blk null;
                        },
                        else => null,
                    };
                    if (fallback_scope) |fs| {
                        final = resolveLocal(fs, field);
                    }
                }
            },
            else => {},
        }
    }
}
