const std = @import("std");
const node = @import("node.zig");
const common = @import("common.zig");

const GeneralContext = @import("GeneralContext.zig");
const tyref = @import("type_ref.zig");
const TypeRef = tyref.TypeRef;

const mem = std.mem;

pub const Store = struct {
    ctx: *GeneralContext,
    arena: std.heap.ArenaAllocator,
    storage: std.ArrayList(Data) = .empty,
    mapping: std.HashMapUnmanaged(Data, TypeRef, Data.Context, std.hash_map.default_max_load_percentage) = .empty,

    pub fn init(ctx: *GeneralContext) Store {
        var store = Store{
            .ctx = ctx,
            .arena = ctx.createLifetime(),
            .storage = std.ArrayList(Data).initCapacity(ctx.allocator, TypeRef.reserved()) catch @panic("OOM"),
        };
        store.storage.items.len = TypeRef.reserved();
        @memset(store.storage.items[0..TypeRef.reserved()], .primitive);
        return store;
    }

    pub fn intern(store: *Store, t: *const node.Type) TypeRef {
        return store.internImpl(t, true) catch @panic("OOM");
    }
    // After the top level call to 'intern' we clear the scratch arena. This
    // allows the memory to be valid throughout recursive calls to 'internImpl`
    fn internImpl(store: *Store, t: *const node.Type, reset: bool) !TypeRef {
        defer if (reset) {
            _ = store.ctx.scratch.reset(.retain_capacity);
        };

        const a = store.arena.allocator();
        const scratch = store.ctx.scratch.allocator();

        switch (t.*) {
            .builtin => |bi| {
                switch (bi.token.type) {
                    inline else => |tag| {
                        if (@hasField(TypeRef, @tagName(tag))) {
                            return @field(TypeRef, @tagName(tag));
                        }
                    },
                }
                unreachable;
            },
            .coll => |coll| {
                common.todo(coll.index_expr == null, "arrays", .{});
                return store.internDataStable(.{ .slice = try store.internImpl(coll.value_type, false) });
            },
            .sum => |sum| {
                var list = try scratch.alloc(SumField, sum.alts.len);
                for (sum.alts, 0..) |*alt, i| {
                    list[i] = switch (alt.*) {
                        .type => |*ty| .{
                            .name = null,
                            .type = try store.internImpl(ty, false),
                        },
                        .type_decl => |*td| .{
                            .name = td.name.text(),
                            .type = store.internDataStable(.{ .user = td }),
                        },
                        .dirty => unreachable,
                    };
                }
                const res = store.internData(.{ .sum = list });
                if (res.inserted) {
                    res.freeze(.{ .sum = try a.dupe(SumField, list) });
                }
                return res.id;
            },
            .tuple => |tup| {
                var list = try scratch.alloc(TypeRef, tup.types.len);
                for (tup.types, 0..) |*subt, i| {
                    list[i] = try store.internImpl(&subt.type, false);
                }
                const res = store.internData(.{ .tuple = list });
                if (res.inserted) {
                    res.freeze(.{ .tuple = try a.dupe(TypeRef, list) });
                }
                return res.id;
            },
            .@"struct" => |st| {
                var list = try scratch.alloc(StructField, st.fields.len);
                for (st.fields, 0..) |*f, i| {
                    list[i] = .{
                        .name = f.name.text(),
                        .type = try store.internImpl(&f.type, false),
                    };
                }
                const res = store.internData(.{ .@"struct" = list });
                if (res.inserted) {
                    res.freeze(.{ .@"struct" = try a.dupe(StructField, list) });
                }
                return res.id;
            },
            .@"enum" => |en| {
                var list = try scratch.alloc([]const u8, en.alts.len);
                for (en.alts, 0..) |alt, i| {
                    list[i] = alt.name.text();
                }
                const res = store.internData(.{ .@"enum" = list });
                if (res.inserted) {
                    res.freeze(.{ .@"enum" = try a.dupe([]const u8, list) });
                }
                return res.id;
            },
            .ptr => |p| {
                return store.internDataStable(.{
                    .ptr = try store.internImpl(p.child, false),
                });
            },
            .err => |e| {
                return store.internDataStable(.{
                    .err = try store.internImpl(e.child, false),
                });
            },
            .type_of => |to| {
                return store.internDataStable(.{
                    .type_of = try store.internImpl(to.child, false),
                });
            },
            .fun => |fun| {
                var params = try scratch.alloc(Fun.Param, fun.params.len);
                for (fun.params, 0..) |*param, i| {
                    params[i] = .{
                        .type = try store.internImpl(&param.type, false),
                        .unwrap = param.unwrap,
                    };
                }
                const return_type = if (fun.return_type) |ret|
                    try store.internImpl(ret, false)
                else
                    .unit;
                const res = store.internData(.{
                    .fun = .{
                        .params = params,
                        .return_type = return_type,
                    },
                });
                if (res.inserted) {
                    res.freeze(.{
                        .fun = .{
                            .params = try a.dupe(Fun.Param, params),
                            .return_type = return_type,
                        },
                    });
                }
                return res.id;
            },
            .ident => |id| {
                const resolved = id.resolves_to.?;
                switch (resolved.data) {
                    .type_decl => |td| return store.internDataStable(.{ .user = td }),
                    .sub_type => |subt| return store.internImpl(&subt.type, false),
                    else => {},
                }
                unreachable;
            },
            .selector => |sel| {
                const resolved = sel.resolves_to.?;
                switch (resolved.data) {
                    .type_decl => |td| return store.internDataStable(.{ .user = td }),
                    .sub_type => |subt| return store.internImpl(&subt.type, false),
                    else => {},
                }
                unreachable;
            },
            .dirty => unreachable,
        }
    }

    const InternResult = struct {
        id: TypeRef,
        data_ptr: *Data,
        inserted: bool,
        store: *Store,

        fn freeze(res: InternResult, data: Data) void {
            res.data_ptr.* = data;
            res.store.storage.items[@intFromEnum(res.id)] = data;
        }
    };

    fn internData(store: *Store, data: Data) InternResult {
        const result = store.mapping.getOrPutContext(store.ctx.allocator, data, .{ .store = store }) catch @panic("OOM");
        if (result.found_existing) {
            return .{
                .id = result.value_ptr.*,
                .data_ptr = result.key_ptr,
                .inserted = false,
                .store = store,
            };
        }
        result.value_ptr.* = @enumFromInt(store.storage.items.len);
        store.storage.ensureTotalCapacity(store.ctx.allocator, store.storage.items.len + 1) catch @panic("OOM");
        store.storage.items.len += 1;
        return .{
            .id = result.value_ptr.*,
            .data_ptr = result.key_ptr,
            .inserted = true,
            .store = store,
        };
    }

    pub fn internDataStable(store: *Store, data: Data) TypeRef {
        const res = store.internData(data);
        if (res.inserted) {
            res.freeze(data);
        }
        return res.id;
    }

    pub fn get(store: *const Store, id: TypeRef) Data {
        return store.storage.items[@intFromEnum(id)];
    }

    pub fn update(store: *Store, id: TypeRef, data: Data) void {
        store.storage.items[@intFromEnum(id)] = data;
    }

    pub fn deinit(store: *Store) void {
        store.storage.deinit(store.ctx.allocator);
        store.mapping.deinit(store.ctx.allocator);
        store.arena.deinit();
    }
};

pub const Data = union(enum) {
    user: *node.TypeDecl,
    fun: Fun,
    ptr: TypeRef,
    slice: TypeRef,
    err: TypeRef,
    type_of: TypeRef,
    sum: []SumField,
    tuple: []TypeRef,
    @"enum": [][]const u8,
    @"struct": []StructField,
    primitive,

    pub fn formatWithStore(
        data: Data,
        store: *const Store,
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (data) {
            .user => |user| {
                try writer.writeAll(user.name.text());
            },
            .fun => |fun| {
                try writer.writeAll("fun (");
                for (fun.params, 0..) |param, index| {
                    if (index != 0) {
                        try writer.writeAll(", ");
                    }
                    if (param.unwrap) {
                        try writer.writeAll("..");
                    }
                    try formatView(store, param.type).format(writer);
                }
                try writer.writeByte(')');
                if (fun.return_type != .unit) {
                    try writer.print("-> {f}", .{
                        formatView(store, fun.return_type),
                    });
                }
            },
            .ptr => |child| {
                try writer.print("*{f}", .{
                    formatView(store, child),
                });
            },
            .slice => |child| {
                try writer.print("[]{f}", .{
                    formatView(store, child),
                });
            },
            .err => |child| {
                try writer.print("!{f}", .{
                    formatView(store, child),
                });
            },
            .type_of => |child| {
                try writer.print("type_of({f})", .{
                    formatView(store, child),
                });
            },
            .sum => |alts| {
                try writer.writeByte('(');
                for (alts, 0..) |alt, index| {
                    if (index != 0) {
                        try writer.writeAll("| ");
                    }
                    try formatView(store, alt.type).format(writer);
                }
                try writer.writeByte(')');
            },
            .tuple => |alts| {
                try writer.writeByte('(');
                for (alts, 0..) |alt, index| {
                    if (index != 0) {
                        try writer.writeAll(", ");
                    }
                    try formatView(store, alt).format(writer);
                }
                try writer.writeByte(')');
            },
            .@"enum" => |enumerators| {
                try writer.writeAll("enum {");
                for (enumerators, 0..) |enumerator, index| {
                    if (index != 0) {
                        try writer.writeAll(", ");
                    }
                    try writer.writeAll(enumerator);
                }
                try writer.writeByte('}');
            },
            .@"struct" => |fields| {
                try writer.writeAll("struct {");
                for (fields, 0..) |field, index| {
                    if (index != 0) {
                        try writer.writeAll(", ");
                    }
                    try writer.print("{s}: {f}", .{
                        field.name,
                        formatView(store, field.type),
                    });
                }
                try writer.writeByte('}');
            },
            .primitive => try writer.writeAll("primitive type"),
        }
    }

    pub const Context = struct {
        store: *const Store,

        pub fn hash(_: Context, data: Data) u64 {
            var hasher = std.hash.Wyhash.init(0);
            switch (data) {
                .user => |u| return @intFromPtr(u),
                .fun => |fun| {
                    for (fun.params) |param| {
                        hasher.update(mem.asBytes(&param.type));
                        hasher.update(mem.asBytes(&param.unwrap));
                    }
                    hasher.update(mem.asBytes(&fun.return_type));
                },
                .sum => |sum| {
                    for (sum) |alt| {
                        hasher.update(mem.asBytes(&alt.type));
                    }
                    hasher.update(mem.asBytes(&Data.sum));
                },
                .tuple => |tuple| {
                    for (tuple) |alt| {
                        hasher.update(mem.asBytes(&alt));
                    }
                    hasher.update(mem.asBytes(&Data.tuple));
                },
                .@"struct" => |st| {
                    for (st) |f| {
                        hasher.update(f.name);
                        hasher.update(mem.asBytes(&f.type));
                    }
                },
                .@"enum" => |en| {
                    for (en) |alt| {
                        hasher.update(alt);
                    }
                },
                .err => |e| {
                    hasher.update(mem.asBytes(&Data.err));
                    hasher.update(mem.asBytes(&e));
                },
                .type_of => |t| {
                    hasher.update(mem.asBytes(&Data.type_of));
                    hasher.update(mem.asBytes(&t));
                },
                .ptr => |p| {
                    hasher.update(mem.asBytes(&Data.ptr));
                    hasher.update(mem.asBytes(&p));
                },
                .slice => |s| {
                    hasher.update(mem.asBytes(&Data.slice));
                    hasher.update(mem.asBytes(&s));
                },
                inline else => |_, tag| hasher.update(mem.asBytes(&tag)),
            }
            return hasher.final();
        }

        pub fn eql(_: Context, a: Data, b: Data) bool {
            if (std.meta.activeTag(a) != std.meta.activeTag(b)) {
                return false;
            }
            switch (a) {
                .user => |u| return u == b.user,
                .fun => |fun| {
                    if (fun.params.len != b.fun.params.len) {
                        return false;
                    }
                    for (fun.params, b.fun.params) |pa, pb| {
                        if (pa.type != pb.type) {
                            return false;
                        }
                        if (pa.unwrap != pb.unwrap) {
                            return false;
                        }
                    }
                    return fun.return_type == b.fun.return_type;
                },
                .sum => |sum| {
                    if (sum.len != b.sum.len) {
                        return false;
                    }
                    for (sum, b.sum) |alt_a, alt_b| {
                        if (alt_a.type != alt_b.type) {
                            return false;
                        }
                    }
                    return true;
                },
                .tuple => |tuple| {
                    if (tuple.len != b.tuple.len) {
                        return false;
                    }
                    return mem.eql(TypeRef, tuple, b.tuple);
                },
                .@"struct" => |st| {
                    if (st.len != b.@"struct".len) {
                        return false;
                    }
                    for (st, b.@"struct") |fa, fb| {
                        if (fa.type != fb.type or
                            !mem.eql(u8, fa.name, fb.name))
                        {
                            return false;
                        }
                    }
                },
                .err => |e| return e == b.err,
                .type_of => |t| return t == b.type_of,
                .ptr => |p| return p == b.ptr,
                .slice => |s| return s == b.slice,
                else => {},
            }
            return true;
        }
    };
};

pub const Fun = struct {
    params: []Param,
    return_type: TypeRef,

    pub const Param = struct {
        type: TypeRef,
        unwrap: bool,
    };
};

pub const StructField = struct {
    name: []const u8,
    type: TypeRef,
};

pub const SumField = struct {
    name: ?[]const u8,
    type: TypeRef,
};

pub const Array = struct {
    child: TypeRef,
    capacity: usize,
};

pub const FormatView = struct {
    ref: TypeRef,
    store: *const Store,

    pub fn format(
        view: FormatView,
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (view.ref) {
            .dirty => try writer.writeAll("<type with error>"),
            .unset => try writer.writeAll("<unset type>"),
            inline .u8, .s8, .u16, .s16, .u32, .s32, .u64, .s64, .f32, .f64, .str, .unit => |tag| try writer.writeAll(@tagName(tag)),
            else => |index| {
                try view.store.get(index).formatWithStore(view.store, writer);
            },
        }
    }
};

pub fn formatView(store: *const Store, type_ref: TypeRef) FormatView {
    return .{
        .ref = type_ref,
        .store = store,
    };
}
