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
    storage: std.ArrayList(Info) = .empty,
    mapping: std.HashMapUnmanaged(Info, TypeRef, Info.Context, std.hash_map.default_max_load_percentage) = .empty,

    pub fn init(ctx: *GeneralContext) Store {
        var store = Store{
            .ctx = ctx,
            .arena = ctx.createLifetime(),
            .storage = std.ArrayList(Info).initCapacity(ctx.allocator, TypeRef.reserved()) catch @panic("OOM"),
        };
        store.storage.items.len = TypeRef.reserved();
        @memset(store.storage.items[0..TypeRef.reserved()], .{});
        return store;
    }

    pub fn intern(store: *Store, t: *const node.Type) TypeRef {
        defer _ = store.ctx.scratch.reset(.retain_capacity);
        return store.internImpl(t) catch @panic("OOM");
    }
    // After the top level call to 'intern' we clear the scratch arena. This
    // allows the memory to be valid throughout recursive calls to 'internImpl`
    fn internImpl(store: *Store, t: *const node.Type) !TypeRef {
        const a = store.arena.allocator();
        const scratch = store.ctx.scratch.allocator();

        switch (t.*) {
            .builtin => |bi| {
                const prim_t = base: switch (bi.token.type) {
                    inline else => |tag| {
                        if (@hasField(TypeRef, @tagName(tag))) {
                            break :base @field(TypeRef, @tagName(tag));
                        }
                        unreachable;
                    },
                };
                if (t.isLinear() or t.isWeak()) {
                    return store.internInfoStable(.init(.{ .builtin = prim_t }, t));
                }
                return prim_t;
            },
            .coll => |coll| {
                common.todo(coll.index_expr == null, "arrays", .{});
                return store.internInfoStable(.init(.{ .slice = try store.internImpl(coll.value_type) }, t));
            },
            .sum => |sum| {
                var list = try scratch.alloc(SumField, sum.alts.len);
                for (sum.alts, 0..) |*alt, i| {
                    list[i] = switch (alt.*) {
                        .type => |*ty| .{
                            .name = null,
                            .type = try store.internImpl(ty),
                        },
                        .type_decl => |*td| .{
                            .name = td.name.text(),
                            .type = store.internInfoStable(.init(.{ .user = .{
                                .name = td.name.text(),
                                .source = td.head.position,
                                .type = &td.symbol,
                            } }, t)),
                        },
                        .dirty => unreachable,
                    };
                }
                const res = store.internInfo(.init(.{ .sum = .init(list) }, t));
                if (res.inserted) {
                    res.freeze(.init(.{ .sum = .init(try a.dupe(SumField, list)) }, t));
                }
                return res.id;
            },
            .tuple => |tup| {
                var list = try scratch.alloc(TypeRef, tup.types.len);
                for (tup.types, 0..) |*subt, i| {
                    list[i] = try store.internImpl(&subt.type);
                }
                const res = store.internInfo(.init(.{ .tuple = .init(list) }, t));
                if (res.inserted) {
                    res.freeze(.init(.{ .tuple = .init(try a.dupe(TypeRef, list)) }, t));
                }
                return res.id;
            },
            .@"struct" => |st| {
                var list = try scratch.alloc(StructField, st.fields.len);
                for (st.fields, 0..) |*f, i| {
                    list[i] = .{
                        .name = f.name.text(),
                        .type = try store.internImpl(&f.type),
                    };
                }
                const res = store.internInfo(.init(.{ .@"struct" = .init(list) }, t));
                if (res.inserted) {
                    res.freeze(.init(.{ .@"struct" = .init(try a.dupe(StructField, list)) }, t));
                }
                return res.id;
            },
            .@"enum" => |en| {
                var list = try scratch.alloc([]const u8, en.alts.len);
                for (en.alts, 0..) |alt, i| {
                    list[i] = alt.name.text();
                }
                const res = store.internInfo(.init(.{ .@"enum" = .init(list) }, t));
                if (res.inserted) {
                    res.freeze(.init(.{ .@"enum" = .init(try a.dupe([]const u8, list)) }, t));
                }
                return res.id;
            },
            .ptr => |p| {
                return store.internInfoStable(.init(.{
                    .ptr = try store.internImpl(p.child),
                }, t));
            },
            .err => |e| {
                return store.internInfoStable(.init(.{
                    .err = try store.internImpl(e.child),
                }, t));
            },
            .type_of => |to| {
                return store.internInfoStable(.init(.{
                    .type_of = try store.internImpl(to.child),
                }, t));
            },
            .fun => |fun| {
                var params = try scratch.alloc(Fun.Param, fun.params.len);
                var bindings = try scratch.alloc(node.Ident, fun.params.len);
                for (fun.params, 0..) |param, i| {
                    params[i] = .{
                        .type = try store.internImpl(&param.type),
                        .unpack = param.unpack,
                    };
                    bindings[i] = param.name;
                }
                const return_type = if (fun.return_type) |ret|
                    try store.internImpl(ret)
                else
                    .unit;
                const sig_res = store.internInfo(.init(.{
                    .fun_sig = .{
                        .params = params,
                        .return_type = return_type,
                    },
                }, t));
                if (sig_res.inserted) {
                    sig_res.freeze(.init(.{
                        .fun_sig = .{
                            .params = try a.dupe(Fun.Param, params),
                            .return_type = return_type,
                        },
                    }, t));
                }
                const sig = sig_res.id;
                const res = store.internInfo(.init(.{
                    .fun = .{
                        .signature = .{ .ref = sig },
                        .bindings = bindings,
                    },
                }, t));
                if (res.inserted) {
                    res.freeze(.init(.{
                        .fun = .{
                            .signature = .{ .ref = sig },
                            .bindings = try a.dupe(node.Ident, bindings),
                        },
                    }, t));
                }
                return res.id;
            },
            .ident => |id| {
                const resolved = id.resolves_to.?;
                switch (resolved.data) {
                    .type => |tp| {
                        std.debug.assert(tp.id != .unset);
                        return store.internInfoStable(.init(.{ .user = .{
                            .name = resolved.name,
                            .source = resolved.source,
                            .type = tp,
                        } }, t));
                    },
                    else => {},
                }
                unreachable;
            },
            .selector => |sel| {
                const resolved = sel.resolves_to.?;
                switch (resolved.data) {
                    .type => |tp| return store.internInfoStable(.init(.{ .user = .{
                        .name = resolved.name,
                        .source = resolved.source,
                        .type = tp,
                    } }, t)),
                    else => {},
                }
                unreachable;
            },
            .dirty => unreachable,
        }
    }

    const InternResult = struct {
        id: TypeRef,
        info_ptr: *Info,
        inserted: bool,
        store: *Store,

        fn freeze(res: InternResult, info: Info) void {
            res.info_ptr.* = info;
            res.store.storage.items[@intFromEnum(res.id)] = info;
        }
    };

    fn internInfo(store: *Store, info: Info) InternResult {
        const result = store.mapping.getOrPutContext(store.ctx.allocator, info, .{ .store = store }) catch @panic("OOM");
        if (result.found_existing) {
            return .{
                .id = result.value_ptr.*,
                .info_ptr = result.key_ptr,
                .inserted = false,
                .store = store,
            };
        }
        result.value_ptr.* = @enumFromInt(store.storage.items.len);
        store.storage.ensureTotalCapacity(store.ctx.allocator, store.storage.items.len + 1) catch @panic("OOM");
        store.storage.items.len += 1;
        return .{
            .id = result.value_ptr.*,
            .info_ptr = result.key_ptr,
            .inserted = true,
            .store = store,
        };
    }

    pub fn internInfoStable(store: *Store, info: Info) TypeRef {
        const res = store.internInfo(info);
        if (res.inserted) {
            res.freeze(info);
        }
        return res.id;
    }

    pub fn get(store: *const Store, id: TypeRef) Info {
        return store.storage.items[@intFromEnum(id)];
    }

    pub fn update(store: *Store, id: TypeRef, info: Info) void {
        store.storage.items[@intFromEnum(id)] = info;
    }

    pub fn getBaseRef(store: *const Store, ref_: TypeRef) TypeRef {
        var ref = ref_;
        again: switch (store.get(ref).data) {
            .user => |tp| {
                ref = tp.type.id;
                continue :again store.get(ref).data;
            },
            .builtin => |prim| {
                ref = prim;
                continue :again store.get(prim).data;
            },
            else => {},
        }
        return ref;
    }

    pub fn deinit(store: *Store) void {
        store.storage.deinit(store.ctx.allocator);
        store.mapping.deinit(store.ctx.allocator);
        store.arena.deinit();
    }
};

pub const Info = struct {
    data: Data = .primitive,
    flags: std.EnumSet(Flag) = .initEmpty(),

    pub fn init(data: Data, t: *const node.Type) Info {
        var info: Info = .{};
        if (t.isLinear()) {
            info.flags.insert(.linear);
        }
        if (t.isWeak()) {
            info.flags.insert(.weak);
        }
        info.data = data;
        return info;
    }

    pub fn fromData(data: Data) Info {
        return .{ .data = data };
    }

    pub fn isLinear(info: Info) bool {
        return info.flags.contains(.linear) and !info.isWeak();
    }

    pub fn isWeak(info: Info) bool {
        return info.flags.contains(.weak);
    }

    pub const Context = struct {
        store: *const Store,

        pub fn hash(_: Context, info: Info) u64 {
            var hasher = std.hash.Wyhash.init(0);
            var it = info.flags.iterator();
            while (it.next()) |fl| {
                hasher.update(mem.asBytes(&fl));
            }
            switch (info.data) {
                .user => |u| {
                    hasher.update(mem.asBytes(&u.type.id));
                },
                .fun => |fun| {
                    hasher.update(mem.asBytes(&fun.signature.ref));
                    for (fun.bindings) |binding| {
                        hasher.update(mem.asBytes(&binding.head.position));
                        hasher.update(binding.text());
                    }
                },
                .fun_sig => |sig| {
                    for (sig.params) |param| {
                        hasher.update(mem.asBytes(&param.type));
                        hasher.update(mem.asBytes(&param.unpack));
                    }
                    hasher.update(mem.asBytes(&sig.return_type));
                },
                .sum => |sum| {
                    for (sum.fields) |field| {
                        hasher.update(mem.asBytes(&field.type));
                    }
                    hasher.update(mem.asBytes(&Data.sum));
                },
                .tuple => |tuple| {
                    for (tuple.types) |typ| {
                        hasher.update(mem.asBytes(&typ));
                    }
                    hasher.update(mem.asBytes(&Data.tuple));
                },
                .@"struct" => |st| {
                    for (st.fields) |f| {
                        hasher.update(f.name);
                        hasher.update(mem.asBytes(&f.type));
                    }
                },
                .@"enum" => |en| {
                    for (en.enumerators) |enumerator| {
                        hasher.update(enumerator);
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

        pub fn eql(_: Context, ai: Info, bi: Info) bool {
            if (!ai.flags.eql(bi.flags)) {
                return false;
            }
            const a, const b = .{ ai.data, bi.data };
            if (std.meta.activeTag(a) != std.meta.activeTag(b)) {
                return false;
            }
            switch (a) {
                .user => |u| return u.type.id == b.user.type.id,
                .fun_sig => |sig| {
                    if (sig.params.len != b.fun_sig.params.len) {
                        return false;
                    }
                    for (sig.params, b.fun_sig.params) |pa, pb| {
                        if (pa.type != pb.type) {
                            return false;
                        }
                        if (pa.unpack != pb.unpack) {
                            return false;
                        }
                    }
                    return sig.return_type == b.fun_sig.return_type;
                },
                .fun => |fun| {
                    if (fun.signature.ref != b.fun.signature.ref) {
                        return false;
                    }
                    if (fun.bindings.len != b.fun.bindings.len) {
                        return false;
                    }
                    for (fun.bindings, 0..) |binding, i| {
                        const binding_b = b.fun.bindings[i];
                        if (!std.meta.eql(binding, binding_b)) {
                            return false;
                        }
                    }
                    return true;
                },
                .sum => |sum| {
                    if (sum.fields.len != b.sum.fields.len) {
                        return false;
                    }
                    for (sum.fields, b.sum.fields) |fa, fb| {
                        if (fa.type != fb.type) {
                            return false;
                        }
                    }
                    return true;
                },
                .tuple => |tuple| {
                    if (tuple.types.len != b.tuple.types.len) {
                        return false;
                    }
                    return mem.eql(TypeRef, tuple.types, b.tuple.types);
                },
                .@"struct" => |st| {
                    if (st.fields.len != b.@"struct".fields.len) {
                        return false;
                    }
                    for (st.fields, b.@"struct".fields) |fa, fb| {
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

    pub fn formatWithStore(
        info: Info,
        store: *const Store,
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        if (info.isLinear()) {
            try writer.writeAll("linear ");
        }
        if (info.isWeak()) {
            try writer.writeAll("linear weak ");
        }
        switch (info.data) {
            .user => |user| {
                try writer.writeAll(user.name);
            },
            .fun => |fun| {
                const sig = fun.signature.get(store.*);
                try writer.writeAll("fun (");
                for (sig.params, 0..) |param, index| {
                    if (index != 0) {
                        try writer.writeAll(", ");
                    }
                    if (param.unpack) {
                        try writer.writeAll("..");
                    }
                    try writer.print("{s}: {f}", .{
                        fun.bindings[index].text(),
                        formatView(store, param.type),
                    });
                }
                try writer.print(") -> {f}", .{
                    formatView(store, sig.return_type),
                });
            },
            .fun_sig => |sig| {
                try writer.writeAll("fun (");
                for (sig.params, 0..) |param, index| {
                    if (index != 0) {
                        try writer.writeAll(", ");
                    }
                    if (param.unpack) {
                        try writer.writeAll("..");
                    }
                    try formatView(store, param.type).format(writer);
                }
                try writer.print(") -> {f}", .{
                    formatView(store, sig.return_type),
                });
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
                try writer.print("{f}", .{
                    formatView(store, child),
                });
            },
            .sum => |sum| {
                try writer.writeByte('(');
                for (sum.fields, 0..) |field, index| {
                    if (index != 0) {
                        try writer.writeAll("| ");
                    }
                    try formatView(store, field.type).format(writer);
                }
                try writer.writeByte(')');
            },
            .tuple => |tup| {
                try writer.writeByte('(');
                for (tup.types, 0..) |typ, index| {
                    if (index != 0) {
                        try writer.writeAll(", ");
                    }
                    try formatView(store, typ).format(writer);
                }
                try writer.writeByte(')');
            },
            .@"enum" => |en| {
                try writer.writeAll("enum {");
                for (en.enumerators, 0..) |enumerator, index| {
                    if (index != 0) {
                        try writer.writeAll(", ");
                    }
                    try writer.writeAll(enumerator);
                }
                try writer.writeByte('}');
            },
            .@"struct" => |st| {
                try writer.writeAll("struct {");
                for (st.fields, 0..) |field, index| {
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
            .builtin => |prim_t| try formatView(store, prim_t).format(writer),
            .primitive => try writer.writeAll("primitive type"),
        }
    }

    pub const Flag = enum {
        linear,
        weak,
    };
};

pub const Data = union(enum) {
    user: User,
    fun: Fun,
    fun_sig: Fun.Signature,
    ptr: TypeRef,
    slice: TypeRef,
    err: TypeRef,
    type_of: TypeRef,
    sum: SumType,
    tuple: TupleType,
    @"enum": EnumType,
    @"struct": StructType,
    builtin: TypeRef, // for flagged instances of primitive types
    primitive,

    pub fn isBuiltinCallable(d: Data, store: Store) bool {
        if (d == .type_of and store.getBaseRef(d.type_of).isBuiltin()) {
            return true;
        }
        return false;
    }

    pub fn getUnderlyingType(d: Data, store: Store) Data {
        return again: switch (d) {
            .user => |u| if (u.type.underlying_type) |und| {
                continue :again store.get(und.typeRef()).data;
            } else d,
            else => |x| x,
        };
    }

    pub const TupleType = struct {
        types: []TypeRef,
        pub fn init(types: []TypeRef) TupleType {
            return .{ .types = types };
        }
    };

    pub const EnumType = struct {
        enumerators: [][]const u8,
        pub fn init(enumerators: [][]const u8) EnumType {
            return .{ .enumerators = enumerators };
        }
    };

    pub const SumType = struct {
        fields: []SumField,
        pub fn init(fields: []SumField) SumType {
            return .{ .fields = fields };
        }
    };

    pub const StructType = struct {
        fields: []StructField,

        pub fn init(fields: []StructField) StructType {
            return .{
                .fields = fields,
            };
        }

        pub fn get(st: StructType, field: []const u8) ?StructField {
            for (st.fields) |f| {
                if (std.mem.eql(u8, f.name, field)) {
                    return f;
                }
            }
            return null;
        }
    };
};

pub const User = struct {
    name: []const u8,
    source: usize,
    type: *node.Symbol.Type,
};

pub const Fun = struct {
    signature: TypeRefStrict(Signature),
    bindings: []node.Ident, // param index -> name

    pub const Signature = struct {
        params: []const Param = &.{},
        return_type: TypeRef = .unset,

        // Needs to be inline otherwise I get valgrind errors :O
        pub inline fn initCast(cast_to: TypeRef) Signature {
            return .{
                .params = &.{
                    .{
                        .type = cast_to,
                        .unpack = false,
                    },
                },
                .return_type = cast_to,
            };
        }
    };

    pub const Param = struct {
        type: TypeRef = .unset,
        unpack: bool = false,
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

pub fn TypeRefStrict(comptime T: type) type {
    return struct {
        ref: TypeRef,
        pub fn get(r: @This(), store: Store) T {
            inline for (std.meta.fields(Data)) |f| {
                if (f.type == T) {
                    return @field(store.get(r.ref).data, f.name);
                }
            }
            unreachable;
        }
    };
}
