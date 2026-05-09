const std = @import("std");
const util = @import("util.zig");
const Code = @import("Code.zig");
const node = @import("node.zig");
const GeneralContext = @import("GeneralContext.zig");
const Token = @import("Lexer.zig").Token;
const common = @import("common.zig");
const tyref = @import("type_ref.zig");
const TypeRef = tyref.TypeRef;

const meta = std.meta;
const mem = std.mem;

const Ast = @This();

const SymbolMap = union(enum) {
    scope: *node.Scope,
    label_scope: *node.LabelScope,
};

ctx: *GeneralContext,
root: ?node.SourceFile = null,
arena: std.heap.ArenaAllocator,

pub fn init(ctx: *GeneralContext) Ast {
    return .{
        .ctx = ctx,
        .arena = ctx.createLifetime(),
    };
}

pub fn deinit(ast: *Ast) void {
    const ScopeCleaner = struct {
        ctx: *GeneralContext,

        pub fn enter(sc: *@This(), item: Node) void {
            switch (item) {
                inline else => |value| {
                    const T = @TypeOf(value.*);
                    if (@hasField(T, "scope")) {
                        value.scope.deinit(sc.ctx.allocator);
                    }
                    if (@hasField(T, "label_scope")) {
                        value.label_scope.deinit(sc.ctx.allocator);
                    }
                },
            }
        }
    };

    var scope_cleaner: ScopeCleaner = .{ .ctx = ast.ctx };
    walk(&scope_cleaner, &ast.root.?);

    ast.arena.deinit();
}

pub fn num(ast: *Ast, index: usize) []const u8 {
    const indexName = common.indexName(&common.index_name_buf, index);
    return ast.arena.allocator().dupe(u8, indexName) catch @panic("OOM");
}

// Allocate value on the heap
pub fn box(ast: *Ast, value: anytype) *@TypeOf(value) {
    if (@typeInfo(@TypeOf(value)) == .pointer) {
        @compileError("Can only box value types");
    }
    const ptr = ast.arena
        .allocator()
        .create(@TypeOf(value)) catch @panic("OOM");
    ptr.* = value;
    return ptr;
}

pub fn own(ast: *Ast, comptime T: type, items: []T) []T {
    return ast.arena.allocator().dupe(T, items) catch @panic("OOM");
}

fn callback(comptime name: []const u8, comptime Item: type, comptime Listener: type, item: *Item, listener: *Listener) ChildDisposition {
    var result: ChildDisposition = .walk;
    const generic_callback_name = name;
    if (comptime isNode(Item) and meta.hasMethod(Listener, generic_callback_name)) {
        const Args = meta.ArgsTuple(@TypeOf(@field(Listener, generic_callback_name)));
        if (@FieldType(Args, "0") != *Listener or
            @FieldType(Args, "1") != Node)
        {
            @compileError("function named " ++ generic_callback_name ++ " has invalid argument types");
        }
        const ret = @field(Listener, generic_callback_name)(listener, @unionInit(Node, @tagName(TypeTag(Item)), item));
        if (comptime @TypeOf(ret) == ChildDisposition) {
            result = ret;
        }
    }
    const callback_name = name ++ comptime common.unqualTypeName(Item);
    if (comptime meta.hasMethod(Listener, callback_name)) {
        const Args = meta.ArgsTuple(@TypeOf(@field(Listener, callback_name)));
        if (@FieldType(Args, "0") != *Listener or
            @FieldType(Args, "1") != *Item)
        {
            @compileError("function named " ++ callback_name ++ " has invalid argument types");
        }
        const ret = @field(Listener, callback_name)(listener, item);
        if (comptime @TypeOf(ret) == ChildDisposition) {
            result = ret;
        }
    }
    return result;
}

fn handleChild(ctx: anytype, ref: anytype, handle: anytype) void {
    const T = @typeInfo(@TypeOf(ref)).pointer.child;
    switch (@typeInfo(T)) {
        .@"union", .@"struct" => handle(ctx, ref),
        .optional => if (ref.*) |*child| {
            handleChild(ctx, child, handle);
        },
        .pointer => |ptr| switch (ptr.size) {
            .one => handleChild(ctx, ref.*, handle),
            .slice => for (ref.*) |*child| {
                handleChild(ctx, child, handle);
            },
            .many => @compileError("Ast cannot store many pointer (i.e. [*]T)"),
            .c => @compileError("Ast cannot store c pointer (i.e. [*c]T)"),
        },
        else => {}, // TODO: serialize other data with default formatters
    }
}

fn forEachChild(comptime Item: type, item: *Item, ctx: anytype, handle: anytype) void {
    if (@typeInfo(Item) != .@"struct" or @hasDecl(Item, "dont_walk")) {
        return;
    }
    inline for (meta.fields(Item)) |field| {
        const skip = (mem.eql(u8, field.name, "head") and field.type == node.Head) or
            field.name[0] == '_';
        if (!skip) {
            const field_ref = &@field(item, field.name);
            handleChild(ctx, field_ref, handle);
        }
    }
}

pub fn walk(listener: anytype, item: anytype) void {
    const Item = @typeInfo(@TypeOf(item)).pointer.child;
    const Listener = @typeInfo(@TypeOf(listener)).pointer.child;

    if (comptime isNode(Item) or isSumNode(Item)) {
        if (callback("enter", Item, Listener, item, listener) == .skip) {
            // Skip requested, be sure to call exit handlers
            _ = callback("exit", Item, Listener, item, listener);
            return;
        }
    }

    switch (@typeInfo(Item)) {
        .@"union" => switch (item.*) {
            inline else => |*n| walk(listener, n),
        },
        .@"struct" => forEachChild(Item, item, listener, walk),
        else => {},
    }

    if (comptime isNode(Item) or isSumNode(Item)) {
        _ = callback("exit", Item, Listener, item, listener);
    }
}

fn getChild(ref: anytype) ?Ast.Node {
    const N = @TypeOf(ref.*);
    if ((@typeInfo(N) == .@"struct" or @typeInfo(N) == .@"union") and @hasDecl(N, "dont_walk")) {
        return null;
    }
    return switch (@typeInfo(N)) {
        .@"struct" => @unionInit(Ast.Node, @tagName(TypeTag(N)), ref),
        .@"union" => switch (ref.*) {
            .dirty => null,
            inline else => |*alt| getChild(alt),
        },
        .pointer => |ptr| switch (ptr.size) {
            .one => getChild(ref.*),
            .slice => if (ref.len > 0) getChild(&ref.*[ref.len - 1]) else null,
            else => null,
        },
        .optional => if (ref.*) |*val| getChild(val) else null,
        else => null,
    };
}

pub fn lastChild(item: anytype) ?Ast.Node {
    var result: ?Ast.Node = null;
    inline for (meta.fields(@TypeOf(item.*))) |field| {
        const skip = comptime mem.eql(u8, field.name, "head") and field.type == node.Head;
        if (!skip) {
            if (getChild(&@field(item, field.name))) |n| {
                result = n;
            }
        }
    }
    return result;
}

pub fn isNode(comptime T: type) bool {
    return @typeInfo(T) == .@"struct" and @hasField(T, "head") and @FieldType(T, "head") == node.Head;
}

pub fn isSumNode(comptime T: type) bool {
    if (@typeInfo(T) != .@"union") return false;
    var has_non_node = false;
    inline for (std.meta.fields(T)) |alt| {
        has_non_node = !isNode(alt.type);
    }
    return has_non_node;
}

pub const NodeTag = std.meta.FieldEnum(Node);

pub fn TagType(comptime tag: NodeTag) type {
    return @FieldType(Node, @tagName(tag));
}

pub fn TypeTag(comptime T: type) NodeTag {
    comptime {
        for (std.meta.fields(Node)) |nf| {
            if (*T == nf.type) {
                return std.meta.stringToEnum(NodeTag, nf.name).?;
            }
        }
        @compileLog(T);
        @compileError("type is not a AST node");
    }
}

fn comptimeSnakeCase(comptime text: []const u8) std.meta.Tuple(&.{ [text.len * 2 + 1]u8, usize }) {
    var buf = std.mem.zeroes([text.len * 2 + 1]u8);
    var write: usize = 0;
    for (text) |ch_| {
        var ch = ch_;
        if (std.ascii.isUpper(ch)) {
            if (write != 0) {
                buf[write] = '_';
                write += 1;
            }
            ch = std.ascii.toLower(ch);
        }
        buf[write] = ch;
        write += 1;
    }
    return .{ buf, write };
}

fn comptimeCamelCase(comptime text: []const u8) std.meta.Tuple(&.{ [text.len]u8, usize }) {
    var buf = std.mem.zeroes([text.len]u8);
    var write: usize = 0;
    var upper = true;
    for (text) |ch_| {
        var ch = ch_;
        if (ch == '_') {
            upper = true;
            continue;
        }
        if (upper) {
            ch = std.ascii.toUpper(ch);
            upper = false;
        }
        buf[write] = ch;
        write += 1;
    }
    return .{ buf, write };
}

pub const Node = blk: {
    const decls = std.meta.declarations(node);
    var tags: [decls.len][]const u8 = undefined;
    var types: [decls.len]type = undefined;
    var attrs: [decls.len]std.builtin.Type.UnionField.Attributes = undefined;
    var values: [decls.len]u32 = undefined;
    var count: usize = 0;

    for (std.meta.declarations(node)) |decl| {
        const T = @field(node, decl.name);
        if (@TypeOf(T) == type and @typeInfo(T) == .@"struct" and @hasField(T, "head") and @FieldType(T, "head") == node.Head) {
            @setEvalBranchQuota(2000);
            const data, const len = comptimeSnakeCase(decl.name);
            tags[count] = @ptrCast(data[0..len]);
            values[count] = count;
            types[count] = *T;
            attrs[count] = .{ .@"align" = @alignOf(*T) };
            count += 1;
        }
    }

    break :blk @Union(
        .auto,
        @Enum(u32, .exhaustive, tags[0..count], values[0..count]),
        tags[0..count],
        types[0..count],
        attrs[0..count],
    );
};

pub const ChildDisposition = enum {
    walk,
    skip,
};

pub fn getNodeHead(item: Ast.Node) *node.Head {
    return switch (item) {
        inline else => |data| &data.head,
    };
}

pub const Dumper = struct {
    ctx: *GeneralContext,
    is_last: std.DynamicBitSetUnmanaged,
    writer: *std.Io.Writer,
    err: ?std.Io.Writer.Error = null,

    const done_child_str = "   ";
    const more_child_str = "│  ";
    const some_child_str = "├─ ";
    const last_child_str = "└─ ";

    pub fn init(ctx: *GeneralContext, writer: *std.Io.Writer) Dumper {
        return .{
            .ctx = ctx,
            .is_last = std.DynamicBitSetUnmanaged.initEmpty(ctx.allocator, 0) catch @panic("OOM"),
            .writer = writer,
        };
    }

    pub fn deinit(d: *Dumper) void {
        d.is_last.deinit(d.ctx.allocator);
    }

    fn bind(d: *Dumper, res: anytype) void {
        res catch |e| if (d.err == null) {
            d.err = e;
        };
    }

    fn emitAuxData(d: *Dumper, comptime name: []const u8, comptime fmt: []const u8, data: anytype, count: u32) void {
        d.bind(d.writer.writeAll(if (count == 0) "(" else ", "));
        d.bind(d.writer.print("{s}: " ++ fmt, .{ name, data }));
    }

    fn handleAuxData(d: *Dumper, comptime name: []const u8, comptime T: type, data_ref: anytype, count: u32) bool {
        const skip = comptime mem.eql(u8, name, "head") and T == node.Head;
        if (skip or getChild(data_ref) != null) {
            return false;
        }
        // Ship .unset type references
        if (T == TypeRef) {
            if (data_ref.* == .unset) {
                return false;
            }
        }
        switch (@typeInfo(T)) {
            .@"union", .@"struct" => if (meta.hasMethod(T, "format")) {
                d.emitAuxData(name, "{f}", data_ref.*, count);
                return true;
            },
            .optional => if (data_ref.*) |*data| {
                return d.handleAuxData(name, @TypeOf(data.*), data, count);
            },
            .bool => {
                d.emitAuxData(name, "{any}", data_ref.*, count);
                return true;
            },
            .int, .float => {
                d.emitAuxData(name, "{d}", data_ref.*, count);
                return true;
            },
            .@"enum" => {
                if (meta.hasMethod(T, "format")) {
                    d.emitAuxData(name, "{f}", data_ref.*, count);
                    return true;
                }
                d.emitAuxData(name, "{t}", data_ref.*, count);
                return true;
            },
            .pointer => |ptr| switch (ptr.size) {
                .one => return d.handleAuxData(name, @TypeOf(data_ref.*.*), data_ref, count),
                else => {},
            },
            // TODO: .array => ???,
            // TODO: .@"union" => ???,
            else => {},
        }
        return false;
    }

    fn emit(d: *Dumper, item: Ast.Node, dirty: bool) void {
        switch (std.meta.activeTag(item)) {
            inline else => |tag| {
                const data, const len = comptimeCamelCase(@tagName(tag));
                // Print as error node if it has an error
                if (dirty) {
                    d.bind(d.writer.print("Error[{s}]\n", .{data[0..len]}));
                    return;
                }
                d.bind(d.writer.writeAll(data[0..len]));

                var count: u32 = 0;
                const item_ref = @field(item, @tagName(tag));

                // Dump any none node fields as extra data
                inline for (meta.fields(@TypeOf(item_ref.*))) |field| {
                    if (d.handleAuxData(field.name, field.type, &@field(item_ref, field.name), count)) {
                        count += 1;
                    }
                }

                if (count != 0) {
                    d.bind(d.writer.writeByte(')'));
                }

                d.bind(d.writer.writeByte('\n'));
            },
        }
    }

    fn pushIsLast(d: *Dumper, is_last: bool) void {
        d.is_last.resize(d.ctx.allocator, d.is_last.bit_length + 1, is_last) catch
            @panic("OOM");
    }

    fn popIsLast(d: *Dumper) void {
        d.is_last.resize(d.ctx.allocator, d.is_last.bit_length - 1, false) catch unreachable;
    }

    pub fn enter(d: *Dumper, item: Ast.Node) ChildDisposition {
        const hd = Ast.getNodeHead(item);
        const is_dirty = hd.flags.contains(.dirty);
        const is_last = hd.flags.contains(.last_child);
        if (d.is_last.bit_length == 0) {
            d.emit(item, is_dirty);
            d.pushIsLast(is_last);
            return if (is_dirty) .skip else .walk;
        }

        for (1..d.is_last.bit_length) |i| {
            const prefix = if (d.is_last.isSet(i)) done_child_str else more_child_str;
            d.bind(d.writer.writeAll(prefix));
        }

        const prefix = if (is_last) last_child_str else some_child_str;
        d.bind(d.writer.writeAll(prefix));

        d.emit(item, is_dirty);
        d.pushIsLast(is_last);
        return if (is_dirty) .skip else .walk;
    }

    pub fn exit(d: *Dumper, _: Ast.Node) void {
        d.popIsLast();
        if (d.is_last.bit_length == 0) {
            // This means we are exiting the root node, flush the output
            // buffer.
            d.bind(d.writer.flush());
        }
    }
};

pub fn format(_ast: Ast, w: *std.Io.Writer) std.Io.Writer.Error!void {
    var ast = _ast;
    var dumper = Ast.Dumper.init(ast.ctx, w);
    defer dumper.deinit();
    Ast.walk(&dumper, &ast.root.?);
    if (dumper.err) |e| {
        return e;
    }
}
