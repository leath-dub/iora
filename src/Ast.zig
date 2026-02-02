const std = @import("std");
const util = @import("util.zig");
const Lexer = @import("Lexer.zig");
const Token = Lexer.Token;
const Code = @import("Code.zig");
const node = @import("node.zig");
const GeneralContext = @import("GeneralContext.zig");
const meta = std.meta;
const mem = std.mem;

const Ast = @This();

ctx: *GeneralContext,
lexer: *Lexer,
side_tables: SideTables = .{},
storage: std.ArrayList(NodeEntry) = .{},

pub fn init(lexer: *Lexer) Ast {
    return .{
        .ctx = lexer.ctx,
        .lexer = lexer,
    };
}

pub fn deinit(ast: *Ast) void {
    ast.side_tables.deinit(ast.ctx.allocator);
    ast.storage.deinit(ast.ctx.allocator);
}

pub fn get(ast: Ast, comptime attr: SideTables.Attachment, of: node.Handle) SideTables.Data(attr) {
    const T = @FieldType(SideTables, @tagName(attr));
    if (comptime util.isArrayList(T)) {
        return @field(ast.side_tables, @tagName(attr)).items[of];
    }
    return @field(ast.side_tables, @tagName(attr)).get(of);
}

pub fn set(ast: *Ast, comptime attr: SideTables.Attachment, of: node.Handle, value: util.Unwrap(SideTables.Data(attr))) !void {
    const T = @FieldType(SideTables, @tagName(attr));
    if (comptime util.isArrayList(T)) {
        try @field(ast.side_tables, @tagName(attr)).insert(ast.ctx.allocator, of, value);
    } else {
        try @field(ast.side_tables, @tagName(attr)).put(of, ast.ctx.allocator, value);
    }
}

// NOTE: the expectation is that these functions are called in pre-order while
// parsing. This allows for walking the tree in both post and pre order without
// needing some auxillery structure - only need to keep track of how many children
// each node has.
pub fn startNode(ast: *Ast, value: anytype) !node.Ref(@TypeOf(value)) {
    const handle: node.Handle = @intCast(ast.storage.items.len);
    try ast.storage.append(ast.ctx.allocator, .{ .node = @unionInit(Node, @tagName(TypeTag(@TypeOf(value))), value), .skip = 0 });
    return .{ .handle = handle };
}

pub fn endNode(ast: *Ast, handle: node.Handle) void {
    ast.storage.items[handle].skip = @intCast(ast.storage.items.len - handle);
}

// pub fn add(ast: *Ast, value: anytype, child_count: u16, data: SideTables.Required) !node.Ref(@TypeOf(value)) {
//     // Set any required data attachments (non hash map data attachments)
//     inline for (comptime meta.tags(meta.FieldEnum(SideTables.Required))) |required_tag| {
//         try ast.set(required_tag, handle, @field(data, @tagName(required_tag)));
//     }
//     return .{ .handle = handle };
// }

pub const PreOrderWalker = struct {
    enter: fn (*PreOrderWalker, Node) void,
};

pub const PostOrderWalker = struct {
    exit: fn (*PostOrderWalker, Node) void,
};

pub fn walkPreOrder(ast: Ast, walker: *PreOrderWalker) void {
    for (ast.storage.items) |n| {
        walker.enter(walker, n.node);
    }
}

pub fn walkPostOrder(ast: Ast, walker: *PostOrderWalker) !void {
    var pending = std.ArrayList(node.Handle).initCapacity(ast.ctx.allocator, 256);
    defer pending.deinit(ast.ctx.allocator);

    const nodes = ast.storage.items;

    var i: usize = 0;
    while (i < nodes.len or pending.items.len > 0) {
        if (i < nodes.len) {
            try pending.append(ast.ctx.allocator, i);
            i += 1;
        }

        while (pending.items.len > 0) {
            const top = pending.items[pending.items.len - 1];
            if (i < top + nodes[top].skip) {
                // Subtree not exausted as 'i' is still less than skip of
                // the pending node
                break;
            }
            walker.exit(walker, nodes[pending.pop().?].node);
        }
    }
}

pub fn walk(ast: Ast, pre: *PreOrderWalker, post: *PostOrderWalker) !void {
    var pending = std.ArrayList(node.Handle).initCapacity(ast.ctx.allocator, 256);
    defer pending.deinit(ast.ctx.allocator);

    const nodes = ast.storage.items;

    var i: usize = 0;
    while (i < nodes.len or pending.items.len > 0) {
        if (i < nodes.len) {
            pre.enter(pre, nodes[i].node);
            try pending.append(ast.ctx.allocator, i);
            i += 1;
        }

        while (pending.items.len > 0) {
            const top = pending.items[pending.items.len - 1];
            if (i < top + nodes[top].skip) {
                // Subtree not exausted as 'i' is still less than skip of
                // the pending node
                break;
            }
            post.exit(post, nodes[pending.pop().?].node);
        }
    }
}

pub const SideTables = struct {
    pub const Attachment = meta.FieldEnum(SideTables);

    position: std.ArrayList(Code.Offset) = .{},

    pub fn Data(comptime attr: meta.FieldEnum(@This())) type {
        const T = @FieldType(@This(), @tagName(attr));
        if (util.isArrayList(T)) {
            return @typeInfo(T.Slice).pointer.child;
        }
        return ?@FieldType(T.KV, "value");
    }

    pub const Required = required: {
        const tags = meta.tags(Attachment);
        var fields: [tags.len]std.builtin.Type.StructField = undefined;
        var write: usize = 0;

        for (tags) |tag| {
            if (@typeInfo(Data(tag)) != .optional) {
                fields[write] = .{
                    .name = @tagName(tag),
                    .type = Data(tag),
                    .default_value_ptr = &mem.zeroes(Data(tag)),
                    .is_comptime = false,
                    .alignment = @alignOf(Data(tag)),
                };
                write += 1;
            }
        }

        break :required @Type(.{
            .@"struct" = .{
                .layout = .auto,
                .fields = fields[0..write],
                .decls = &.{},
                .is_tuple = false,
            },
        });
    };

    pub fn deinit(st: *@This(), allocator: mem.Allocator) void {
        inline for (comptime meta.fieldNames(@This())) |field| {
            @field(st, field).deinit(allocator);
        }
    }

    comptime {
        // Make sure all the fields are either ArrayList or AutoHashMapUnmanaged
        for (meta.fields(@This())) |field| {
            if (util.isAutoHashMapUnmanaged(field.type)) {
                const K, _ = util.AutoHashMapUnmanagedKVTuple(field.type);
                if (K != node.Handle) {
                    @compileError("all side table hash maps must be keyed by 'NodeRef'");
                }
                continue;
            }
            if (!util.isArrayList(field.type)) {
                @compileError("all side tables should be either 'std.ArrayList' or 'std.AutoHashMapUnmanaged'");
            }
        }
    }
};

fn comptimeSnakeCase(comptime text: []const u8) meta.Tuple(&.{ [text.len * 2 + 1]u8, usize }) {
    var buf = mem.zeroes([text.len * 2 + 1]u8);
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

// Automatically construct sum type from all the AST nodes defined in node.zig
const Node = blk: {
    var fields: [meta.declarations(node).len]std.builtin.Type.UnionField = undefined;

    var node_count: usize = 0;
    for (meta.declarations(node)) |decl| {
        const T = @field(node, decl.name);
        if (@TypeOf(T) == type and @typeInfo(T) == .@"struct") {
            // E.g. IfStmt -. if_stmts field
            const data, const len = comptimeSnakeCase(decl.name);
            fields[node_count] = .{
                .name = @ptrCast(data[0..len]),
                .type = T,
                .alignment = @alignOf(T),
            };
            node_count += 1;
        }
    }
    if (node_count != node.ast_node_count) {
        // If you are hitting this, make sure to either increment the `node.ast_node_count`
        // variable or do not specify public (struct) types in `node.zig` that are not ast nodes
        const msg = std.fmt.comptimePrint(
            "mismatch between reported node count {d} and actual node count {d}",
            .{ node.ast_node_count, node_count },
        );
        @compileError(msg);
    }

    break :blk @Type(.{
        .@"union" = .{
            .layout = .auto,
            .tag_type = null,
            .fields = fields[0..node_count],
            .decls = &.{},
        },
    });
};

const NodeEntry = struct {
    node: Node,
    // This stores how many nodes you need to skip to get the sibling of this
    // node
    skip: u16,
};

const NodeTag = meta.FieldEnum(Node);

fn TagType(comptime tag: NodeTag) type {
    return @FieldType(Node, @tagName(tag));
}

fn TypeTag(comptime T: type) NodeTag {
    comptime {
        for (meta.fields(Node)) |nf| {
            if (T == nf.type) {
                return meta.stringToEnum(NodeTag, nf.name).?;
            }
        }
        @compileLog(T);
        @compileError("type is not a AST node");
    }
}
