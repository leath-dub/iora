const std = @import("std");

const Io = std.Io;

const Token = @import("Lexer.zig").Token;
const Code = @import("Code.zig");
const common = @import("common.zig");
const TypeRef = @import("type_ref.zig").TypeRef;
const ir = @import("ir.zig");
const util = @import("util.zig");

// TODO:
// pub const Module = struct {
//     scope: Scope = .{},
//     source_files: []SourceFile = &.{},
// };

pub const SourceFile = struct {
    head: Head = .{},
    imports: []Import = &.{},
    decls: []Decl = &.{},
    scope: Scope = .{}, // Available after name resolution
};

pub const Decl = union(enum) {
    def: DefDecl,
    @"var": VarDecl,
    fun: FunDecl,
    type: TypeDecl,
    dirty,
};

// TODO: allow 'use core as c;'
pub const Import = struct {
    head: Head = .{},
    module: Token = .{},
};

pub const VarDecl = struct {
    head: Head = .{},
    declarator: Token = .{},
    name: Ident = .{},
    type: ?Type = null,
    init_expr: ?Expr = null,
    symbol: Symbol.Var = .{ .kind = .@"var" },

    pub const _S = util.Unwrap(@FieldType(@This(), "symbol"));

    pub fn x(n: *@This(), comptime field: std.meta.FieldEnum(_S)) *@FieldType(_S, @tagName(field)) {
        return &@field(n.symbol, @tagName(field));
    }

    pub fn xv(n: *const @This(), comptime field: std.meta.FieldEnum(_S)) @FieldType(_S, @tagName(field)) {
        return @field(n.symbol, @tagName(field));
    }
};

pub const DefDecl = struct {
    head: Head = .{},
    type_name: ?Ident = null,
    name: Ident = .{},
    type: ?Type = null,
    init_expr: Expr = .dirty,
    symbol: Symbol.Var = .{ .kind = .def },

    pub const _S = util.Unwrap(@FieldType(@This(), "symbol"));

    pub fn x(n: *@This(), comptime field: std.meta.FieldEnum(_S)) *@FieldType(_S, @tagName(field)) {
        return &@field(n.symbol, @tagName(field));
    }

    pub fn xv(n: *const @This(), comptime field: std.meta.FieldEnum(_S)) @FieldType(_S, @tagName(field)) {
        return @field(n.symbol, @tagName(field));
    }
};

const Linkage = enum {
    external,
    internal,
};

pub const FunDecl = struct {
    head: Head = .{},
    type_name: ?Ident = null,
    name: Ident = .{},
    params: []FunParam = &.{},
    linkage: Linkage = .external,
    return_type: ?Type = null,
    body: ?CompStmt = null,
    symbol: Symbol.Fun = .{},

    pub const _S = util.Unwrap(@FieldType(@This(), "symbol"));

    pub fn x(n: *@This(), comptime field: std.meta.FieldEnum(_S)) *@FieldType(_S, @tagName(field)) {
        return &@field(n.symbol, @tagName(field));
    }

    pub fn xv(n: *const @This(), comptime field: std.meta.FieldEnum(_S)) @FieldType(_S, @tagName(field)) {
        return @field(n.symbol, @tagName(field));
    }
};

pub const FunParam = struct {
    head: Head = .{},
    name: Ident = .{},
    type: Type = .dirty,
    default: ?Expr = null,
    unpack: bool = false,
    symbol: Symbol.Var = .{ .kind = .param },

    pub const _S = util.Unwrap(@FieldType(@This(), "symbol"));

    pub fn x(n: *@This(), comptime field: std.meta.FieldEnum(_S)) *@FieldType(_S, @tagName(field)) {
        return &@field(n.symbol, @tagName(field));
    }

    pub fn xv(n: *const @This(), comptime field: std.meta.FieldEnum(_S)) @FieldType(_S, @tagName(field)) {
        return @field(n.symbol, @tagName(field));
    }
};

pub const TypeDecl = struct {
    head: Head = .{},
    name: Ident = .{},
    type: Type = .dirty,
    symbol: Symbol.Type = .{},

    pub const _S = util.Unwrap(@FieldType(@This(), "symbol"));

    pub fn x(n: *@This(), comptime field: std.meta.FieldEnum(_S)) *@FieldType(_S, @tagName(field)) {
        return &@field(n.symbol, @tagName(field));
    }

    pub fn xv(n: *const @This(), comptime field: std.meta.FieldEnum(_S)) @FieldType(_S, @tagName(field)) {
        return @field(n.symbol, @tagName(field));
    }
};

pub const Type = union(enum) {
    builtin: BuiltinType,
    coll: CollType,
    tuple: TupleType,
    @"struct": StructType,
    sum: SumType,
    @"enum": EnumType,
    ptr: PtrType,
    err: ErrType,
    fun: FunType,
    ident: IdentType,
    selector: SelectorType,
    type_of: TypeOfType,
    dirty,

    pub fn head(ty: *Type) *Head {
        return switch (ty.*) {
            .dirty => unreachable,
            inline else => |*foo| &foo.head,
        };
    }

    pub fn headConst(ty: *const Type) *const Head {
        return switch (ty.*) {
            .dirty => unreachable,
            inline else => |*foo| &foo.head,
        };
    }

    pub fn isLinear(ty: Type) bool {
        return ty.headConst().flags.contains(.linear);
    }

    pub fn isWeak(ty: Type) bool {
        return ty.headConst().flags.contains(.weak);
    }

    pub fn at(ty: Type) Code.Offset {
        return ty.headConst().position;
    }

    pub fn symbol(ty: *Type) Symbol {
        return switch (ty.*) {
            .dirty => unreachable,
            inline else => |*conc| .fromNode(conc),
        };
    }
};

pub const IdentType = struct {
    head: Head = .{},
    name: Ident = .{},
    is_inferred: bool = false,
    resolves_to: ?Symbol = null,
    symbol: Symbol.Type = .{},
};

pub const IdentOrSelector = union(enum) {
    ident: IdentType,
    selector: SelectorType,
    dirty,
};

pub const SelectorType = struct {
    head: Head = .{},
    type: *IdentOrSelector = undefined,
    field: Ident = .{},
    resolves_to: ?Symbol = null,
    symbol: Symbol.Type = .{},
};

pub const BuiltinType = struct {
    head: Head = .{},
    token: Token = .{},
    symbol: Symbol.Type = .{},
};

pub const CollType = struct {
    head: Head = .{},
    index_expr: ?Expr = null,
    value_type: *Type = undefined,
    symbol: Symbol.Type = .{},
};

pub const TupleType = struct {
    head: Head = .{},
    types: []SubType = &.{},
    symbol: Symbol.Type = .{},
    callable: FunType = .{},

    pub const _S = util.Unwrap(@FieldType(@This(), "symbol"));

    pub fn x(n: *@This(), comptime field: std.meta.FieldEnum(_S)) *@FieldType(_S, @tagName(field)) {
        return &@field(n.symbol, @tagName(field));
    }

    pub fn xv(n: *const @This(), comptime field: std.meta.FieldEnum(_S)) @FieldType(_S, @tagName(field)) {
        return @field(n.symbol, @tagName(field));
    }
};

pub const SubType = struct {
    head: Head = .{},
    type: Type = .dirty,
    symbol: Symbol.Type = .{},

    pub const _S = util.Unwrap(@FieldType(@This(), "symbol"));

    pub fn x(n: *@This(), comptime field: std.meta.FieldEnum(_S)) *@FieldType(_S, @tagName(field)) {
        return &@field(n.symbol, @tagName(field));
    }

    pub fn xv(n: *const @This(), comptime field: std.meta.FieldEnum(_S)) @FieldType(_S, @tagName(field)) {
        return @field(n.symbol, @tagName(field));
    }
};

pub const SumType = struct {
    head: Head = .{},
    alts: []TypeOrInlineDecl = &.{},
    symbol: Symbol.Type = .{},

    pub const _S = util.Unwrap(@FieldType(@This(), "symbol"));

    pub fn x(n: *@This(), comptime field: std.meta.FieldEnum(_S)) *@FieldType(_S, @tagName(field)) {
        return &@field(n.symbol, @tagName(field));
    }

    pub fn xv(n: *const @This(), comptime field: std.meta.FieldEnum(_S)) @FieldType(_S, @tagName(field)) {
        return @field(n.symbol, @tagName(field));
    }
};

pub const TypeOrInlineDecl = union(enum) {
    type: Type,
    type_decl: TypeDecl,
    dirty,
};

pub const StructType = struct {
    head: Head = .{},
    fields: []StructField = &.{},
    symbol: Symbol.Type = .{},
    callable: FunType = .{},

    pub const _S = util.Unwrap(@FieldType(@This(), "symbol"));

    pub fn x(n: *@This(), comptime field: std.meta.FieldEnum(_S)) *@FieldType(_S, @tagName(field)) {
        return &@field(n.symbol, @tagName(field));
    }

    pub fn xv(n: *const @This(), comptime field: std.meta.FieldEnum(_S)) @FieldType(_S, @tagName(field)) {
        return @field(n.symbol, @tagName(field));
    }
};

pub const StructField = struct {
    head: Head = .{},
    name: Ident = .{},
    type: Type = .dirty,
    default: ?Expr = null,
    symbol: Symbol.Field = .{},

    pub const _S = util.Unwrap(@FieldType(@This(), "symbol"));

    pub fn x(n: *@This(), comptime field: std.meta.FieldEnum(_S)) *@FieldType(_S, @tagName(field)) {
        return &@field(n.symbol, @tagName(field));
    }

    pub fn xv(n: *const @This(), comptime field: std.meta.FieldEnum(_S)) @FieldType(_S, @tagName(field)) {
        return @field(n.symbol, @tagName(field));
    }
};

pub const EnumType = struct {
    head: Head = .{},
    alts: []Enumerator = &.{},
    symbol: Symbol.Type = .{},

    pub const _S = util.Unwrap(@FieldType(@This(), "symbol"));

    pub fn x(n: *@This(), comptime field: std.meta.FieldEnum(_S)) *@FieldType(_S, @tagName(field)) {
        return &@field(n.symbol, @tagName(field));
    }

    pub fn xv(n: *const @This(), comptime field: std.meta.FieldEnum(_S)) @FieldType(_S, @tagName(field)) {
        return @field(n.symbol, @tagName(field));
    }
};

pub const Enumerator = struct {
    head: Head = .{},
    name: Ident = .{},
    symbol: Symbol.Enumerator = .{},

    pub const _S = util.Unwrap(@FieldType(@This(), "symbol"));

    pub fn x(n: *@This(), comptime field: std.meta.FieldEnum(_S)) *@FieldType(_S, @tagName(field)) {
        return &@field(n.symbol, @tagName(field));
    }

    pub fn xv(n: *const @This(), comptime field: std.meta.FieldEnum(_S)) @FieldType(_S, @tagName(field)) {
        return @field(n.symbol, @tagName(field));
    }
};

pub const PtrType = struct {
    head: Head = .{},
    child: *Type = undefined,
    symbol: Symbol.Type = .{},
};

pub const ErrType = struct {
    head: Head = .{},
    child: *Type = undefined,
    symbol: Symbol.Type = .{},
};

pub const TypeOfType = struct {
    head: Head = .{},
    child: *Type = undefined,
    symbol: Symbol.Type = .{},
};

pub const FunType = struct {
    head: Head = .{},
    linkage: Linkage = .external,
    params: []FunParam = &.{},
    return_type: ?*Type = null,
    symbol: Symbol.Type = .{},

    pub const _S = util.Unwrap(@FieldType(@This(), "symbol"));

    pub fn x(n: *@This(), comptime field: std.meta.FieldEnum(_S)) *@FieldType(_S, @tagName(field)) {
        return &@field(n.symbol, @tagName(field));
    }

    pub fn xv(n: *const @This(), comptime field: std.meta.FieldEnum(_S)) @FieldType(_S, @tagName(field)) {
        return @field(n.symbol, @tagName(field));
    }
};

pub const Ident = struct {
    head: Head = .{},
    token: Token = .{},

    pub fn text(id: Ident) []const u8 {
        return id.token.span;
    }

    pub fn at(id: Ident) Code.Offset {
        return id.head.position;
    }
};

pub const Stmt = union(enum) {
    decl: Decl,
    @"if": IfStmt,
    @"while": WhileStmt,
    case: CaseStmt,
    @"return": ReturnStmt,
    @"defer": DeferStmt,
    comp: CompStmt,
    expr: Expr,
    assign: Assign,
    labelled: LabelledStmt,
    branch: BranchStmt,
    dirty,
};

pub const LabelledStmt = struct {
    head: Head = .{},
    name: Ident = .{},
    stmt: *Stmt = undefined,
};

pub const BranchStmt = struct {
    head: Head = .{},
    action: Token = .{},
    _label: ?*LabelledStmt = null,
    label_name: ?Ident = null,
};

pub const Assign = struct {
    head: Head = .{},
    lvalue: Expr = .dirty,
    rvalue: Expr = .dirty,
    type_ref: TypeRef = .unset,
};

pub const IfStmt = struct {
    head: Head = .{},
    cond: Cond = .dirty,
    then_arm: CompStmt = .{},
    else_arm: ?Else = null,
    scope: Scope = .{}, // used for SumTypeReduce
};

pub const Else = union(enum) {
    @"if": *IfStmt,
    comp: CompStmt,
    dirty,
};

pub const WhileStmt = struct {
    head: Head = .{},
    cond: Cond = .dirty,
    body: CompStmt = .{},
    scope: Scope = .{}, // used for SumTypeReduce
};

pub const Cond = union(enum) {
    sum_type_reduce: SumTypeReduce,
    expr: Expr,
    dirty,
};

pub const SumTypeReduce = struct {
    head: Head = .{},
    declarator: Token = .{},
    name: Ident = .{},
    reduction: Type = .dirty,
    value: Expr = .dirty,
    symbol: Symbol.Var = .{ .kind = .reduce },

    pub const _S = util.Unwrap(@FieldType(@This(), "symbol"));

    pub fn x(n: *@This(), comptime field: std.meta.FieldEnum(_S)) *@FieldType(_S, @tagName(field)) {
        return &@field(n.symbol, @tagName(field));
    }

    pub fn xv(n: *const @This(), comptime field: std.meta.FieldEnum(_S)) @FieldType(_S, @tagName(field)) {
        return @field(n.symbol, @tagName(field));
    }
};

pub const CaseStmt = struct {
    head: Head = .{},
    arg: Expr = .dirty,
    arms: []CaseArm = &.{},
};

pub const CaseArm = struct {
    head: Head = .{},
    patt: CasePatt = .dirty,
    action: Stmt = .dirty,
    scope: Scope = .{},
};

pub const CasePatt = union(enum) {
    type: Type,
    expr: Expr,
    binding: CaseBinding,
    default,
    dirty,
};

pub const CaseBinding = struct {
    head: Head = .{},
    declarator: Token = .{},
    name: Ident = .{},
    type: Type = .dirty,
    symbol: Symbol.Var = .{ .kind = .case },

    pub const _S = util.Unwrap(@FieldType(@This(), "symbol"));

    pub fn x(n: *@This(), comptime field: std.meta.FieldEnum(_S)) *@FieldType(_S, @tagName(field)) {
        return &@field(n.symbol, @tagName(field));
    }

    pub fn xv(n: *const @This(), comptime field: std.meta.FieldEnum(_S)) @FieldType(_S, @tagName(field)) {
        return @field(n.symbol, @tagName(field));
    }
};

pub const ReturnStmt = struct {
    head: Head = .{},
    child: Expr = .dirty,
};

pub const DeferStmt = struct {
    head: Head = .{},
    child: *Stmt = undefined,
};

pub const CompStmt = struct {
    head: Head = .{},
    stmts: []Stmt = &.{},
    scope: Scope = .{},
};

pub const Expr = union(enum) {
    postfix: PostfixExpr,
    call: CallExpr,
    coll_access: CollAccessExpr,
    ident_expr: IdentExpr,
    selector_expr: SelectorExpr,
    unary: UnaryExpr,
    bin: BinExpr,
    // Atomic expressions
    anon_call: AnonCallExpr,
    token_expr: TokenExpr,
    type_expr: TypeExpr,
    dirty,

    pub fn head(expr: *Expr) *Head {
        return switch (expr.*) {
            .dirty => unreachable,
            inline else => |*foo| &foo.head,
        };
    }

    pub fn headConst(expr: *const Expr) *const Head {
        return switch (expr.*) {
            .dirty => unreachable,
            inline else => |*foo| &foo.head,
        };
    }

    pub fn getType(expr: *Expr) *TypeRef {
        return switch (expr.*) {
            .dirty => unreachable,
            inline else => |*foo| &foo.type_ref,
        };
    }

    pub fn getTypeConst(expr: *const Expr) *const TypeRef {
        return switch (expr.*) {
            .dirty => &.unset,
            inline else => |foo| &foo.type_ref,
        };
    }

    pub fn register(expr: *Expr) *ir.Register {
        return switch (expr.*) {
            .dirty => unreachable,
            inline else => |*foo| &foo.register,
        };
    }

    pub fn registerConst(expr: *const Expr) *const ir.Register {
        return switch (expr.*) {
            .dirty => &.nil,
            inline else => |foo| &foo.register,
        };
    }

    pub fn at(expr: Expr) Code.Offset {
        return expr.headConst().position;
    }

    pub fn format(
        expr: Expr,
        writer: *Io.Writer,
    ) Io.Writer.Error!void {
        switch (expr) {
            .postfix => |e| try writer.print("{f}{s}", .{ e.operand.*, e.op.span }),
            .call => |e| {
                try writer.print("{f}(", .{e.callable.*});
                for (e.args, 0..) |a, i| {
                    if (i != 0) {
                        try writer.writeAll(", ");
                    }
                    try a.format(writer);
                }
                try writer.writeByte(')');
            },
            .coll_access => |e| try writer.print("{f}[{f}]", .{ e.lvalue.*, e.subscript.* }),
            .ident_expr => |e| {
                if (e.is_inferred) {
                    try writer.writeByte('.');
                }
                try writer.writeAll(e.name.text());
            },
            .selector_expr => |e| {
                try writer.print("{f}.{s}", .{ e.value.*, e.field.text() });
            },
            .unary => |e| try writer.print("{s}{f}", .{ e.op.span, e.operand.* }),
            .bin => |e| try writer.print("{f}{s}{f}", .{ e.left.*, e.op.span, e.right.* }),
            .anon_call => |e| {
                try writer.writeByte('(');
                for (e.args, 0..) |a, i| {
                    if (i != 0) {
                        try writer.writeAll(", ");
                    }
                    try a.format(writer);
                }
                try writer.writeByte(')');
            },
            .token_expr => |e| try writer.writeAll(e.token.span),
            .type_expr => try writer.writeAll("<type expr>"),
            .dirty => try writer.writeAll("<error>"),
        }
    }
};

pub const IdentExpr = struct {
    head: Head = .{},
    name: Ident = .{},
    is_inferred: bool = false,
    hint: ?Symbol = null,
    type_ref: TypeRef = .unset,
    resolves_to: ?Symbol = null,
    register: ir.Register = .nil,
};

pub const SelectorExpr = struct {
    head: Head = .{},
    value: *Expr = undefined,
    field: Ident = .{},
    type_ref: TypeRef = .unset,
    resolves_to: ?Symbol = null,
    register: ir.Register = .nil,
};

pub const TokenExpr = struct {
    head: Head = .{},
    token: Token = .{},
    type_ref: TypeRef = .unset,
    register: ir.Register = .nil,
};

pub const TypeExpr = struct {
    head: Head = .{},
    type: *Type = undefined,
    type_ref: TypeRef = .unset,
    register: ir.Register = .nil,
};

pub const PostfixExpr = struct {
    head: Head = .{},
    op: Token,
    operand: *Expr,
    type_ref: TypeRef = .unset,
    register: ir.Register = .nil,
};

pub const UnaryExpr = struct {
    head: Head = .{},
    op: Token,
    operand: *Expr,
    type_ref: TypeRef = .unset,
    register: ir.Register = .nil,
};

pub const BinExpr = struct {
    head: Head = .{},
    op: Token = .{},
    left: *Expr,
    right: *Expr,
    type_ref: TypeRef = .unset,
    register: ir.Register = .nil,
};

pub const CallBindings = struct {
    kind: Kind = .call,
    bindings: []ArgBinding,

    pub fn format(
        cb: CallBindings,
        w: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try w.writeByte('(');
        for (cb.bindings, 0..) |binding, i| {
            if (i != 0) {
                try w.writeAll(", ");
            }
            try w.print("{s} = ", .{binding.name});
            if (binding.expr) |ex| {
                try ex.format(w);
            } else {
                try w.writeAll("<unbound>");
            }
        }
        try w.writeByte(')');
    }

    pub const Kind = enum {
        call,
        cast,
    };

    pub const ArgBinding = struct {
        name: []const u8 = "",
        expr: ?*Expr = null,
    };

    pub const dont_walk = true;
};

pub const CallExpr = struct {
    head: Head = .{},
    callable: *Expr = undefined,
    args: []CallExprArg = &.{},
    type_ref: TypeRef = .unset,
    call_bindings: ?CallBindings = null,
    register: ir.Register = .nil,
};

pub const AnonCallExpr = struct {
    head: Head = .{},
    args: []CallExprArg = &.{},
    hint: ?Symbol = null,
    type_ref: TypeRef = .unset,
    call_bindings: ?CallBindings = null,
    register: ir.Register = .nil,
};

pub const CallExprArg = union(enum) {
    unpack: UnpackExpr,
    labelled: LabelledExpr,
    expr: Expr,
    dirty,

    pub fn at(arg: CallExprArg) Code.Offset {
        return switch (arg) {
            .labelled => |lab| lab.expr.headConst().position,
            .expr => |ex| ex.headConst().position,
            .unpack => |un| un.expr.headConst().position,
            .dirty => unreachable,
        };
    }

    pub fn format(
        arg: CallExprArg,
        writer: *Io.Writer,
    ) Io.Writer.Error!void {
        switch (arg) {
            .unpack => |ua| {
                try writer.print("{f}..", .{ua.expr});
            },
            .labelled => |la| {
                const un = if (la.unpack) ".." else "";
                try writer.print("{s}{s} = {f}", .{ un, la.label.text(), la.expr });
            },
            .expr => |ea| {
                try ea.format(writer);
            },
            .dirty => try writer.writeAll("<error>"),
        }
    }
};

pub const UnpackExpr = struct {
    head: Head = .{},
    expr: Expr = .dirty,
};

pub const LabelledExpr = struct {
    head: Head = .{},
    label: Ident = .{},
    unpack: bool = false,
    expr: Expr = .dirty,
};

pub const CollAccessExpr = struct {
    head: Head = .{},
    lvalue: *Expr,
    subscript: *CollSubscript,
    type_ref: TypeRef = .unset,
    register: ir.Register = .nil,
};

pub const CollSubscript = union(enum) {
    expr: Expr,
    range: SliceRange,
    dirty,

    pub fn format(
        sub: CollSubscript,
        writer: *Io.Writer,
    ) Io.Writer.Error!void {
        switch (sub) {
            .expr => |e| try e.format(writer),
            .range => |r| {
                if (r.begin) |b| {
                    try b.format(writer);
                }
                try writer.writeByte(':');
                if (r.end) |e| {
                    try e.format(writer);
                }
            },
            .dirty => try writer.writeAll("<error>"),
        }
    }
};

pub const SliceRange = struct {
    head: Head = .{},
    begin: ?Expr = null,
    end: ?Expr = null,
};

pub const Flag = enum {
    dirty,
    last_child,
    fake,
    linear,
    weak,
};

pub const Symbol = struct {
    name: []const u8 = "",
    data: Data,
    source: Code.Offset = 0,

    pub fn fromNode(n: anytype) Symbol {
        return .{
            .name = if (@hasField(@TypeOf(n.*), "name")) n.name.text() else "",
            .data = .fromAlt(&n.symbol),
            .source = n.head.position,
        };
    }

    pub fn varId(s: Symbol) ?ir.Var {
        if (s.data != .@"var") {
            return null;
        }
        return s.data.@"var".id;
    }

    pub fn scope(s: Symbol) ?*Scope {
        switch (s.data) {
            inline else => |data| {
                if (@hasField(@TypeOf(data.*), "scope")) {
                    return &data.scope;
                }
                return null;
            },
        }
    }

    pub fn typeRef(s: Symbol) TypeRef {
        return switch (s.data) {
            .type => |t| t.id,
            .enumerator => .unset,
            inline else => |x| x.type,
        };
    }

    pub const Data = union(enum) {
        @"var": *Var,
        fun: *Fun,
        type: *Symbol.Type,
        enumerator: *Symbol.Enumerator,
        field: *Field,

        pub fn fromAlt(n: anytype) Data {
            const field = switch (@TypeOf(n)) {
                *Var => "var",
                *Fun => "fun",
                *Symbol.Type => "type",
                *Symbol.Enumerator => "enumerator",
                *Field => "field",
                else => @compileError("invalid type: " ++ @typeName(@TypeOf(n))),
            };
            return @unionInit(Data, field, n);
        }
    };

    pub const Var = struct {
        id: ir.Var = .nil,
        type: TypeRef = .unset,
        hint: ?Symbol = null,
        kind: Kind,
        flags: std.EnumSet(Var.Flag) = .empty,
        pub const Flag = enum {
            unpack,
            is_const,
        };
        pub const Kind = enum {
            @"var",
            def,
            param,
            reduce,
            case,
        };
        pub const dont_walk = true;
    };

    pub const Fun = struct {
        unit: ir.FunUnit = .{},
        type: TypeRef = .unset,
        params: []Symbol = &.{},
        scope: Scope = .{},
        label_scope: LabelScope = .{},
        pub const dont_walk = true;
    };

    pub const Type = struct {
        id: TypeRef = .unset,
        scope: Scope = .{},
        underlying_type: ?Symbol = null,
        is_resolving: bool = false,
        pub const dont_walk = true;
    };

    pub const Enumerator = struct {
        value: u64 = 0,
        enclosed_by: Symbol = .{ .data = .{ .type = undefined } },
        pub const dont_walk = true;
    };

    pub const Field = struct {
        type: TypeRef = .unset,
        enclosed_by: *Symbol.Type = undefined,
        pub const dont_walk = true;
    };

    pub const dont_walk = true;
};

// TODO: add name to scope
pub const Scope = struct {
    parent: ?*Scope = null,
    entries: std.StringHashMapUnmanaged(Symbol) = .empty,

    pub fn insert(s: *Scope, allocator: std.mem.Allocator, symbol: Symbol) ?Symbol {
        const result = s.entries.getOrPut(allocator, symbol.name) catch @panic("OOM");
        if (result.found_existing) {
            return result.value_ptr.*;
        }
        result.value_ptr.* = symbol;
        return null;
    }

    pub inline fn get(s: Scope, name: []const u8) ?Symbol {
        return s.entries.get(name);
    }

    pub fn deinit(s: *Scope, allocator: std.mem.Allocator) void {
        s.entries.deinit(allocator);
    }

    pub fn format(s: Scope, w: *std.Io.Writer) std.Io.Writer.Error!void {
        try w.writeByte('(');
        var it = s.entries.iterator();
        var first = true;
        while (it.next()) |entry| {
            if (!first) {
                try w.writeAll(", ");
            } else first = false;
            try w.writeAll(entry.key_ptr.*);
        }
        try w.writeByte(')');
    }

    pub const dont_walk = true;
};

pub const LabelScope = struct {
    entries: std.StringHashMapUnmanaged(*LabelledStmt) = .empty,

    pub fn insert(s: *LabelScope, allocator: std.mem.Allocator, symbol: *LabelledStmt) ?*LabelledStmt {
        const result = s.entries.getOrPut(allocator, symbol.name.text()) catch @panic("OOM");
        if (result.found_existing) {
            return result.value_ptr.*;
        }
        result.value_ptr.* = symbol;
        return null;
    }

    pub fn deinit(s: *LabelScope, allocator: std.mem.Allocator) void {
        s.entries.deinit(allocator);
    }

    pub fn format(s: LabelScope, w: *std.Io.Writer) std.Io.Writer.Error!void {
        try w.writeByte('(');
        var it = s.entries.iterator();
        var first = true;
        while (it.next()) |entry| {
            if (!first) {
                try w.writeAll(", ");
            } else first = false;
            try w.writeAll(entry.key_ptr.*);
        }
        try w.writeByte(')');
    }

    pub const dont_walk = true;
};

pub const Head = struct {
    flags: std.EnumSet(Flag) = .empty,
    position: Code.Offset = 0,

    pub fn toFake(h: Head) Head {
        return .{
            .position = h.position,
            .flags = h.flags.unionWith(.initOne(.fake)),
        };
    }
};
