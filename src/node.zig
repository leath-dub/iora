const std = @import("std");

const Token = @import("Lexer.zig").Token;
const Code = @import("Code.zig");
const common = @import("common.zig");
const TypeVar = @import("type_ref.zig").TypeVar;

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

pub const Import = struct {
    head: Head = .{},
    module: Token = .{},
};

pub const VarDecl = struct {
    head: Head = .{},
    declarator: Token = .{},
    name: Ident = .{},
    type: ?Type = null,
    type_var: TypeVar = .unset,
    init_expr: ?Expr = null,
};

pub const DefDecl = struct {
    head: Head = .{},
    type_name: ?Ident = null,
    name: Ident = .{},
    type: ?Type = null,
    type_var: TypeVar = .unset,
    init_expr: Expr = .dirty,
};

pub const FunDecl = struct {
    head: Head = .{},
    type_name: ?Ident = null,
    name: Ident = .{},
    params: []FunParam = &.{},
    is_local: bool = false,
    return_type: ?Type = null,
    body: CompStmt = .{},
    scope: Scope = .{},
    label_scope: LabelScope = .{},
    type_var: TypeVar = .unset,
};

pub const FunParam = struct {
    head: Head = .{},
    name: Ident = .{},
    type: Type = .dirty,
    unwrap: bool = false,
    type_var: TypeVar = .unset,
};

pub const TypeDecl = struct {
    head: Head = .{},
    name: Ident = .{},
    type: Type = .dirty,
    // NOTE: this represents the underlying type
    // not the distinct type created by the declaration
    type_var: TypeVar = .unset,
    scope: Scope = .{},
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
};

pub const IdentType = struct {
    head: Head = .{},
    name: Ident = .{},
    is_inferred: bool = false,
    resolves_to: ?Symbol = null,
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
};

pub const BuiltinType = struct {
    head: Head = .{},
    token: Token = .{},
};

pub const CollType = struct {
    head: Head = .{},
    index_expr: ?Expr = null,
    value_type: *Type = undefined,
};

pub const TupleType = struct {
    head: Head = .{},
    types: []SubType = &.{},
    scope: Scope = .{},
};

pub const SubType = struct {
    head: Head = .{},
    type: Type = .dirty,
    type_var: TypeVar = .unset,
};

pub const SumType = struct {
    head: Head = .{},
    alts: []TypeOrInlineDecl = &.{},
    scope: Scope = .{},
};

pub const TypeOrInlineDecl = union(enum) {
    type: Type,
    type_decl: TypeDecl,
    dirty,
};

pub const StructType = struct {
    head: Head = .{},
    fields: []StructField = &.{},
    scope: Scope = .{},
};

pub const StructField = struct {
    head: Head = .{},
    name: Ident = .{},
    type: Type = .dirty,
    default: ?Expr = null,
};

pub const EnumType = struct {
    head: Head = .{},
    alts: []Enumerator = &.{},
    scope: Scope = .{},
};

pub const Enumerator = struct {
    head: Head = .{},
    name: Ident = .{},
};

pub const PtrType = struct {
    head: Head = .{},
    child: *Type = undefined,
};

pub const ErrType = struct {
    head: Head = .{},
    child: *Type = undefined,
};

pub const FunType = struct {
    head: Head = .{},
    is_local: bool = false,
    params: []FunParam = &.{},
    return_type: ?*Type = null,
    scope: Scope = .{},
};

pub const Ident = struct {
    head: Head = .{},
    token: Token = .{},

    pub fn text(id: Ident) []const u8 {
        return id.token.span;
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
    type_var: TypeVar = .unset,
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
    type_var: TypeVar = .unset,
    value: Expr = .dirty,
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
    type_var: TypeVar = .unset,
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

    pub fn getType(expr: *Expr) *TypeVar {
        return switch (expr.*) {
            .dirty => unreachable,
            inline else => |*foo| &foo.type_var,
        };
    }

    pub fn getTypeConst(expr: *const Expr) *const TypeVar {
        return switch (expr.*) {
            .dirty => .unset,
            inline else => |foo| &foo.type_var,
        };
    }
};

pub const IdentExpr = struct {
    head: Head = .{},
    name: Ident = .{},
    is_inferred: bool = false,
    type_var: TypeVar = .unset,
    resolves_to: ?Symbol = null,
};

pub const SelectorExpr = struct {
    head: Head = .{},
    value: *Expr = undefined,
    field: Ident = .{},
    type_var: TypeVar = .unset,
    resolves_to: ?Symbol = null,
};

pub const TokenExpr = struct {
    head: Head = .{},
    token: Token = .{},
    type_var: TypeVar = .unset,
};

pub const PostfixExpr = struct {
    head: Head = .{},
    op: Token,
    operand: *Expr,
    type_var: TypeVar = .unset,
};

pub const UnaryExpr = struct {
    head: Head = .{},
    op: Token,
    operand: *Expr,
    type_var: TypeVar = .unset,
};

pub const BinExpr = struct {
    head: Head = .{},
    op: Token = .{},
    left: *Expr,
    right: *Expr,
    type_var: TypeVar = .unset,
};

pub const CallExpr = struct {
    head: Head = .{},
    callable: *Expr = undefined,
    args: []CallExprArg = &.{},
    type_var: TypeVar = .unset,
};

pub const AnonCallExpr = struct {
    head: Head = .{},
    args: []CallExprArg = &.{},
    type_var: TypeVar = .unset,
};

pub const CallExprArg = union(enum) {
    labelled: LabelledExpr,
    expr: Expr,
    dirty,
};

pub const LabelledExpr = struct {
    head: Head = .{},
    label: Ident = .{},
    expr: Expr = .dirty,
};

pub const CollAccessExpr = struct {
    head: Head = .{},
    lvalue: *Expr,
    subscript: *CollSubscript,
    type_var: TypeVar = .unset,
};

pub const CollSubscript = union(enum) {
    expr: Expr,
    range: SliceRange,
    dirty,
};

pub const SliceRange = struct {
    head: Head = .{},
    begin: ?Expr = null,
    end: ?Expr = null,
};

pub const Flag = enum {
    dirty,
    last_child,
    resolving,
};

pub const Symbol = struct {
    name: []const u8,
    data: Data,
    type_ctx: ?TypeCtx = null,

    pub fn head(sym: Symbol) *Head {
        return switch (sym.data) {
            inline else => |foo| &foo.head,
        };
    }

    pub fn fieldNameFromType(comptime T: type) ?[]const u8 {
        inline for (std.meta.fields(Data)) |field| {
            if (field.type == T) {
                return field.name;
            }
        }
    }

    pub fn fromSymbolLike(n: anytype) Symbol {
        return .{
            .name = n.name.text(),
            .data = Symbol.Data.fromSymbolLike(n),
        };
    }

    pub fn format(
        s: Symbol,
        w: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (s.data) {
            inline else => |value| {
                try w.print("{s}", .{common.unqualTypeName(@TypeOf(value.*))});
            },
        }
    }

    pub const Data = union(enum) {
        def_decl: *DefDecl,
        var_decl: *VarDecl,
        fun_decl: *FunDecl,
        fun_param: *FunParam,
        type_decl: *TypeDecl,
        enumerator: *Enumerator,
        sub_type: *SubType,
        case_binding: *CaseBinding,
        struct_field: *StructField,
        sum_type_reduce: *SumTypeReduce,

        pub fn fromSymbolLike(n: anytype) Data {
            const field = comptime Symbol.fieldNameFromType(@TypeOf(n)).?;
            return @unionInit(Data, field, n);
        }
    };

    pub const TypeCtx = union(enum) {
        type_decl: *TypeDecl,
        enum_type: *EnumType,
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
    flags: std.EnumSet(Flag) = .initEmpty(),
    position: Code.Offset = 0,
};
