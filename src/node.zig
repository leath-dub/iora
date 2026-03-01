const std = @import("std");

const Token = @import("Lexer.zig").Token;
const Code = @import("Code.zig");

// TODO: Ast walking will be done using metaprogramming
// We pass pointers down to fill memory in sum routines (e.g. parse_decl)

pub const Flag = enum {
    dirty,
    last_child,
};

pub const Head = struct {
    flags: std.EnumSet(Flag) = .initEmpty(),
    position: Code.Offset = 0,
};

pub const SourceFile = struct {
    head: Head = .{},
    imports: []Import = &.{},
    decls: []Decl = &.{},
};

pub const Decl = union(enum) {
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
    init_expr: ?Expr = null,
};

pub const FunDecl = struct {
    head: Head,
    name: ScopedIdent,
    params: []FunParam,
    local: bool,
    return_type: ?Type,
    body: CompStmt,
};

pub const FunParam = struct {
    head: Head,
    name: Ident,
    type: Type,
    unwrap: bool,
};

pub const TypeDecl = struct {
    head: Head,
    name: Ident,
    type: Type,
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
    dirty,
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
    head: Head,
    args: []TypeOrInlineDecl,
};

pub const SumType = struct {
    head: Head,
    args: []TypeOrInlineDecl,
};

pub const TypeOrInlineDecl = union(enum) {
    type: Type,
    type_decl: TypeDecl,
    dirty,
};

pub const StructType = struct {
    head: Head = .{},
    fields: []StructField = &.{},
};

pub const StructField = struct {
    head: Head = .{},
    name: Ident = .{},
    type: Type = .dirty,
    default: ?Expr = null,
};

pub const EnumType = struct {
    head: Head,
    args: []Ident,
};

pub const PtrType = struct {
    head: Head,
    referent_type: *Type,
};

pub const ErrType = struct {
    head: Head,
    child: *Type,
};

pub const FunType = struct {
    head: Head,
    params: []FunParam,
    return_type: ?*Type,
    is_local: bool,
};

pub const Ident = struct {
    head: Head = .{},
    token: Token = .{},
};

pub const ScopedIdent = struct {
    head: Head = .{},
    idents: []Ident = &.{},
};

pub const Stmt = union(enum) {
    decl: Decl,
    if_stmt: IfStmt,
    while_stmt: WhileStmt,
    case_stmt: CaseStmt,
    return_stmt: ReturnStmt,
    defer_stmt: DeferStmt,
    comp_stmt: CompStmt,
    dirty,
};

pub const IfStmt = struct {
    head: Head,
    cond: Cond,
    then_arm: CompStmt,
    else_arm: ?Else,
};

pub const Else = union(enum) {
    if_stmt: *IfStmt,
    comp_stmt: CompStmt,
    dirty,
};

pub const WhileStmt = struct {
    head: Head,
    cond: Cond,
    block: CompStmt,
};

pub const Cond = union(enum) {
    sum_type_reduce: SumTypeReduce,
    expr: Expr,
    dirty,
};

pub const SumTypeReduce = struct {
    head: Head,
    name: Ident,
    reduction: Type,
    value: Expr,
};

pub const CaseStmt = struct {
    head: Head,
    arg: Expr,
    branches: []CaseBranch,
};

pub const CaseBranch = struct {
    head: Head,
    patt: CasePatt,
    action: Stmt,
};

pub const CasePatt = union(enum) {
    type: Type,
    expr: Expr,
    binding: CaseBinding,
    default,
    dirty,
};

pub const CaseBinding = struct {
    head: Head,
    name: Ident,
    type: Type,
};

pub const ReturnStmt = struct {
    head: Head,
    child: Expr,
};

pub const DeferStmt = struct {
    head: Head,
    child: Expr,
};

pub const CompStmt = struct {
    head: Head,
    stmts: []Stmt,
};

pub const Expr = union(enum) {
    postfix: PostfixExpr,
    call: CallExpr,
    field_access: FieldAccessExpr,
    coll_access: CollAccessExpr,
    unary: UnaryExpr,
    bin: BinExpr,
    // Atomic expressions
    anon_call_expr: AnonCallExpr,
    token_expr: TokenExpr,
    builtin_type: BuiltinType,
    scoped_ident: ScopedIdent,
    dirty,
};

pub const TokenExpr = struct {
    head: Head = .{},
    token: Token = .{},
};

pub const PostfixExpr = struct {
    head: Head = .{},
    op: Token,
    operand: *Expr,
};

pub const UnaryExpr = struct {
    head: Head = .{},
    op: Token,
    operand: *Expr,
};

pub const BinExpr = struct {
    head: Head = .{},
    op: Token = .{},
    left: *Expr,
    right: *Expr,
};

pub const CallExpr = struct {
    head: Head,
    callable: *Expr,
    args: []CallExprArg,
};

pub const AnonCallExpr = struct {
    head: Head,
    args: []CallExprArg,
};

pub const CallExprArg = union(enum) {
    labelled: LabelledExpr,
    expr: Expr,
    dirty,
};

pub const LabelledExpr = struct {
    head: Head,
    label: Ident,
    expr: Expr,
};

pub const CollAccessExpr = struct {
    head: Head,
    lvalue: *Expr,
    subscript: CollSubscript,
};

pub const CollSubscript = union(enum) {
    expr: *Expr,
    range: SliceRange,
    dirty,
};

pub const SliceRange = struct {
    head: Head,
    begin: ?*Expr,
    end: ?*Expr,
};

pub const FieldAccessExpr = struct {
    head: Head,
    value: *Expr,
    field: Ident,
};
