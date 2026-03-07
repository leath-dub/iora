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
    head: Head = .{},
    name: ScopedIdent = .{},
    params: []FunParam = &.{},
    is_local: bool = false,
    return_type: ?Type = null,
    body: CompStmt = .{},
};

pub const FunParam = struct {
    head: Head = .{},
    name: Ident = .{},
    type: Type = .dirty,
    unwrap: bool = false,
};

pub const TypeDecl = struct {
    head: Head = .{},
    name: Ident = .{},
    type: Type = .dirty,
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
    scoped_ident: ScopedIdent,
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
    head: Head = .{},
    types: []Type = &.{},
};

pub const SumType = struct {
    head: Head = .{},
    alts: []TypeOrInlineDecl = &.{},
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
    head: Head = .{},
    alts: []Ident = &.{},
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
    @"if": IfStmt,
    @"while": WhileStmt,
    case: CaseStmt,
    @"return": ReturnStmt,
    @"defer": DeferStmt,
    comp: CompStmt,
    expr: Expr,
    assign: Assign,
    dirty,
};

pub const Assign = struct {
    head: Head = .{},
    lvalue: Expr = .dirty,
    rvalue: Expr = .dirty,
};

pub const IfStmt = struct {
    head: Head = .{},
    cond: Cond = .dirty,
    then_arm: CompStmt = .{},
    else_arm: ?Else = null,
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
};

pub const Expr = union(enum) {
    postfix: PostfixExpr,
    call: CallExpr,
    field_access: FieldAccessExpr,
    coll_access: CollAccessExpr,
    unary: UnaryExpr,
    bin: BinExpr,
    // Atomic expressions
    anon_call: AnonCallExpr,
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
    head: Head = .{},
    callable: *Expr = undefined,
    args: []CallExprArg = &.{},
};

pub const AnonCallExpr = struct {
    head: Head = .{},
    args: []CallExprArg = &.{},
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

pub const FieldAccessExpr = struct {
    head: Head = .{},
    value: *Expr = undefined,
    field: Ident = .{},
};
