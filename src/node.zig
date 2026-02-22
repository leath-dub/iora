const Token = @import("Lexer.zig").Token;

pub const Handle = u32;

pub fn Ref(comptime N: type) type {
    return struct {
        pub const Child = N;
        handle: Handle,
    };
}

pub fn Deref(comptime R: type) type {
    if (!@hasDecl(R, "Child") or Ref(R.Child) != R) {
        @compileLog(R);
        @compileError("cannot deref type not instanciated with node.Ref");
    }
    return R.Child;
}

pub fn Access(comptime N: type) type {
    return struct {
        ptr: *N,
        handle: Handle,
    };
}

pub const SourceFile = struct {
    imports: Ref(Imports),
    decls: Ref(Decls),
};

pub const Decls = struct {
    pub const child = .{.decl};
};

pub const Imports = struct {
    pub const child = .{.import};
};

pub const Decl = struct {
    pub const child = .{
        .var_decl,
        .fun_decl,
        .type_decl,
    };
};

pub const Import = struct {
    module: Token,
};

pub const VarDecl = struct {
    declarator: Token,
    name: Ref(Ident),
    type: ?Ref(Type),
    init_expr: ?Ref(Expr),
};

pub const FunDecl = struct {
    name: Ref(ScopedIdent),
    params: Ref(FunParams),
    local: bool,
    return_type: ?Ref(Type),
    body: Ref(CompStmt),
};

pub const FunParams = struct {
    pub const child = .{.fun_param};
};

pub const FunParam = struct {
    name: Ref(Ident),
    type: Ref(Type),
    unwrap: bool,
};

pub const TypeDecl = struct {
    name: Ref(Ident),
    type: Ref(Type),
};

pub const Type = struct {
    pub const child = .{
        .builtin_type,
        .coll_type,
        .tuple_type,
        .struct_type,
        .sum_type,
        .enum_type,
        .ptr_type,
        .err_type,
        .fun_type,
    };
};

pub const BuiltinType = struct {
    token: Token,
};

pub const CollType = struct {
    index_expr: ?Ref(Expr),
    value_type: Ref(Type),
};

pub const TupleType = struct {
    pub const child = .{.type_or_inline_decl};
};

pub const StructType = struct {
    pub const child = .{.struct_field};
};

pub const StructField = struct {
    name: Ref(Ident),
    type: Ref(Type),
    default: ?Ref(Expr),
};

pub const SumType = struct {
    pub const child = .{.type_or_inline_decl};
};

pub const TypeOrInlineDecl = struct {
    pub const child = .{
        .type,
        .type_decl,
    };
};

pub const EnumType = struct {
    pub const child = .{.ident};
};

pub const PtrType = struct {
    // TODO add mut/immut information here
    referent_type: Ref(Type),
};

pub const ErrType = struct {
    pub const child = .{.type};
};

pub const FunType = struct {
    params: Ref(FunParams),
    return_type: ?Ref(Type),
    is_local: bool,
};

pub const Ident = struct {
    token: Token,
};

pub const ScopedIdent = struct {
    pub const child = .{.ident};
};

pub const Stmt = struct {
    pub const child = .{
        .decl,
        .if_stmt,
        .while_stmt,
        .case_stmt,
        .return_stmt,
        .defer_stmt,
        .comp_stmt,
    };
};

pub const IfStmt = struct {
    cond: Ref(Cond),
    true_branch: Ref(CompStmt),
    false_branch: ?Ref(Else),
};

pub const Else = struct {
    pub const child = .{ .if_stmt, .comp_stmt };
};

pub const WhileStmt = struct {
    cond: Ref(Cond),
    block: Ref(CompStmt),
};

pub const Cond = struct {
    pub const child = .{ .sum_type_reduce, .expr };
};

pub const SumTypeReduce = struct {
    name: Ref(Ident),
    reduction: Ref(Type),
    value: Ref(Expr),
};

pub const CaseStmt = struct {
    arg: Ref(Expr),
    branches: Ref(CaseBranches),
};

pub const CaseBranches = struct {
    pub const child = .{.case_branch};
};

pub const CaseBranch = struct {
    patt: Ref(CasePatt),
    action: Ref(Stmt),
};

pub const CasePatt = struct {
    pub const child = .{
        .type,
        .expr,
        .case_binding,
        .case_default,
    };
};

pub const CaseBinding = struct {
    name: Ref(Ident),
    type: Ref(Type),
};

pub const CaseDefault = struct {};

pub const ReturnStmt = struct {
    pub const child = .{.expr};
};

pub const DeferStmt = struct {
    pub const child = .{.stmt};
};

pub const CompStmt = struct {
    pub const child = .{.stmt};
};

pub const Expr = struct {
    pub const child = .{
        .atom_expr,
        .postfix_expr,
        .call_expr,
        .field_access_expr,
        .coll_access_expr,
        .unary_expr,
        .bin_expr,
    };
};

pub const AtomExpr = struct {
    pub const child = .{
        .token_expr,
        .builtin_type_expr,
        .scoped_ident,
    };
};

pub const TokenExpr = struct {
    token: Token,
};

pub const BuiltinTypeExpr = struct {
    token: Token,
};

pub const PostfixExpr = struct {
    op: Token,
    operand: Ref(Expr),
};

pub const UnaryExpr = struct {
    op: Token,
    operand: Ref(Expr),
};

pub const BinExpr = struct {
    op: Token,
    left_operand: Ref(Expr),
    right_operand: Ref(Expr),
};

pub const CallExpr = struct {
    callable: ?Ref(Expr),
    args: CallExprArgs,
};

pub const AnonCallExpr = struct {
    args: CallExprArgs,
};

pub const CallExprArgs = struct {
    pub const child = .{.call_arg};
};

pub const CallExprArg = struct {
    name: ?Ref(Ident),
    value: Ref(Expr),
};

pub const CollAccessExpr = struct {
    lvalue: Ref(Expr),
    subscript: Ref(CollSubscript),
};

pub const CollSubscript = struct {
    pub const child = .{
        .expr,
        .slice_range,
    };
};

pub const SliceRange = struct {
    begin: ?Ref(Expr),
    end: ?Ref(Expr),
};

pub const FieldAccessExpr = struct {
    value: Ref(Expr),
    field: Ref(Ident),
};

// This is used so we don't accidently introduce a new ast node into this file.
// If you actually want to add an ast node, be sure to increment this number
// to curb the compilation errors.
pub const ast_node_count: usize = 54;
