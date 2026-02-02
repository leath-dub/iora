const Token = @import("Lexer.zig").Token;

pub const Handle = u32;

pub fn Ref(comptime N: type) type {
    return struct {
        pub const Child = N;
        handle: Handle,
    };
}

pub const SourceFile = struct {};
pub const Decls = struct {};
pub const Decl = struct {};

pub const VarDecl = struct {
    binding: Ref(Binding),
    // init_expr: ?Ref(Expr),
};

pub const Binding = struct {
    keyword: Token, // let or var
    ident: Ref(Ident),
    type: ?Ref(Type),

    pub fn mutable(b: @This()) bool {
        return b.keyword.type == .kw_var;
    }
};

pub const Ident = struct {
    token: Token,
};

pub const Type = struct {};

// This is used so we don't accidently introduce a new ast node into this file.
// If you actually want to add an ast node, be sure to increment this number
// to curb the compilation errors.
pub const ast_node_count: usize = 7;
