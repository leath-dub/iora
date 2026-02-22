const std = @import("std");
const util = @import("util.zig");
const meta = std.meta;
const assert = std.debug.assert;

const Ast = @import("Ast.zig");
const node = @import("node.zig");

const Lexer = @import("Lexer.zig");
const Token = Lexer.Token;
const TokenType = Lexer.TokenType;
const GeneralContext = @import("GeneralContext.zig");

const Parser = @This();

ctx: *GeneralContext,
ast: Ast,
lexer: Lexer,
panic: bool = false,
parsing: ?node.Handle = null,

pub fn init(ctx: *GeneralContext, lexer: Lexer) Parser {
    return .{
        .ctx = ctx,
        .ast = Ast.init(ctx),
        .lexer = lexer,
    };
}

pub fn parse(p: *Parser) Ast {
    _ = p.parse_source_file();
    return p.ast;
}

fn parse_source_file(p: *Parser) node.Ref(node.SourceFile) {
    const task = p.startTask(node.SourceFile);
    defer p.endTask(task);

    _ = p.parse_imports();
    _ = p.parse_decls();

    return task.output;
}

fn parse_imports(p: *Parser) node.Ref(node.Imports) {
    const task = p.startTask(node.Imports);
    defer p.endTask(task);

    while (p.on(.kw_import)) {
        p.ensureProgress(parse_import);
    }

    return task.output;
}

fn parse_import(p: *Parser) node.Ref(node.Import) {
    const task = p.startTask(node.Import);
    defer p.endTask(task);

    if (!p.skipIf(.kw_import)) return task.output;
    if (!p.expect(.string_lit)) return task.output;
    task.set(p, .module, p.munch());

    _ = p.skipIf(.semicolon);
    return task.output;
}

fn parse_decls(p: *Parser) node.Ref(node.Decls) {
    const task = p.startTask(node.Decls);
    defer p.endTask(task);

    while (!p.on(.eof)) {
        p.ensureProgress(parse_decl);
    }

    return task.output;
}

fn parse_decl(p: *Parser) node.Ref(node.Decl) {
    const task = p.startTask(node.Decl);
    defer p.endTask(task);

    again: switch (p.at().type) {
        .kw_let, .kw_var, .kw_def => _ = p.parse_var_decl(),
        // .kw_fun => p.parse_fun_decl(),
        .kw_type => _ = p.parse_type_decl(),
        else => if (p.expectOneOf(.{ .kw_let, .kw_var, .kw_def, .kw_fun, .kw_type })) {
            continue :again p.at().type;
        },
    }

    return task.output;
}

fn parse_var_decl(p: *Parser) node.Ref(node.VarDecl) {
    const task = p.startTask(node.VarDecl);
    defer p.endTask(task);

    const start = p.munch();
    assert(start.type == .kw_let or start.type == .kw_var or start.type == .kw_def);
    task.set(p, .declarator, start);

    task.set(p, .name, p.parse_ident());

    if (p.on(.colon)) {
        _ = p.next();
        task.set(p, .type, p.parse_type());
    }

    if (p.on(.equal)) {
        // TODO
        // task.set(p, .init_expr, p.parse_expr());
        unreachable;
    }

    _ = p.skipIf(.semicolon);

    return task.output;
}

fn parse_type_decl(p: *Parser) node.Ref(node.TypeDecl) {
    const task = p.startTask(node.TypeDecl);
    defer p.endTask(task);
    if (!p.skipIf(.kw_type)) return task.output;
    task.set(p, .name, p.parse_ident());
    if (!p.skipIf(.equal)) return task.output;
    task.set(p, .type, p.parse_type());
    if (!p.skipIf(.semicolon)) return task.output;
    return task.output;
}

fn parse_ident(p: *Parser) node.Ref(node.Ident) {
    const task = p.startTask(node.Ident);
    defer p.endTask(task);
    if (p.expect(.ident)) {
        task.set(p, .token, p.munch());
    }
    return task.output;
}

fn parse_type(p: *Parser) node.Ref(node.Type) {
    const task = p.startTask(node.Type);
    defer p.endTask(task);
    again: switch (p.at().type) {
        .kw_s8,
        .kw_u8,
        .kw_s16,
        .kw_u16,
        .kw_s32,
        .kw_u32,
        .kw_s64,
        .kw_u64,
        .kw_f32,
        .kw_f64,
        .kw_bool,
        .kw_string,
        .kw_unit,
        => _ = p.parse_builtin_type(),
        .lbracket => _ = p.parse_coll_type(),
        .kw_struct => _ = p.parse_struct_type(),
        .kw_enum => _ = p.parse_enum_type(),
        .bang => _ = p.parse_err_type(),
        .star => _ = p.parse_ptr_type(),
        .kw_fun => _ = p.parse_fun_type(),
        .scope, .ident => _ = p.parse_scoped_ident(),
        .lparen => _ = p.parse_tuple_or_sum_type(),
        else => if (p.expectOneOf(.{
            .kw_s8,
            .kw_u8,
            .kw_s16,
            .kw_u16,
            .kw_s32,
            .kw_u32,
            .kw_s64,
            .kw_u64,
            .kw_f32,
            .kw_f64,
            .kw_bool,
            .kw_string,
            .kw_unit,
            .lbracket,
            .kw_struct,
            .kw_enum,
            .bang,
            .star,
            .kw_fun,
            .scope,
            .ident,
            .lparen,
        })) {
            continue :again p.at().type;
        },
    }
    return task.output;
}

fn parse_builtin_type(p: *Parser) node.Ref(node.BuiltinType) {
    const task = p.startTask(node.BuiltinType);
    defer p.endTask(task);
    task.set(p, .token, p.munch());
    return task.output;
}

fn parse_coll_type(p: *Parser) node.Ref(node.CollType) {
    const task = p.startTask(node.CollType);
    defer p.endTask(task);
    if (!p.skipIf(.lbracket)) return task.output;
    if (!p.on(.rbracket)) {
        // TODO
        // task.set(p, .index_expr, p.parse_expr());
        unreachable;
    }
    if (!p.skipIf(.rbracket)) return task.output;
    task.set(p, .value_type, p.parse_type());
    return task.output;
}

fn parse_comma_sep(p: *Parser, parse_fun: anytype, delim: TokenType) void {
    while (true) {
        if (p.on(delim)) break;
        _ = parse_fun(p);
        if (p.on(delim)) break;
        if (!p.skipIf(.comma)) break;
    }
}

fn parse_struct_type(p: *Parser) node.Ref(node.StructType) {
    const task = p.startTask(node.StructType);
    defer p.endTask(task);
    if (!p.skipIf(.kw_struct)) return task.output;
    if (!p.skipIf(.lbrace)) return task.output;
    p.parse_comma_sep(parse_struct_field, .rbrace);
    if (!p.skipIf(.rbrace)) return task.output;
    return task.output;
}

fn parse_struct_field(p: *Parser) node.Ref(node.StructField) {
    const task = p.startTask(node.StructField);
    defer p.endTask(task);
    task.set(p, .name, p.parse_ident());
    if (!p.skipIf(.colon)) return task.output;
    task.set(p, .type, p.parse_type());
    if (p.on(.equal)) {
        // TODO
        // task.set(p, .default, p.parse_expr());
        unreachable;
    }
    return task.output;
}

fn parse_enum_type(p: *Parser) node.Ref(node.EnumType) {
    const task = p.startTask(node.EnumType);
    defer p.endTask(task);
    if (!p.skipIf(.kw_enum)) return task.output;
    if (!p.skipIf(.lbrace)) return task.output;
    p.parse_comma_sep(parse_ident, .rbrace);
    if (!p.skipIf(.rbrace)) return task.output;
    return task.output;
}

fn parse_err_type(p: *Parser) node.Ref(node.ErrType) {
    const task = p.startTask(node.ErrType);
    defer p.endTask(task);
    if (!p.skipIf(.bang)) return task.output;
    _ = p.parse_type();
    return task.output;
}

fn parse_ptr_type(p: *Parser) node.Ref(node.PtrType) {
    const task = p.startTask(node.PtrType);
    defer p.endTask(task);
    if (!p.skipIf(.star)) return task.output;
    task.set(p, .referent_type, p.parse_type());
    return task.output;
}

fn parse_fun_type(p: *Parser) node.Ref(node.FunType) {
    const task = p.startTask(node.FunType);
    defer p.endTask(task);
    if (!p.skipIf(.kw_fun)) return task.output;
    task.set(p, .params, p.parse_fun_params());
    if (p.on(.kw_local)) {
        _ = p.next();
        task.set(p, .is_local, true);
    }
    if (p.on(.arrow)) {
        _ = p.next();
        task.set(p, .return_type, p.parse_type());
    }
    return task.output;
}

fn parse_fun_params(p: *Parser) node.Ref(node.FunParams) {
    const task = p.startTask(node.FunParams);
    defer p.endTask(task);
    if (!p.skipIf(.lparen)) return task.output;
    p.parse_comma_sep(parse_fun_param, .rparen);
    if (!p.skipIf(.rparen)) return task.output;
    return task.output;
}

fn parse_fun_param(p: *Parser) node.Ref(node.FunParam) {
    const task = p.startTask(node.FunParam);
    defer p.endTask(task);
    task.set(p, .name, p.parse_ident());
    if (!p.skipIf(.colon)) return task.output;
    if (p.on(.ddot)) {
        _ = p.next();
        task.set(p, .unwrap, true);
    }
    task.set(p, .type, p.parse_type());
    return task.output;
}

fn parse_scoped_ident(p: *Parser) node.Ref(node.ScopedIdent) {
    const task = p.startTask(node.ScopedIdent);
    defer p.endTask(task);
    if (p.on(.scope)) {
        _ = p.empty_ident();
        _ = p.next(); // skip '::'
    }
    _ = p.parse_ident();
    while (p.on(.scope)) {
        _ = p.next();
        _ = p.parse_ident();
    }
    return task.output;
}

fn empty_ident(p: *Parser) node.Ref(node.Ident) {
    const task = p.startTask(node.Ident);
    defer p.endTask(task);
    task.set(p, .token, Token{ .type = .empty, .span = p.at().span[0..0] });
    return task.output;
}

fn parse_type_or_inline_decl(p: *Parser) node.Ref(node.TypeOrInlineDecl) {
    const task = p.startTask(node.TypeOrInlineDecl);
    defer p.endTask(task);
    switch (p.at().type) {
        .kw_type => _ = p.parse_type_decl(),
        else => _ = p.parse_type(),
    }
    return task.output;
}

fn parse_tuple_or_sum_type(p: *Parser) node.Ref(Ast.Node) {
    // Create a task to no concreate AST node. Subparse steps can reify this
    // to the appropriate type once it is unambiguous.
    const task = p.startTask(Ast.Node);
    defer p.endTask(task);

    if (!p.skipIf(.lparen)) return task.output;

    // Both tuples and sum types share the same first child.
    // Parse it so we are looking at either ')', '|' or ','.
    _ = p.parse_type_or_inline_decl();

    again: switch (p.at().type) {
        // You can't have a union of single type so this is interpreted
        // as a tuple
        .comma, .rparen => _ = p.subparse_tuple_type(task),
        .pipe => _ = p.subparse_sum_type(task),
        else => if (p.expectOneOf(.{ .comma, .rparen, .pipe })) {
            continue :again p.at().type;
        },
    }

    if (!p.skipIf(.rparen)) return task.output;

    return task.output;
}

fn subparse_sum_type(p: *Parser, task: ParseTask(Ast.Node)) node.Ref(node.SumType) {
    while (p.on(.pipe)) {
        _ = p.next();
        if (p.on(.rparen)) {
            break;
        }
        _ = p.parse_type_or_inline_decl();
    }
    p.ast.atIndex(task.output.handle).ptr.* = .{ .sum_type = .{} };
    return node.Ref(node.SumType) { .handle = task.output.handle };
}

fn subparse_tuple_type(p: *Parser, task: ParseTask(Ast.Node)) node.Ref(node.TupleType) {
    while (p.on(.comma)) {
        _ = p.next();
        if (p.on(.rparen)) {
            break;
        }
        _ = p.parse_type_or_inline_decl();
    }
    p.ast.atIndex(task.output.handle).ptr.* = .{ .tuple_type = .{} };
    return node.Ref(node.TupleType) { .handle = task.output.handle };
}

// TODO: have expression parsing in a separate file as it is so distinct to the
// normal recursive decent parser. In fact it may be a good idea to build the
// expression tree in a temporary arena and copy into the normal AST, due to
// the fact that the AST can have re-parenting happen unless I figure out how
// to properly have suspended nodes (the issue with this is it needs to work recursively).

fn ParseTask(comptime N: type) type {
    return struct {
        parent: ?node.Handle,
        output: node.Ref(N),

        pub fn set(t: @This(), p: *Parser, comptime field: meta.FieldEnum(N), value: anytype) void {
            @field(t.ptr(p), @tagName(field)) = value;
        }

        fn ptr(t: @This(), p: *Parser) *N {
            return p.ast.at(t.output).ptr;
        }
    };
}

fn startTask(p: *Parser, comptime N: type) ParseTask(N) {
    const n = p.ast.startNode(N) catch unreachable;
    const parent = p.parsing;
    p.parsing = n.handle;
    p.ast.entry(p.parsing.?).position = p.lexer.cursor;
    return .{
        .parent = parent,
        .output = n,
    };
}

fn endTask(p: *Parser, task: anytype) void {
    p.ast.endNode(p.parsing.?);
    p.parsing = task.parent;
}

fn raiseExpect(p: *Parser, expected: []const u8) void {
    p.ast.entry(p.parsing.?).dirty = true;
    p.lexer.code.raise(p.ctx.error_out, p.lexer.cursor, "expected {s} got {f}", .{ expected, p.lexer.peek() }) catch unreachable;
}

fn tokenTypes(comptime tts: anytype) [meta.fields(@TypeOf(tts)).len]TokenType {
    const len = meta.fields(@TypeOf(tts)).len;
    var res: [len]TokenType = undefined;
    inline for (0..len) |i| {
        res[i] = @field(tts, std.fmt.comptimePrint("{d}", .{i}));
    }
    return res;
}

fn oneOf(tt: TokenType, comptime tts: []const TokenType) bool {
    inline for (tts) |exp| {
        if (exp == tt) {
            return true;
        }
    }
    return false;
}

fn expect(p: *Parser, comptime tt: TokenType) bool {
    return p.expectOneOf(.{tt});
}

fn expectOneOf(p: *Parser, comptime args: anytype) bool {
    @setEvalBranchQuota(5000);
    const tts = comptime tokenTypes(args);
    const tok = p.lexer.peek();
    const match = oneOf(tok.type, &tts);
    if (!match) {
        // If we are in a panic state already, don't report an error.
        // Try to synchronise on the currently expected token:
        // * if we can't - just backtrack and stay in a panic state.
        // * if we can - leave panic state and return true.
        if (p.panic) {
            const marker = p.lexer;
            p.advance(args);
            const curr = p.lexer.peek();
            if (oneOf(curr.type, &tts)) {
                p.panic = false;
                return true;
            }
            p.lexer = marker;
            return false;
        }
        p.panic = true;
        const FormatToks = struct {
            pub fn format(_: @This(), w: *std.Io.Writer) void {
                if (tts.len == 1) {
                    try w.print("{s}", .{@tagName(tts[0])});
                    return;
                }
                try w.print("one of [", .{});
                inline for (tts, 0..) |tt, i| {
                    if (i != 0) {
                        try w.print(", ", .{});
                    }
                    try w.print("{s}", .{@tagName(tt)});
                }
                try w.print("]", .{});
            }
        };
        p.raiseExpect(std.fmt.comptimePrint("{f}", .{FormatToks{}}));
    } else {
        p.panic = false;
    }
    return match;
}

// "barrier" tokens are tokens which cannot be skipped over when trying to
// syncronize the parser state during panic mode.
fn isBarrierToken(tt: TokenType) bool {
    return switch (tt) {
        .kw_type, .kw_let, .kw_var, .kw_def, .kw_fun => true,
        else => false,
    };
}

fn advance(p: *Parser, comptime args: anytype) void {
    const tts = comptime tokenTypes(args);
    var tok = p.lexer.peek();
    while (tok.type != .eof) : (tok = p.next()) {
        if (isBarrierToken(tok.type)) {
            return;
        }
        inline for (tts) |tt| {
            if (tt == tok.type) {
                return;
            }
        }
    }
}

fn at(p: *Parser) Token {
    return p.lexer.peek();
}

fn next(p: *Parser) Token {
    p.lexer.consume();
    return p.at();
}

fn munch(p: *Parser) Token {
    defer _ = p.next();
    return p.at();
}

fn skipIf(p: *Parser, comptime tt: TokenType) bool {
    if (p.expect(tt)) {
        _ = p.next();
        return true;
    }
    return false;
}

fn on(p: *Parser, tt: TokenType) bool {
    return p.at().type == tt;
}

// Skip token if no progress made by call to `func`
fn ensureProgress(p: *Parser, func: anytype) void {
    const cursor = p.lexer.cursor;
    _ = func(p);
    if (cursor == p.lexer.cursor) {
        _ = p.next();
    }
}
