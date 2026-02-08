const std = @import("std");
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
        .kw_let, .kw_var => _ = p.parse_var_decl(),
        // .kw_fun => p.parse_fun_decl(),
        // .kw_type => p.parse_type_decl(),
        else => if (p.expectOneOf(.{ .kw_let, .kw_var, .kw_fun, .kw_type })) {
            std.log.debug("{any}", .{p.at()});
            continue :again p.at().type;
        },
    }

    return task.output;
}

fn parse_var_decl(p: *Parser) node.Ref(node.VarDecl) {
    const task = p.startTask(node.VarDecl);
    defer p.endTask(task);

    const start = p.munch();
    assert(start.type == .kw_let or start.type == .kw_var);

    task.set(p, .name, p.parse_ident());

    if (p.on(.colon)) {
        // TODO
        // _ = p.next();
        // task.set(p, .type, p.parse_type());
        unreachable;
    }

    if (p.on(.equal)) {
        // TODO
        // task.set(p, .init_expr, p.parse_expr());
        unreachable;
    }

    _ = p.skipIf(.semicolon);

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

// fn parse_fun_decl(p: *Parser) node.Ref(node.FunDecl) {}
// fn parse_type_decl(p: *Parser) node.Ref(node.TypeDecl) {}

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
    p.ast.set(.dirty, p.parsing.?, true) catch unreachable;
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
        .kw_type, .kw_let, .kw_var, .kw_fun => true,
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
