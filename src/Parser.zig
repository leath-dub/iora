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

pub fn init(ctx: *GeneralContext, lexer: Lexer) Parser {
    return .{
        .ctx = ctx,
        .ast = Ast.init(ctx),
        .lexer = lexer,
    };
}

pub fn parse(p: *Parser) Ast {
    p.ast.root = p.parseSourceFile();
    return p.ast;
}

fn parseSourceFile(p: *Parser) node.SourceFile {
    var sf = p.create(node.SourceFile);

    sf.imports = p.zeroOrMore(node.Import, parseImport, onImportToken);
    sf.decls = p.zeroOrMore(node.Decl, parseDecl, notEof);

    return ok(sf);
}

fn onImportToken(p: *Parser) bool {
    return p.on(.import);
}

fn notEof(p: *Parser) bool {
    return !p.on(.eof);
}

fn parseImport(p: *Parser) node.Import {
    var imp = p.create(node.Import);
    if (!p.skipIf(.import)) return err(imp);
    if (!p.expect(.string_lit)) return err(imp);
    imp.module = p.munch();
    if (!p.skipIf(.semicolon)) return err(imp);
    return ok(imp);
}

fn parseDecl(p: *Parser) node.Decl {
    return again: switch (p.at().type) {
        .let, .@"var", .def => .{ .@"var" = p.parseVarDecl() },
        .fun => .{ .fun = p.parseFunDecl() },
        .type => .{ .type = p.parseTypeDecl(true) },
        else => if (p.expectOneOf(.{ .let, .@"var", .def, .fun, .type }, "declaration")) {
            continue :again p.at().type;
        } else .dirty,
    };
}

fn parseFunDecl(p: *Parser) node.FunDecl {
    var fun = p.create(node.FunDecl);
    if (!p.skipIf(.fun)) return err(fun);
    fun.name = p.parseScopedIdent();
    if (!p.skipIf(.lparen)) return err(fun);
    fun.params = p.zeroOrMoreDelim(node.FunParam, parseFunParam, .comma, .rparen);
    if (!p.skipIf(.rparen)) return err(fun);
    if (p.on(.local)) {
        _ = p.next();
        fun.is_local = true;
    }
    if (p.on(.arrow)) {
        _ = p.next();
        fun.return_type = p.parseType();
    }
    fun.body = p.parseCompStmt();
    return ok(fun);
}

fn parseCompStmt(p: *Parser) node.CompStmt {
    var comp = p.create(node.CompStmt);
    if (!p.skipIf(.lbrace)) return err(comp);
    comp.stmts = p.zeroOrMore(node.Stmt, parseStmt, notOnRbrace);
    if (!p.skipIf(.rbrace)) return err(comp);
    return ok(comp);
}

fn notOnRbrace(p: *Parser) bool {
    return !p.on(.rbrace);
}

fn parseStmt(p: *Parser) node.Stmt {
    return switch (p.at().type) {
        .fun, .let, .@"var", .def, .type => .{
            .decl = p.parseDecl(),
        },
        .lbrace => .{ .comp = p.parseCompStmt() },
        .@"if" => .{ .@"if" = p.parseIfStmt() },
        .@"return" => .{ .@"return" = p.parseReturnStmt() },
        .@"while" => .{ .@"while" = p.parseWhileStmt() },
        .case => .{ .case = p.parseCaseStmt() },
        .@"defer" => .{ .@"defer" = p.parseDeferStmt() },
        else => p.parseAssignOrExpr(),
    };
}

fn parseDeferStmt(p: *Parser) node.DeferStmt {
    var defer_ = p.create(node.DeferStmt);
    if (!p.skipIf(.@"defer")) return err(defer_);
    defer_.child = p.ast.box(p.parseStmt());
    return ok(defer_);
}

fn parseAssignOrExpr(p: *Parser) node.Stmt {
    const left = p.parseExpr();
    if (p.on(.equal)) {
        var assign = p.create(node.Assign);
        assign.lvalue = left;
        _ = p.next();
        assign.rvalue = p.parseExpr();
        if (!p.skipIf(.semicolon)) return .{ .assign = err(assign) };
        return .{ .assign = assign };
    }
    if (!p.skipIf(.semicolon)) return .{ .expr = .dirty };
    return .{ .expr = left };
}

fn parseCaseStmt(p: *Parser) node.CaseStmt {
    var case = p.create(node.CaseStmt);
    if (!p.skipIf(.case)) return err(case);
    case.arg = p.parseExpr();
    if (!p.skipIf(.lbrace)) return err(case);
    case.arms = p.zeroOrMore(node.CaseArm, parseCaseArm, notOnRbrace);
    if (!p.skipIf(.rbrace)) return err(case);
    return ok(case);
}

fn parseCaseArm(p: *Parser) node.CaseArm {
    var arm = p.create(node.CaseArm);
    arm.patt = p.parseCasePatt();
    if (!p.skipIf(.arrow)) return err(arm);
    arm.action = p.parseStmt();
    return ok(arm);
}

fn parseCasePatt(p: *Parser) node.CasePatt {
    return switch (p.at().type) {
        .@"var", .let => .{ .binding = p.parseCaseBinding() },
        .colon => blk: {
            _ = p.next();
            break :blk .{ .type = p.parseType() };
        },
        .@"else" => blk: {
            _ = p.next();
            break :blk .default;
        },
        else => .{ .expr = p.parseExpr() },
    };
}

fn parseCaseBinding(p: *Parser) node.CaseBinding {
    var bind = p.create(node.CaseBinding);
    if (!p.expectOneOf(.{ .let, .@"var" }, "keyword 'let' or 'var'")) {
        return err(bind);
    }
    bind.declarator = p.munch();
    bind.name = p.parseIdent();
    if (!p.skipIf(.colon)) return err(bind);
    bind.type = p.parseType();
    return ok(bind);
}

fn parseWhileStmt(p: *Parser) node.WhileStmt {
    var while_ = p.create(node.WhileStmt);
    if (!p.skipIf(.@"while")) return err(while_);
    while_.cond = p.parseCond();
    while_.body = p.parseCompStmt();
    return ok(while_);
}

fn parseReturnStmt(p: *Parser) node.ReturnStmt {
    var ret = p.create(node.ReturnStmt);
    if (!p.skipIf(.@"return")) return err(ret);
    ret.child = p.parseExpr();
    if (!p.skipIf(.semicolon)) return err(ret);
    return ok(ret);
}

fn parseIfStmt(p: *Parser) node.IfStmt {
    var if_stmt = p.create(node.IfStmt);
    if (!p.skipIf(.@"if")) return err(if_stmt);
    if_stmt.cond = p.parseCond();
    if_stmt.then_arm = p.parseCompStmt();
    if (p.on(.@"else")) {
        if_stmt.else_arm = p.parseElse();
    }
    return ok(if_stmt);
}

fn parseElse(p: *Parser) node.Else {
    return switch (p.at().type) {
        .@"if" => .{ .@"if" = p.ast.box(p.parseIfStmt()) },
        else => .{ .comp = p.parseCompStmt() },
    };
}

fn parseCond(p: *Parser) node.Cond {
    return switch (p.at().type) {
        .let, .@"var" => .{ .sum_type_reduce = p.parseSumTypeReduce() },
        else => .{ .expr = p.parseExpr() },
    };
}

fn parseSumTypeReduce(p: *Parser) node.SumTypeReduce {
    var reduce = p.create(node.SumTypeReduce);
    if (!p.expectOneOf(.{ .let, .@"var" }, "keyword 'let' or 'var'")) {
        return err(reduce);
    }
    reduce.declarator = p.munch();
    reduce.name = p.parseIdent();
    if (!p.skipIf(.colon)) return err(reduce);
    reduce.reduction = p.parseType();
    if (!p.skipIf(.equal)) return err(reduce);
    reduce.value = p.parseExpr();
    return ok(reduce);
}

fn parseVarDecl(p: *Parser) node.VarDecl {
    var var_decl = p.create(node.VarDecl);

    const start = p.munch();
    assert(start.type == .let or start.type == .@"var" or start.type == .def);
    var_decl.declarator = start;

    var_decl.name = p.parseIdent();

    if (p.on(.colon)) {
        _ = p.next();
        var_decl.type = p.parseType();
    }

    if (p.on(.equal)) {
        _ = p.next();
        var_decl.init_expr = p.parseExpr();
    }

    if (!p.skipIf(.semicolon)) return err(var_decl);

    return ok(var_decl);
}

fn parseTypeDecl(p: *Parser, delim: bool) node.TypeDecl {
    var type_decl = p.create(node.TypeDecl);
    if (!p.skipIf(.type)) return err(type_decl);
    type_decl.name = p.parseIdent();
    if (!p.skipIf(.equal)) return err(type_decl);
    type_decl.type = p.parseType();
    if (delim) {
        if (!p.skipIf(.semicolon)) return err(type_decl);
    }
    return ok(type_decl);
}

// builtin: BuiltinType,
// coll: CollType,
// tuple: TupleType,
// @"struct": StructType,
// sum: SumType,
// @"enum": EnumType,
// ptr: PtrType,
// err: ErrType,
// fun: FunType,

fn parseType(p: *Parser) node.Type {
    return again: switch (p.at().type) {
        .s8,
        .u8,
        .s16,
        .u16,
        .s32,
        .u32,
        .s64,
        .u64,
        .f32,
        .f64,
        .bool,
        .string,
        .unit,
        => .{ .builtin = p.createBuiltinType() },
        .lbracket => .{ .coll = p.parseCollType() },
        .@"struct" => .{ .@"struct" = p.parseStructType() },
        .@"enum" => .{ .@"enum" = p.parseEnumType() },
        .lparen => p.parseTupleOrSumType(),
        .scope, .ident => .{ .scoped_ident = p.parseScopedIdent() },
        .bang => .{ .err = p.parseErrType() },
        .star => .{ .ptr = p.parsePtrType() },
        .fun => .{ .fun = p.parseFunType() },
        else => if (p.expectOneOf(.{
            .s8,
            .u8,
            .s16,
            .u16,
            .s32,
            .u32,
            .s64,
            .u64,
            .f32,
            .f64,
            .bool,
            .string,
            .unit,
            .lbracket,
            .@"struct",
            .@"enum",
            .lparen,
            .scope,
            .ident,
            .bang,
            .star,
            .fun,
        }, "a type")) {
            continue :again p.at().type;
        } else .dirty,
    };
}

fn parseFunType(p: *Parser) node.FunType {
    var fun = p.create(node.FunType);
    if (!p.skipIf(.fun)) return err(fun);
    if (!p.skipIf(.lparen)) return err(fun);
    fun.params = p.zeroOrMoreDelim(node.FunParam, parseFunParam, .comma, .rparen);
    if (!p.skipIf(.rparen)) return err(fun);
    if (p.on(.local)) {
        _ = p.next();
        fun.is_local = true;
    }
    if (p.on(.arrow)) {
        _ = p.next();
        fun.return_type = p.ast.box(p.parseType());
    }
    return ok(fun);
}

fn parseFunParam(p: *Parser) node.FunParam {
    var param = p.create(node.FunParam);
    if (p.on(.ddot)) {
        _ = p.next();
        param.unwrap = true;
    }
    param.name = p.parseIdent();
    if (!p.skipIf(.colon)) return err(param);
    param.type = p.parseType();
    return ok(param);
}

fn parseErrType(p: *Parser) node.ErrType {
    var err_type = p.create(node.ErrType);
    if (!p.skipIf(.bang)) return err(err_type);
    err_type.child = p.ast.box(p.parseType());
    return ok(err_type);
}

fn parsePtrType(p: *Parser) node.PtrType {
    var ptr = p.create(node.PtrType);
    if (!p.skipIf(.star)) return err(ptr);
    ptr.child = p.ast.box(p.parseType());
    return ok(ptr);
}

fn parseTupleOrSumType(p: *Parser) node.Type {
    if (!p.skipIf(.lparen)) return .dirty;
    if (p.on(.type)) {
        return .{ .sum = p.subparseSumType(null) };
    }
    const first = p.parseType();
    return again: switch (p.at().type) {
        .comma, .rparen => .{ .tuple = p.subparseTupleType(first) },
        .pipe => .{ .sum = p.subparseSumType(.{ .type = first }) },
        else => if (p.expectOneOf(.{
            .comma,
            .rparen,
            .pipe,
        }, "a tuple or sum type")) {
            continue :again p.at().type;
        } else .dirty,
    };
}

fn subparseTupleType(p: *Parser, first: ?node.Type) node.TupleType {
    var tuple = p.create(node.TupleType);
    tuple.types = p.oneOrMoreDelimWithFirst(node.Type, first, parseType, .comma, .rparen);
    if (!p.skipIf(.rparen)) return err(tuple);
    return ok(tuple);
}

fn subparseSumType(p: *Parser, first: ?node.TypeOrInlineDecl) node.SumType {
    var sum = p.create(node.SumType);
    sum.alts = p.oneOrMoreDelimWithFirst(node.TypeOrInlineDecl, first, parseTypeOrInlineDecl, .pipe, .rparen);
    if (!p.skipIf(.rparen)) return err(sum);
    return ok(sum);
}

fn parseTypeOrInlineDecl(p: *Parser) node.TypeOrInlineDecl {
    return switch (p.at().type) {
        .type => .{ .type_decl = p.parseTypeDecl(false) },
        else => .{ .type = p.parseType() },
    };
}

fn parseEnumType(p: *Parser) node.EnumType {
    var enum_type = p.create(node.EnumType);
    if (!p.skipIf(.@"enum")) return err(enum_type);
    if (!p.skipIf(.lbrace)) return err(enum_type);
    enum_type.alts = p.oneOrMoreCsv(node.Ident, parseIdent, .rbrace);
    if (!p.skipIf(.rbrace)) return err(enum_type);
    return ok(enum_type);
}

fn createBuiltinType(p: *Parser) node.BuiltinType {
    var builtin_type = p.create(node.BuiltinType);
    builtin_type.token = p.munch();
    return ok(builtin_type);
}

fn parseStructType(p: *Parser) node.StructType {
    var struct_type = p.create(node.StructType);
    if (!p.skipIf(.@"struct")) return err(struct_type);
    if (!p.skipIf(.lbrace)) return err(struct_type);
    struct_type.fields = p.oneOrMoreCsv(node.StructField, parseStructField, .rbrace);
    if (!p.skipIf(.rbrace)) return err(struct_type);
    return ok(struct_type);
}

fn parseStructField(p: *Parser) node.StructField {
    var field = p.create(node.StructField);
    field.name = p.parseIdent();
    if (!p.skipIf(.colon)) return err(field);
    field.type = p.parseType();
    if (p.on(.equal)) {
        _ = p.next();
        field.default = p.parseExpr();
    }
    return ok(field);
}

fn parseCollType(p: *Parser) node.CollType {
    var coll = p.create(node.CollType);
    if (!p.skipIf(.lbracket)) return err(coll);
    if (!p.on(.rbracket)) {
        coll.index_expr = p.parseExpr();
    }
    if (!p.skipIf(.rbracket)) return err(coll);
    coll.value_type = p.ast.box(p.parseType());
    return ok(coll);
}

fn parseIdent(p: *Parser) node.Ident {
    var ident = p.create(node.Ident);
    if (p.expect(.ident)) {
        ident.token = p.munch();
        return ok(ident);
    }
    return err(ident);
}

// TODO: see if pratt parser would improve performance in a meaningful way

// The order here determines the precendance (low to high)
const PrecGroup = enum {
    or_op,
    and_op,
    bor_op,
    bxor_op,
    band_op,
    equality,
    relational,
    shift,
    additive,
    multiplicative,
    unary,
    postfix,

    fn next(grp: PrecGroup) PrecGroup {
        return @enumFromInt(@intFromEnum(grp) + 1);
    }
};

fn parseExpr(p: *Parser) node.Expr {
    return ok(p.parseBinGroup(.or_op));
}

const TokenTypeSet = std.EnumSet(TokenType);
const PrecToToken = std.EnumMap(PrecGroup, TokenTypeSet);

const prec_to_token = blk: {
    @setEvalBranchQuota(2000);
    break :blk PrecToToken.init(.{
        .or_op = TokenTypeSet.init(.{ .@"or" = true }),
        .and_op = TokenTypeSet.init(.{ .@"and" = true }),
        .bor_op = TokenTypeSet.init(.{ .pipe = true }),
        .bxor_op = TokenTypeSet.init(.{ .carat = true }),
        .band_op = TokenTypeSet.init(.{ .amper = true }),
        .equality = TokenTypeSet.init(.{ .dequal = true, .not_equal = true }),
        .relational = TokenTypeSet.init(.{
            .lt = true,
            .lt_equal = true,
            .gt = true,
            .gt_equal = true,
        }),
        .shift = TokenTypeSet.init(.{ .lshift = true, .rshift = true }),
        .additive = TokenTypeSet.init(.{ .plus = true, .minus = true }),
        .multiplicative = TokenTypeSet.init(.{ .star = true, .slash = true }),
        .unary = null,
        .postfix = null,
    });
};

fn parseBinGroup(p: *Parser, comptime prec: PrecGroup) node.Expr {
    switch (prec) {
        .unary => return p.parseUnaryGroup(),
        .postfix => return p.parsePostfixGroup(),
        else => {},
    }

    const valid_ops = prec_to_token.get(prec).?;

    var expr = p.parseBinGroup(prec.next());
    while (valid_ops.contains(p.at().type)) {
        const left = p.ast.box(ok(expr));
        const op = p.munch();
        const right = p.ast.box(ok(p.parseBinGroup(prec.next())));
        expr = .{ .bin = p.createInit(node.BinExpr, .{
            .left = left,
            .op = op,
            .right = right,
        }) };
    }

    return expr;
}

fn parseUnaryGroup(p: *Parser) node.Expr {
    if (switch (p.at().type) {
        .amper, .inc, .dec, .star, .minus => true,
        else => false,
    }) {
        return .{ .unary = p.createInit(node.UnaryExpr, .{
            .op = p.munch(),
            .operand = p.ast.box(ok(p.parseUnaryGroup())),
        }) };
    }
    return p.parsePostfixGroup();
}

fn parsePostfixGroup(p: *Parser) node.Expr {
    const left = p.parseAtomExpr();
    switch (p.at().type) {
        .lbracket => return .{ .coll_access = p.parseCollAccessExpr(left) },
        .inc, .dec, .bang, .qmark => return .{ .postfix = p.createPostfixExpr(left) },
        .lparen => return .{ .call = p.parseCallExpr(left) },
        .dot => return .{ .field_access = p.parseFieldAccessExpr(left) },
        else => {},
    }
    return left;
}

fn parseFieldAccessExpr(p: *Parser, left: node.Expr) node.FieldAccessExpr {
    var access = p.create(node.FieldAccessExpr);
    if (!p.skipIf(.dot)) return err(access);
    access.value = p.ast.box(left);
    access.field = p.parseIdent();
    return ok(access);
}

fn parseCallExpr(p: *Parser, left: node.Expr) node.CallExpr {
    var call = p.create(node.CallExpr);
    if (!p.skipIf(.lparen)) return err(call);
    call.callable = p.ast.box(left);
    call.args = p.zeroOrMoreDelim(node.CallExprArg, parseCallExprArg, .comma, .rparen);
    if (!p.skipIf(.rparen)) return err(call);
    return ok(call);
}

fn createPostfixExpr(p: *Parser, left: node.Expr) node.PostfixExpr {
    return p.createInit(node.PostfixExpr, .{
        .op = p.munch(),
        .operand = p.ast.box(left),
    });
}

fn parseCollAccessExpr(p: *Parser, left: node.Expr) node.CollAccessExpr {
    const lvalue = p.ast.box(ok(left));
    return p.createInit(node.CollAccessExpr, .{
        .lvalue = lvalue,
        .subscript = p.ast.box(p.parseCollSubscript()),
    });
}

fn parseCollSubscript(p: *Parser) node.CollSubscript {
    if (!p.skipIf(.lbracket)) return .dirty;

    if (p.on(.colon)) {
        return .{ .range = p.parseSliceRange(null) };
    }

    const begin = p.parseExpr();
    if (p.on(.colon)) {
        return .{ .range = p.parseSliceRange(begin) };
    }

    if (!p.skipIf(.rbracket)) return .dirty;

    return .{ .expr = begin };
}

fn parseSliceRange(p: *Parser, begin: ?node.Expr) node.SliceRange {
    var range = p.create(node.SliceRange);
    if (!p.skipIf(.colon)) return err(range);
    if (begin) |b| {
        range.begin = b;
    }
    if (!p.on(.rbracket)) {
        range.end = p.parseExpr();
    }
    return ok(range);
}

fn parseAtomExpr(p: *Parser) node.Expr {
    return again: switch (p.at().type) {
        .lparen => p.parseParenOrAnonCallExpr(),
        .scope, .ident => .{ .scoped_ident = p.parseScopedIdent() },
        .char_lit, .string_lit, .int_lit, .float_lit, .true, .false => .{ .token_expr = p.createTokenExpr() },
        .u8,
        .s8,
        .u16,
        .s16,
        .u32,
        .s32,
        .u64,
        .s64,
        .f32,
        .f64,
        .bool,
        .unit,
        .string,
        => .{ .builtin_type = p.createBuiltinType() },
        else => if (p.expectOneOf(.{
            .lparen,
            .scope,
            .ident,
            .char_lit,
            .string_lit,
            .int_lit,
            .float_lit,
            .true,
            .false,
            .s8,
            .u16,
            .s16,
            .u32,
            .s32,
            .u64,
            .s64,
            .f32,
            .f64,
            .bool,
            .unit,
            .string,
        }, "an atomic expression")) {
            continue :again p.at().type;
        } else .dirty,
    };
}

fn parseParenOrAnonCallExpr(p: *Parser) node.Expr {
    if (!p.skipIf(.lparen)) return .dirty;
    if (p.on(.rparen)) {
        _ = p.next();
        return .{ .anon_call = .{} };
    }

    const first = p.parseCallExprArg();
    switch (first) {
        .labelled => {
            return .{ .anon_call = p.handleAnonCallExpr(first) };
        },
        .dirty => return .dirty,
        else => {},
    }

    return again: switch (p.at().type) {
        .comma => .{ .anon_call = p.handleAnonCallExpr(first) },
        .rparen => p.handleParenExpr(first.expr),
        else => if (p.expectOneOf(.{ .comma, .rparen }, "a parenthesized expression or anonymous call")) {
            continue :again p.at().type;
        } else .dirty,
    };
}

fn handleParenExpr(p: *Parser, sub_expr_: node.Expr) node.Expr {
    var sub_expr = sub_expr_;
    if (!p.skipIf(.rparen)) switch (sub_expr) {
        .dirty => {},
        inline else => |*ex| {
            ex.head.flags.insert(.dirty);
        },
    };
    return sub_expr;
}

const MaybeLabelledExpr = union(enum) {
    labelled: node.LabelledExpr,
    unlabelled: node.Expr,
};

fn parseCallExprArg(p: *Parser) node.CallExprArg {
    if (p.onLabel()) {
        return .{ .labelled = p.parseLabelledExpr() };
    }
    return .{ .expr = p.parseExpr() };
}

fn onLabel(p: *Parser) bool {
    const marker = p.lexer;
    const on_label = p.munch().type == .ident and p.munch().type == .colon;
    p.lexer = marker;
    return on_label;
}

fn parseLabelledExpr(p: *Parser) node.LabelledExpr {
    var labelled_expr = p.create(node.LabelledExpr);
    labelled_expr.label = p.parseIdent();
    if (!p.skipIf(.colon)) return err(labelled_expr);
    labelled_expr.expr = p.parseExpr();
    return ok(labelled_expr);
}

fn handleAnonCallExpr(p: *Parser, first: node.CallExprArg) node.AnonCallExpr {
    var call = p.create(node.AnonCallExpr);
    call.args = p.zeroOrMoreDelimWithFirst(node.CallExprArg, first, parseCallExprArg, .comma, .rparen);
    if (!p.skipIf(.rparen)) return err(call);
    return ok(call);
}

fn parseScopedIdent(p: *Parser) node.ScopedIdent {
    var scoped_ident = p.create(node.ScopedIdent);
    var first: ?node.Ident = null;
    if (p.on(.scope)) {
        first = p.emptyIdent();
        _ = p.next();
    }
    scoped_ident.idents = p.oneOrMoreDelimWithFirst(node.Ident, first, parseIdent, .scope, null);
    return ok(scoped_ident);
}

fn emptyIdent(p: *Parser) node.Ident {
    var ident = p.create(node.Ident);
    ident.token = .{ .type = .empty, .span = "<empty>" };
    return ok(ident);
}

// Create functions are intended to be called when you already know
// that the correct token to parse it has been reached.
fn createTokenExpr(p: *Parser) node.TokenExpr {
    var token_expr = p.create(node.TokenExpr);
    token_expr.token = p.munch();
    return ok(token_expr);
}

fn raiseExpect(p: *Parser, expected: []const u8) void {
    p.lexer.code.raise(p.ctx.error_out, p.lexer.cursor, "error: expected {s}, got {s}", .{ expected, Lexer.describeTokenType(p.lexer.peek().type) }) catch unreachable;
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
    return p.expectOneOf(.{tt}, Lexer.describeTokenType(tt));
}

fn expectOneOf(p: *Parser, comptime args: anytype, comptime expected: []const u8) bool {
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
        p.raiseExpect(expected);
    } else {
        p.panic = false;
    }
    return match;
}

// "barrier" tokens are tokens which cannot be skipped over when trying to
// syncronize the parser state during panic mode.
fn isBarrierToken(tt: TokenType) bool {
    return switch (tt) {
        .type, .let, .@"var", .def, .fun => true,
        else => false,
    };
}

fn advance(p: *Parser, comptime args: anytype) void {
    const tts = comptime tokenTypes(args);
    var tok = p.lexer.peek();
    while (tok.type != .eof) {
        inline for (tts) |tt| {
            if (tt == tok.type) {
                return;
            }
        }
        tok = p.next();
        if (isBarrierToken(tok.type)) {
            // Only check if it is a barrier token if it is not the first
            // invalid token
            return;
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
fn ensureProgress(p: *Parser, func: anytype) @typeInfo(@TypeOf(func)).@"fn".return_type.? {
    const cursor = p.lexer.cursor;
    const res = func(p);
    if (cursor == p.lexer.cursor) {
        _ = p.next();
    }
    return res;
}

fn create(p: *Parser, comptime N: type) N {
    var n = N{};
    n.head.position = p.lexer.cursor;
    return n;
}

fn createInit(p: *Parser, comptime N: type, init_: anytype) N {
    var n: N = undefined;
    inline for (meta.fields(N)) |field| {
        if (@hasField(@TypeOf(init_), field.name)) {
            @field(n, field.name) = @field(init_, field.name);
        } else if (field.defaultValue()) |defv| {
            @field(n, field.name) = defv;
        } else @compileError("Field must be set or have default: " ++ field.name);
    }
    n.head.position = p.lexer.cursor;
    return n;
}

fn err(n_: anytype) @TypeOf(n_) {
    var n = n_;
    n.head.flags.insert(.dirty);
    return n;
}

fn ok(n_: anytype) @TypeOf(n_) {
    if (comptime @typeInfo(@TypeOf(n_)) == .@"union") {
        switch (n_) {
            .dirty => return .dirty,
            inline else => |value, tag| {
                return @unionInit(@TypeOf(n_), @tagName(tag), ok(value));
            },
        }
    }
    var n = n_;
    if (Ast.lastChild(&n)) |child| switch (child) {
        inline else => |ref| ref.head.flags.insert(.last_child),
    };
    return n;
}

fn zeroOrMore(p: *Parser, comptime T: type, comptime func: fn (p: *Parser) T, comptime cond: fn (p: *Parser) bool) []T {
    var values: std.ArrayList(T) = .{};
    defer values.deinit(p.ctx.allocator);

    while (cond(p)) {
        values.append(p.ctx.allocator, p.ensureProgress(func)) catch @panic("OOM");
    }

    return p.ast.own(T, values.items);
}

fn delimWithFirst(p: *Parser, comptime T: type, first: ?T, comptime func: fn (p: *Parser) T, delim: TokenType, end: ?TokenType, at_least_one_: bool) []T {
    var values: std.ArrayList(T) = .{};
    defer values.deinit(p.ctx.allocator);

    if (first) |f| {
        values.append(p.ctx.allocator, f) catch @panic("OOM");
    }

    var at_least_one = at_least_one_;
    if (end) |e| {
        at_least_one = at_least_one or !p.on(e);
    }

    if (first == null and at_least_one) {
        values.append(p.ctx.allocator, func(p)) catch @panic("OOM");
    }

    if (end) |e| {
        while (p.on(delim)) {
            _ = p.next();
            if (p.on(e)) {
                break;
            }
            values.append(p.ctx.allocator, func(p)) catch @panic("OOM");
        }
    } else {
        while (p.on(delim)) {
            _ = p.next();
            values.append(p.ctx.allocator, func(p)) catch @panic("OOM");
        }
    }

    return p.ast.own(T, values.items);
}

fn oneOrMoreDelimWithFirst(p: *Parser, comptime T: type, first: ?T, comptime func: fn (p: *Parser) T, delim: TokenType, end: ?TokenType) []T {
    return p.delimWithFirst(T, first, func, delim, end, true);
}

fn zeroOrMoreDelimWithFirst(p: *Parser, comptime T: type, first: ?T, comptime func: fn (p: *Parser) T, delim: TokenType, end: ?TokenType) []T {
    return p.delimWithFirst(T, first, func, delim, end, false);
}

fn zeroOrMoreDelim(p: *Parser, comptime T: type, comptime func: fn (p: *Parser) T, delim: TokenType, end: ?TokenType) []T {
    return p.delimWithFirst(T, null, func, delim, end, false);
}

fn oneOrMoreDelim(p: *Parser, comptime T: type, comptime func: fn (p: *Parser) T, delim: TokenType, end: ?TokenType) []T {
    return p.oneOrMoreDelimWithFirst(T, null, func, delim, end);
}

fn oneOrMoreCsv(p: *Parser, comptime T: type, comptime func: fn (p: *Parser) T, delim: TokenType) []T {
    var values: std.ArrayList(T) = .{};
    defer values.deinit(p.ctx.allocator);

    values.append(p.ctx.allocator, func(p)) catch @panic("OOM");
    while (p.at().type == .comma) {
        _ = p.next();
        if (p.on(delim)) {
            break;
        }
        values.append(p.ctx.allocator, func(p)) catch @panic("OOM");
    }

    return p.ast.own(T, values.items);
}
