const std = @import("std");
const math = std.math;

const Ast = @import("Ast.zig");
const node = @import("node.zig");
const Code = @import("Code.zig");
const GeneralContext = @import("GeneralContext.zig");
const common = @import("common.zig");
const util = @import("util.zig");
const ty = @import("type.zig");
const tyref = @import("type_ref.zig");
const TypeRef = tyref.TypeRef;

const TypeChecker = @This();

ast: *Ast,
code: *Code,
type_store: *ty.Store,
arena: std.heap.ArenaAllocator,
global_scope: *node.Scope,
scopes: util.ChunkedStack(*node.Scope),

pub fn init(ast: *Ast, code: *Code, store: *ty.Store) TypeChecker {
    return .{
        .ast = ast,
        .code = code,
        .type_store = store,
        .arena = ast.ctx.createLifetime(),
        .global_scope = &ast.root.?.scope,
        .scopes = .init(ast.ctx.allocator),
    };
}

pub fn deinit(tc: *TypeChecker) void {
    tc.arena.deinit();
    tc.scopes.deinit();
}

pub fn enterSourceFile(tc: *TypeChecker, source_file: *node.SourceFile) void {
    tc.push(&source_file.scope);
}

pub fn exitSourceFile(tc: *TypeChecker, source_file: *node.SourceFile) void {
    tc.pop(&source_file.scope);
}

pub fn enterFunDecl(tc: *TypeChecker, fun_decl: *node.FunDecl) void {
    tc.push(&fun_decl.scope);

    // Synthesize a function type to be interned
    const fun_type = node.FunType{
        .head = .{},
        .params = fun_decl.params,
        .return_type = if (fun_decl.return_type) |*ret| ret else null,
        .is_local = fun_decl.is_local,
    };
    var any_type: node.Type = .{ .fun = fun_type };
    fun_decl.type_ref = tc.type_store.intern(&any_type);
}

pub fn exitFunDecl(tc: *TypeChecker, fun_decl: *node.FunDecl) void {
    tc.pop(&fun_decl.scope);
}

pub fn enterCompStmt(tc: *TypeChecker, comp_stmt: *node.CompStmt) void {
    tc.push(&comp_stmt.scope);
}

pub fn exitCompStmt(tc: *TypeChecker, comp_stmt: *node.CompStmt) void {
    tc.pop(&comp_stmt.scope);
}

pub fn enterVarDecl(tc: *TypeChecker, var_decl: *node.VarDecl) void {
    if (var_decl.type) |*t| {
        var_decl.type_ref = tc.type_store.intern(t);
    }
}

pub fn exitVarDecl(tc: *TypeChecker, var_decl: *node.VarDecl) void {
    if (var_decl.init_expr) |*init_expr| {
        if (var_decl.type != null) {
            tc.tryCastTo(init_expr, var_decl.type_ref);
            const rhs_type = init_expr.getType().*;

            if (var_decl.type_ref != rhs_type) {
                tc.raise(var_decl.type.?.head().position, "declared type {f} differs from right hand-side {f}", .{
                    ty.formatView(tc.type_store, var_decl.type_ref),
                    ty.formatView(tc.type_store, rhs_type),
                });
            }
        }
        var_decl.type_ref = init_expr.getType().*;
    }

    if (var_decl.type_ref == .unit) {
        const pos = if (var_decl.type != null)
            var_decl.type.?.head().position
        else
            var_decl.name.head.position;
        tc.raise(pos, "variable cannot have type unit", .{});
    }
}

fn tryCastTo(_: *TypeChecker, expr: *node.Expr, type_ref: TypeRef) void {
    switch (expr.getType().*) {
        .f32 => switch (type_ref) {
            .f32, .f64 => expr.getType().* = type_ref,
            else => {},
        },
        .f64 => switch (type_ref) {
            .f64 => expr.getType().* = type_ref,
            else => {},
        },
        .s32 => switch (type_ref) {
            .f32, .f64 => expr.getType().* = type_ref,
            else => {},
        },
        .u32 => switch (type_ref) {
            .f64 => expr.getType().* = type_ref,
            else => {},
        },
        else => {},
    }
}

pub fn exitTokenExpr(tc: *TypeChecker, token_expr: *node.TokenExpr) void {
    const token = token_expr.token;
    token_expr.type_ref = switch (token.type) {
        .char_lit => .u8,
        .str_lit => .str,
        .int_lit => id: {
            const int_lit = token.lit.?.int;
            if (int_lit.isSigned(tc.code)) {
                if (int_lit.value <= math.maxInt(i32)) {
                    break :id .s32;
                }
                if (int_lit.value > math.maxInt(i64)) {
                    tc.raise(token.offset(tc.code), "value {d} cannot be signed as it is too large to fit in s64", .{int_lit.value});
                }
                break :id .s64;
            }

            if (int_lit.value <= std.math.maxInt(u32)) {
                break :id .u32;
            }
            break :id .u64;
        },
        .float_lit => .f32,
        else => unreachable,
    };
}

pub fn exitIdentExpr(tc: *TypeChecker, ident_expr: *node.IdentExpr) void {
    if (ident_expr.is_inferred) {
        // TODO
    }
    if (ident_expr.resolves_to == null) {
        return;
    }

    const name = &ident_expr.name;
    const symbol = ident_expr.resolves_to.?;

    tc.propagateSymbolType(&ident_expr.type_ref, name, symbol);
}

pub fn exitSelectorExpr(tc: *TypeChecker, selector_expr: *node.SelectorExpr) void {
    if (selector_expr.resolves_to == null) {
        return;
    }

    const name = &selector_expr.field;
    const symbol = selector_expr.resolves_to.?;

    tc.propagateSymbolType(&selector_expr.type_ref, name, symbol);
}

pub fn exitBinExpr(tc: *TypeChecker, bin_expr: *node.BinExpr) void {
    const left_type = bin_expr.left.getType().*;
    const right_type = bin_expr.right.getType().*;

    if (left_type != right_type) {
        tc.raise(bin_expr.op.offset(tc.code), "invalid operation: mismatched types {f} and {f}", .{
            ty.formatView(tc.type_store, left_type),
            ty.formatView(tc.type_store, right_type),
        });
    }

    bin_expr.type_ref = left_type;
}

pub fn enterUnaryExpr(_: *TypeChecker, unary_expr: *node.UnaryExpr) void {
    // Mark an integer literal which is directly prefixed by a unary '-'
    // as signed. TokenExpr validation will further make sure it can fit, but
    // this just makes it so you can do:
    //
    // let x = -10; // x: s32
    //
    // instead of always
    //
    // let x = -10s; // x: s32
    if (unary_expr.op.type == .minus and
        unary_expr.operand.* == .token_expr)
    {
        var token = &unary_expr.operand.token_expr.token;
        if (token.lit) |*lit| {
            if (lit.* == .int and lit.int.suffix == .none) {
                lit.int.suffix = .synthesized_sign;
            }
        }
    }
}

fn propagateSymbolType(tc: *TypeChecker, type_ref: *TypeRef, name: *const node.Ident, symbol: node.Symbol) void {
    switch (symbol.data) {
        .struct_field => |st| {
            tc.raise(name.head.position, "cannot reference struct field {s} in this context", .{name.text()});
            // Not sure how you would raise this error, however I think it
            // is sane to set the type of the reference to be the type of
            // the field itself
            type_ref.* = tc.type_store.intern(&st.type);
        },
        .enumerator => {
            // Bit of a hack here, need to be careful when doing things like
            // this to make sure the interner does not produce a point to
            // a stack reference
            const t = switch (symbol.type_ctx.?) {
                .enum_type => |en| node.Type{ .@"enum" = en.* },
                .type_decl => |td| node.Type{ .ident = node.IdentType{
                    .name = td.name,
                    .resolves_to = node.Symbol.fromSymbolLike(td),
                } },
            };
            type_ref.* = tc.type_store.intern(&t);
        },
        inline .sub_type, .type_decl => |t| {
            type_ref.* = tc.type_store.intern(&node.Type{ .type_of = .{ .child = &t.type } });
        },
        inline else => |foo| {
            if (foo.type_ref == .unset) {
                // This means that we resolved to a defintion which has not
                // been visited yet. This is the case when you do:
                //
                // let x = foo;
                // def foo = 10;
                tc.raise(name.head.position, "undefined here: {s}", .{name.text()});
                type_ref.* = .dirty;
            }
            type_ref.* = foo.type_ref;
        },
    }
}

// TODO: use Ast.ChildDisposition to order tree walk so that type hints
// are set for function parameters and binary operands.

pub fn exitCallExpr(tc: *TypeChecker, call: *node.CallExpr) void {
    const tid = call.callable.getType().*;
    const callable = tc.type_store.get(tid);
    call.type_ref = switch (callable) {
        .fun => |fun| fun.return_type,
        else => common.todoNoReturn("more callables", .{}),
    };
}

// Type names are resolved by PostModuleScopeResolver
pub fn enterType(_: *TypeChecker, _: *node.Type) Ast.ChildDisposition {
    return .skip;
}

fn push(tc: *TypeChecker, ref: *node.Scope) void {
    tc.scopes.push(ref) catch @panic("OOM");
}

fn pop(tc: *TypeChecker, scope: *node.Scope) void {
    std.debug.assert(tc.scopes.pop() == scope);
}

fn top(tc: *TypeChecker) *node.Scope {
    return tc.topOrNull().?;
}

fn topOrNull(tc: *TypeChecker) ?*node.Scope {
    return tc.scopes.top();
}

fn ctx(tc: *TypeChecker) *GeneralContext {
    return tc.ast.ctx;
}

inline fn raise(tc: *TypeChecker, at: Code.Offset, comptime fmt: []const u8, args: anytype) void {
    tc.code.raise(tc.ctx().error_out, at, fmt, args) catch unreachable;
}

fn insert(tc: *TypeChecker, symbol_: anytype) void {
    const position = if (@TypeOf(symbol_) != node.Symbol)
        symbol_.name.head.position
    else
        symbol_.head().position;
    const symbol = if (@TypeOf(symbol_) != node.Symbol) node.Symbol.fromSymbolLike(symbol_) else symbol_;
    if (tc.top().insert(tc.ctx().allocator, symbol)) |existing| {
        tc.raise(
            position,
            "{s} redeclared in this block; other declaration at {f}",
            .{
                symbol.name,
                tc.code.target(existing.head().position),
            },
        );
    }
}

// fn hint(tc: *TypeChecker, expr: *node.Expr, t: *node.Type) void {
//     tc.type_hints.put(tc.ctx().allocator, expr.head(), t) catch @panic("OOM");
// }
//
// fn getHint(tc: *TypeChecker, head: *node.Head) ?*node.Type {
//     return tc.type_hints.get(head);
// }
