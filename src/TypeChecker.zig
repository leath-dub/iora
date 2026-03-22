const std = @import("std");

const Ast = @import("Ast.zig");
const node = @import("node.zig");
const Code = @import("Code.zig");
const GeneralContext = @import("GeneralContext.zig");
const common = @import("common.zig");
const ty = @import("type.zig");
const TypeRef = @import("type_ref.zig").TypeRef;

const TypeChecker = @This();

ast: *Ast,
code: *Code,
type_store: *ty.Store,
arena: std.heap.ArenaAllocator,
global_scope: *node.Scope,
scopes: std.SegmentedList(*node.Scope, 128) = .{},
label_scopes: std.SegmentedList(*node.LabelScope, 128) = .{},

pub fn init(ast: *Ast, code: *Code, store: *ty.Store) TypeChecker {
    return .{
        .ast = ast,
        .code = code,
        .arena = ast.ctx.createLifetime(),
        .global_scope = &ast.root.?.scope,
        .type_store = store,
    };
}

pub fn deinit(tc: *TypeChecker) void {
    tc.arena.deinit();
}

pub fn enterSourceFile(tc: *TypeChecker, source_file: *node.SourceFile) void {
    tc.push(&source_file.scope);
}

pub fn exitSourceFile(tc: *TypeChecker, source_file: *node.SourceFile) void {
    tc.pop(&source_file.scope);
}

pub fn enterFunDecl(tc: *TypeChecker, fun_decl: *node.FunDecl) void {
    tc.push(&fun_decl.scope);
    tc.pushLabelScope(&fun_decl.label_scope);

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
    tc.popLabelScope(&fun_decl.label_scope);
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

pub fn exitIdentExpr(tc: *TypeChecker, ident_expr: *node.IdentExpr) void {
    if (ident_expr.is_inferred) {
        common.todoNoReturn("resolve inferred names", .{});
    }
    if (ident_expr.resolves_to == null) {
        return;
    }

    const name = &ident_expr.name;
    const symbol = ident_expr.resolves_to.?;

    if (tc.typeOf(name, symbol)) |type_ref| {
        ident_expr.type_ref = type_ref;
    }
}

pub fn exitSelectorExpr(tc: *TypeChecker, selector_expr: *node.SelectorExpr) void {
    if (selector_expr.resolves_to == null) {
        return;
    }

    const name = &selector_expr.field;
    const symbol = selector_expr.resolves_to.?;

    if (tc.typeOf(name, symbol)) |type_ref| {
        selector_expr.type_ref = type_ref;
    }
}

fn typeOf(tc: *TypeChecker, name: *const node.Ident, symbol: node.Symbol) ?TypeRef {
    return switch (symbol.data) {
        .struct_field => |st| out: {
            tc.raise(name.head.position, "cannot reference struct field {s} in this context", .{name.text()});
            // Not sure how you would raise this error, however I think it
            // is sane to set the type of the reference to be the type of
            // the field itself
            break :out tc.type_store.intern(&st.type);
        },
        .sub_type, .type_decl => .type,
        .enumerator => out: {
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
            break :out tc.type_store.intern(&t);
        },
        inline else => |foo| out: {
            if (foo.type_ref == .unset) {
                // This means that we resolved to a defintion which has not
                // been visited yet. This is the case when you do:
                //
                // let x = foo;
                // def foo = 10;
                tc.raise(name.head.position, "undefined here: {s}", .{name.text()});
                return .dirty;
            }
            break :out foo.type_ref;
        },
    };
}

// TODO: use Ast.ChildDisposition to order tree walk so that type hints
// are set for function parameters and binary operands.

pub fn exitCallExpr(tc: *TypeChecker, call: *node.CallExpr) void {
    const callable = tc.type_store.get(call.callable.getType());
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
    tc.scopes.append(tc.arena.allocator(), ref) catch @panic("OOM");
}

fn pop(tc: *TypeChecker, scope: *node.Scope) void {
    const result = tc.scopes.pop();
    std.debug.assert(result != null and result.? == scope);
}

fn pushLabelScope(tc: *TypeChecker, ref: *node.LabelScope) void {
    tc.label_scopes.append(tc.arena.allocator(), ref) catch @panic("OOM");
}

fn popLabelScope(tc: *TypeChecker, scope: *node.LabelScope) void {
    const result = tc.label_scopes.pop();
    std.debug.assert(result != null and result.? == scope);
}

fn top(tc: *TypeChecker) *node.Scope {
    return tc.scopes.at(tc.scopes.len - 1).*;
}

fn topOrNull(tc: *TypeChecker) ?*node.Scope {
    if (tc.scopes.len == 0) {
        return null;
    }
    return tc.top();
}

fn topLabelScope(tc: *TypeChecker) *node.LabelScope {
    return tc.label_scopes.at(tc.label_scopes.len - 1).*;
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
