const std = @import("std");

const Ast = @import("Ast.zig");
const node = @import("node.zig");
const Code = @import("Code.zig");
const GeneralContext = @import("GeneralContext.zig");
const common = @import("common.zig");
const ty = @import("type.zig");
const tyref = @import("type_ref.zig");
const TypeRef = tyref.TypeRef;
const TypeVar = tyref.TypeVar;

const TypeChecker = @This();

ast: *Ast,
code: *Code,
type_store: *ty.Store,
arena: std.heap.ArenaAllocator,
global_scope: *node.Scope,
scopes: std.SegmentedList(*node.Scope, 128) = .{},

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

    // Synthesize a function type to be interned
    const fun_type = node.FunType{
        .head = .{},
        .params = fun_decl.params,
        .return_type = if (fun_decl.return_type) |*ret| ret else null,
        .is_local = fun_decl.is_local,
    };
    var any_type: node.Type = .{ .fun = fun_type };
    fun_decl.type_var = .{ .id = tc.type_store.intern(&any_type) };
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
        var_decl.type_var = .{ .id = tc.type_store.intern(t) };
    }
}

pub fn exitVarDecl(tc: *TypeChecker, var_decl: *node.VarDecl) void {
    if (var_decl.init_expr) |*init_expr| {
        if (var_decl.type != null) {
            if (!tc.coerceTo(
                tc.type_store,
                init_expr.getType(),
                &var_decl.type_var,
            )) {
                tc.raise(
                    init_expr.headConst().position,
                    "type of right-hand side {f} is not convertible to declared type {f}",
                    .{
                        ty.formatView(tc.type_store, init_expr.getType()),
                        ty.formatView(tc.type_store, &var_decl.type_var),
                    },
                );
                return;
            }
        } else {
            var_decl.type_var = .{ .ptr = init_expr.getType() };
        }
    }

    if (var_decl.type_var.canon()) |id| {
        if (id == .unit) {
            const pos = if (var_decl.type != null)
                var_decl.type.?.head().position
            else
                var_decl.name.head.position;
            tc.raise(pos, "variable cannot have type unit", .{});
        }
    }
}

pub fn exitTokenExpr(_: *TypeChecker, token_expr: *node.TokenExpr) void {
    const token = token_expr.token;
    token_expr.type_var = switch (token.type) {
        .char_lit => .{ .id = .u8 },
        .str_lit => .{ .id = .str },
        .int_lit => .int,
        .float_lit => .float,
        else => unreachable,
    };
}

pub fn exitIdentExpr(tc: *TypeChecker, ident_expr: *node.IdentExpr) void {
    if (ident_expr.is_inferred) {
        ident_expr.type_var = .{ .inferred = .{
            .name = ident_expr.name.text(),
            .position = ident_expr.name.head.position,
        } };
        return;
    }
    if (ident_expr.resolves_to == null) {
        return;
    }

    const name = &ident_expr.name;
    const symbol = ident_expr.resolves_to.?;

    tc.propagateSymbolType(&ident_expr.type_var, name, symbol);
}

pub fn exitSelectorExpr(tc: *TypeChecker, selector_expr: *node.SelectorExpr) void {
    if (selector_expr.resolves_to == null) {
        return;
    }

    const name = &selector_expr.field;
    const symbol = selector_expr.resolves_to.?;

    tc.propagateSymbolType(&selector_expr.type_var, name, symbol);
}

pub fn exitBinExpr(tc: *TypeChecker, bin_expr: *node.BinExpr) void {
    const left_type = bin_expr.left.getType();
    const right_type = bin_expr.right.getType();

    // TODO use coerce instead
    if (!tc.coerce(tc.type_store, left_type, right_type)) {
        tc.raise(bin_expr.op.offset(tc.code), "invalid operation: mismatched types {f} and {f}", .{
            ty.formatView(tc.type_store, left_type),
            ty.formatView(tc.type_store, right_type),
        });
    }

    bin_expr.type_var = .{ .ptr = left_type };
}

fn propagateSymbolType(tc: *TypeChecker, type_var: *TypeVar, name: *const node.Ident, symbol: node.Symbol) void {
    return switch (symbol.data) {
        .struct_field => |st| {
            tc.raise(name.head.position, "cannot reference struct field {s} in this context", .{name.text()});
            // Not sure how you would raise this error, however I think it
            // is sane to set the type of the reference to be the type of
            // the field itself
            type_var.* = .{ .id = tc.type_store.intern(&st.type) };
        },
        .sub_type, .type_decl => type_var.* = .{ .id = .type },
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
            type_var.* = .{ .id = tc.type_store.intern(&t) };
        },
        inline else => |foo| {
            if (foo.type_var == .unset) {
                // This means that we resolved to a defintion which has not
                // been visited yet. This is the case when you do:
                //
                // let x = foo;
                // def foo = 10;
                tc.raise(name.head.position, "undefined here: {s}", .{name.text()});
                type_var.* = .{ .id = .dirty };
            }
            if (foo.type_var == .id) {
                type_var.* = .{ .id = foo.type_var.id };
            } else {
                type_var.* = .{ .ptr = &foo.type_var };
            }
        },
    };
}


fn coerceTo(tc: *TypeChecker, store: *ty.Store, from: *TypeVar, to: *TypeVar) bool {
    if (from.* == .ptr or to.* == .ptr) {
        return tc.coerceTo(
            store,
            if (from.* == .ptr) from.ptr else from,
            if (to.* == .ptr) to.ptr else to,
        );
    }

    if (from.* == .id and to.* == .id
            and from.id == to.id) {
        return true;
    }

    switch (from.*) {
        .int => {
            if (to.* == .int or to.* == .float) {
                from.* = .{ .ptr = to };
                return true;
            }
            const to_id = to.canon().?;
            if (to_id.isInteger() or
                to_id.isFloatingPoint()) {
                from.* = .{ .id = to_id };
                return true;
            }
        },
        .float => {
            if (to.* == .float) {
                from.* = .{ .ptr = to };
                return true;
            }
            const to_id = to.canon().?;
            if (to_id.isFloatingPoint()) {
                from.* = .{ .id = to_id };
                return true;
            }
        },
        .inferred => |inferred| {
            if (to.* == .inferred) {
                // Can't deduce type if both are inferred
                return false;
            }
            const name = node.Ident{
                .head = .{
                    .position = inferred.position,
                },
                .token = .{
                    .type = .ident,
                    .span = inferred.name,
                },
            };
            if (to.canon()) |to_id| {
                const to_t = store.get(to_id);
                again: switch (to_t) {
                    .user => |user| {
                        var symbol: ?node.Symbol = null;

                        common.resolveSelector(
                            .fromSymbolLike(user),
                            &name,
                            &symbol,
                        );

                        if (symbol) |sym| {
                            tc.propagateSymbolType(from, &name, sym);
                            return true;
                        } else {
                            tc.raise(
                                inferred.position,
                                "undefined in type {s}: {s}",
                                .{
                                    user.name.text(),
                                    inferred.name,
                                },
                            );
                            return false;
                        }
                    },
                    .err => |child| {
                        continue :again store.get(child);
                    },
                    .sum => |alts| {
                        for (alts) |alt| {
                            if (alt.name) |alt_name| {
                                if (std.mem.eql(u8, alt_name, inferred.name)) {
                                    from.* = .{
                                        .id = alt.type,
                                    };
                                    return true;
                                }
                            }
                        }
                    },
                    .@"enum" => |enumerators| {
                        for (enumerators) |enumerator| {
                            if (std.mem.eql(u8, enumerator, inferred.name)) {
                                from.* = .{
                                    .id = to_id,
                                };
                                return true;
                            }
                        }
                    },
                    // .tuple => {}, // TODO
                    else => {},
                }
            }
        },
        else => {},
    }

    return false;
}

// Like coerceTo however there is not assertion about the direction
// of the coercion - this is useful for binary operators.
fn coerce(tc: *TypeChecker, store: *ty.Store, a: *TypeVar, b: *TypeVar) bool {
    const a_unknown = a.isPartial();
    const b_unknown = b.isPartial();

    if (a_unknown and !b_unknown) {
        // a is unknown so we should convert from a -> b
        return tc.coerceTo(store, a, b);
    }

    if (!a_unknown and b_unknown) {
        // b is unknown so we should convert from b -> a
        return tc.coerceTo(store, b, a);
    }

    // At this point they are either both partial or both distinct
    // types. We arbitrarily just prioritize left to right conversion
    // in this case
    return tc.coerceTo(store, a, b);
}


// TODO: use Ast.ChildDisposition to order tree walk so that type hints
// are set for function parameters and binary operands.

pub fn exitCallExpr(tc: *TypeChecker, call: *node.CallExpr) void {
    if (call.callable.getType().canon()) |tid| {
        const callable = tc.type_store.get(tid);
        call.type_var = .{ .id = switch (callable) {
            .fun => |fun| fun.return_type,
            else => common.todoNoReturn("more callables", .{}),
        } };
    } else {
        call.type_var = .{ .ptr = call.callable.getType() };
    }
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

fn top(tc: *TypeChecker) *node.Scope {
    return tc.scopes.at(tc.scopes.len - 1).*;
}

fn topOrNull(tc: *TypeChecker) ?*node.Scope {
    if (tc.scopes.len == 0) {
        return null;
    }
    return tc.top();
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
