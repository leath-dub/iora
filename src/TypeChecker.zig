const std = @import("std");
const math = std.math;

const Ast = @import("Ast.zig");
const node = @import("node.zig");
const Code = @import("Code.zig");
const GeneralContext = @import("GeneralContext.zig");
const common = @import("common.zig");
const util = @import("util.zig");
const ty = @import("type.zig");
const tu = @import("type_utils.zig");
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
        .linkage = fun_decl.linkage,
    };
    var any_type: node.Type = .{ .fun = fun_type };
    fun_decl.type_ref = tc.type_store.intern(&any_type);
}

pub fn exitFunDecl(tc: *TypeChecker, fun_decl: *node.FunDecl) void {
    tc.pop(&fun_decl.scope);
}

pub fn exitFunParam(tc: *TypeChecker, param: *node.FunParam) void {
    param.type_ref = tc.type_store.intern(&param.type);
    if (param.unpack) {
        const param_td = tc.type_store.get(param.type_ref).getUnderlyingType(tc.type_store.*);
        switch (param_td) {
            .@"struct", .tuple, .slice => {},
            else => {
                tc.raise(
                    param.head.position,
                    "cannot declare parameter {s} of type {f} as unpack",
                    .{
                        param.name.text(),
                        ty.formatView(tc.type_store, param.type_ref),
                    },
                );
            },
        }
    }
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
            tc.tryCastTo(init_expr.getType(), var_decl.type_ref);
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

pub fn enterDefDecl(tc: *TypeChecker, def_decl: *node.DefDecl) void {
    if (def_decl.type) |*t| {
        def_decl.type_ref = tc.type_store.intern(t);
    }
}

pub fn exitDefDecl(tc: *TypeChecker, def_decl: *node.DefDecl) void {
    if (def_decl.type != null) {
        tc.tryCastTo(def_decl.init_expr.getType(), def_decl.type_ref);
        const rhs_type = def_decl.init_expr.getType().*;

        if (def_decl.type_ref != rhs_type) {
            tc.raise(def_decl.type.?.head().position, "declared type {f} differs from right hand-side {f}", .{
                ty.formatView(tc.type_store, def_decl.type_ref),
                ty.formatView(tc.type_store, rhs_type),
            });
        }
    }
    def_decl.type_ref = def_decl.init_expr.getType().*;

    if (def_decl.type_ref == .unit) {
        const pos = if (def_decl.type != null)
            def_decl.type.?.head().position
        else
            def_decl.name.head.position;
        tc.raise(pos, "definition cannot have type unit", .{});
    }
}

fn tryCastTo(_: *TypeChecker, from_type: *TypeRef, to_type: TypeRef) void {
    switch (from_type.*) {
        .f32 => switch (to_type) {
            .f32, .f64 => from_type.* = to_type,
            else => {},
        },
        .f64 => switch (to_type) {
            .f64 => from_type.* = to_type,
            else => {},
        },
        .s32 => switch (to_type) {
            .s64, .f32, .f64 => from_type.* = to_type,
            else => {},
        },
        .u32 => switch (to_type) {
            .s64, .u64, .f64 => from_type.* = to_type,
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
        else => if (tu.isBuiltinTokenType(token.type)) type_: {
            break :type_ tc.type_store.internDataStable(.{ .type_of = tu.toBuiltinType(token.type) });
        } else return,
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
        // Only selecting fields on a type are resolved before this pass
        // (in LexicalScopeResolver). We need to now resolve field access
        // of symbols which don't resolve to types (e.g. variable declaration).
        const lhs_t = selector_expr.value.getType().*;

        const lhs_td = tc.type_store.get(lhs_t);
        // First see if the selector is accessing a declaration of user
        // defined type
        const field_name = selector_expr.field.text();
        if (lhs_td == .user) {
            if (lhs_td.user.scope.get(field_name)) |sym| {
                switch (sym.data) {
                    .def_decl => |def| {
                        selector_expr.type_ref = def.type_ref;
                        return;
                    },
                    .fun_decl => |fun| {
                        selector_expr.type_ref = fun.type_ref;
                        return;
                    },
                    else => unreachable,
                }
            }
        }

        const canon_lhs_td = lhs_td.getUnderlyingType(tc.type_store.*);

        switch (canon_lhs_td) {
            .@"struct" => |st| {
                if (st.get(field_name)) |field| {
                    // Set the overall selector expression to be the
                    selector_expr.type_ref = field.type;
                    return;
                }
            },
            else => {},
        }

        tc.raise(
            selector_expr.field.at(),
            "type {f} has no field or declaration {s}",
            .{
                ty.formatView(tc.type_store, lhs_t),
                field_name,
            },
        );
        selector_expr.type_ref = .dirty;

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

pub fn exitUnaryExpr(tc: *TypeChecker, unary_expr: *node.UnaryExpr) void {
    unary_expr.type_ref = unary_expr.operand.getType().*;

    switch (unary_expr.op.type) {
        .inc, .dec, .minus => {
            const op_text = unary_expr.op.span;
            // These only makes sense on numeric types
            if (!unary_expr.type_ref.isNumeric()) {
                tc.raise(
                    unary_expr.op.offset(tc.code),
                    "unary '{s}' can only be used on numeric types; got type {f}",
                    .{
                        op_text,
                        ty.formatView(tc.type_store, unary_expr.type_ref),
                    },
                );
            }
        },
        .amper => {
            // Add pointer to the type
            unary_expr.type_ref =
                tc.type_store.internDataStable(.{ .ptr = unary_expr.type_ref });
        },
        .star => {
            // Derefernce any pointer
            const td = tc.type_store.get(unary_expr.type_ref);
            if (td != .ptr) {
                tc.raise(
                    unary_expr.op.offset(tc.code),
                    "cannot dereference value of non-pointer type {f}",
                    .{ty.formatView(tc.type_store, unary_expr.type_ref)},
                );
            } else {
                unary_expr.type_ref = td.ptr;
            }
        },
        else => unreachable,
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
        .type_decl => |t| {
            type_ref.* = tc.type_store.internDataStable(.{
                .type_of = tc.type_store.internDataStable(.{
                    .user = t,
                }),
            });
        },
        inline .sub_type => |t| {
            type_ref.* = tc.type_store.intern(&node.Type{ .type_of = .{ .child = &t.type } });
        },
        inline .def_decl, .fun_decl => |foo| {
            if (foo.type_ref == .unset) {
                // This means that we resolved to a defintion which has not
                // been visited yet. This is the case when you do:
                //
                // let x = foo;
                // def foo = 10;
                tc.raise(name.head.position, "undefined here: {s}", .{name.text()});
                type_ref.* = .dirty;
            } else {
                type_ref.* = foo.type_ref;
            }
        },
        inline else => |foo| {
            type_ref.* = foo.type_ref;
        },
    }
}

// TODO: use Ast.ChildDisposition to order tree walk so that type hints
// are set for function parameters and binary operands.

// Add methods for the CallBindings decoration
const CallBindingsOps = struct {
    call_bindings: node.CallBindings,

    pub fn init(al: std.mem.Allocator, store: ty.Store, fun: ty.Fun) CallBindingsOps {
        const sig = fun.signature.get(store);
        var bindings_ = al.alloc(node.CallBindings.ArgBinding, sig.params.len) catch @panic("OOM");
        bindings_.len = sig.params.len;
        for (bindings_, 0..) |*binding, i| {
            binding.* = .{ .name = fun.bindings[i].text() };
        }
        return .{
            .call_bindings = .{
                .bindings = bindings_,
            },
        };
    }

    pub const BindResult = union(enum) {
        failure,
        success: usize,
        already_bound: usize,
    };

    pub fn bind(cb_: *CallBindingsOps, name: []const u8, expr: *node.Expr) BindResult {
        const cb = &cb_.call_bindings;
        for (cb.bindings, 0..) |*b, i| {
            if (std.mem.eql(u8, b.name, name)) {
                if (b.expr != null) {
                    return .{ .already_bound = i };
                }
                b.expr = expr;
                return .{ .success = i };
            }
        }
        return .failure;
    }

    pub fn bindAt(cb_: *CallBindingsOps, index: usize, expr: *node.Expr) BindResult {
        const cb = &cb_.call_bindings;
        if (index >= cb.bindings.len) {
            return .failure;
        }
        if (cb.bindings[index].expr != null) {
            return .{ .already_bound = index };
        }
        cb.bindings[index].expr = expr;
        return .{ .success = index };
    }

    pub fn available(cb_: CallBindingsOps) ?usize {
        const cb = cb_.call_bindings;
        for (cb.bindings, 0..) |b, i| {
            if (b.expr == null) {
                return i;
            }
        }
        return null;
    }

    pub fn bindings(cb: CallBindingsOps) []node.CallBindings.ArgBinding {
        return cb.call_bindings.bindings;
    }
};

pub fn exitCallExpr(tc: *TypeChecker, call: *node.CallExpr) void {
    const tid = call.callable.getType().*;
    if (tid == .dirty) {
        call.type_ref = .dirty;
        return;
    }
    const callable = tc.type_store.get(tid);
    call.type_ref = switch (callable) {
        .fun => |fun| res: {
            const sig = fun.signature.get(tc.type_store.*);
            var cb = CallBindingsOps.init(
                tc.ast.arena.allocator(),
                tc.type_store.*,
                fun,
            );
            defer call.call_bindings = cb.call_bindings;

            var has_error = false;

            for (call.args) |*arg| switch (arg.*) {
                .expr => |*ex| {
                    const i_opt = cb.available();
                    if (i_opt == null) {
                        tc.raise(
                            ex.at(),
                            "extraneous argument",
                            .{},
                        );
                        has_error = true;
                        break;
                    }
                    const i = i_opt.?;
                    const param = sig.params[i];
                    if (param.unpack) {
                        common.todoNoReturn("unpack parameter", .{});
                    }
                    switch (cb.bindAt(i, ex)) {
                        .success => {},
                        .already_bound => |at| {
                            tc.raise(
                                ex.at(),
                                "cannot specify parameter {s} twice",
                                .{cb.bindings()[at].name},
                            );
                            tc.raise(
                                cb.bindings()[at].expr.?.at(),
                                "note: parameter {s} already specified here",
                                .{cb.bindings()[at].name},
                            );
                            has_error = true;
                        },
                        .failure => unreachable,
                    }
                    if (param.type != ex.getType().*) {
                        tc.raise(
                            ex.head().position,
                            "invalid argument type {f}; parameter index {d} expects type {f}",
                            .{
                                ty.formatView(tc.type_store, ex.getType().*),
                                i,
                                ty.formatView(tc.type_store, param.type),
                            },
                        );
                        has_error = true;
                    }
                },
                .unpack => |*un| {
                    const i_opt = cb.available();
                    if (i_opt == null) {
                        tc.raise(
                            un.expr.at(),
                            "extraneous argument",
                            .{},
                        );
                        has_error = true;
                        break;
                    }
                    const i = i_opt.?;
                    const param = sig.params[i];
                    if (!param.unpack) {
                        tc.raise(
                            un.expr.at(),
                            "cannot pass unpacked argument to parameter {s}",
                            .{fun.bindings[i].text()},
                        );
                        has_error = true;
                        continue;
                    }
                    switch (cb.bindAt(i, &un.expr)) {
                        .success => {},
                        .already_bound => |at| {
                            tc.raise(
                                un.expr.at(),
                                "cannot specify parameter {s} twice",
                                .{cb.bindings()[at].name},
                            );
                            tc.raise(
                                cb.bindings()[at].expr.?.at(),
                                "note: parameter {s} already specified here",
                                .{cb.bindings()[at].name},
                            );
                            has_error = true;
                        },
                        .failure => unreachable,
                    }
                    if (param.type != un.expr.getType().*) {
                        tc.raise(
                            un.expr.at(),
                            "invalid argument type {f}; parameter index {d} expects type {f}",
                            .{
                                ty.formatView(tc.type_store, un.expr.getType().*),
                                i,
                                ty.formatView(tc.type_store, param.type),
                            },
                        );
                        has_error = true;
                    }
                },
                .labelled => |*lab| {
                    var param_opt: ?ty.Fun.Param = null;
                    switch (cb.bind(lab.label.text(), &lab.expr)) {
                        .success => |at| {
                            param_opt = sig.params[at];
                        },
                        .already_bound => |at| {
                            tc.raise(
                                lab.head.position,
                                "cannot specify parameter {s} twice",
                                .{cb.bindings()[at].name},
                            );
                            tc.raise(
                                cb.bindings()[at].expr.?.at(),
                                "note: parameter {s} already specified here",
                                .{cb.bindings()[at].name},
                            );
                            has_error = true;
                        },
                        .failure => {
                            tc.raise(
                                lab.head.position,
                                "unknown parameter {s} to function of type: {f}",
                                .{
                                    lab.label.text(),
                                    ty.formatView(tc.type_store, tid),
                                },
                            );
                            has_error = true;
                        },
                    }
                    if (param_opt == null) {
                        continue;
                    }
                    const param = param_opt.?;
                    if (param.type != lab.expr.getType().*) {
                        tc.raise(
                            lab.head.position,
                            "invalid argument type {f}; parameter {s} expects type {f}",
                            .{
                                ty.formatView(tc.type_store, lab.expr.getType().*),
                                lab.label.text(),
                                ty.formatView(tc.type_store, param.type),
                            },
                        );
                        has_error = true;
                    }
                },
                .dirty => {},
            };

            // Now check if the CallBindings have any unbound parameters
            if (!has_error) { // no need to over-report we already have
                // semantic errors so this is less likely to
                // be accurate
                for (cb.bindings(), 0..) |binding, i_| {
                    if (binding.expr == null) {
                        tc.raise(
                            call.head.position,
                            "parameter {s} at index {d} is not passed",
                            .{ binding.name, i_ },
                        );
                    }
                }
            }

            break :res sig.return_type;
        },
        .type_of => |castTo| res: {
            if (!castTo.isBuiltin()) {
                // This means we are trying to call a type: e.g. Point(10, 11)
                // TODO validate the arguments

                // Callable is a user defined type
                // Could be:
                //
                // * Tuple
                // * Sum type
                // * Struct
                // * Primitive type

                // const user_data = tc.type_store.get(castTo);
                // const canon_user_data = user_data.getUnderlyingType(tc.type_store);

                break :res castTo;
            }

            // Cast expression to builtin type: e.g. `u32(10)`
            // First make sure there is only one argument
            var child_type: ?*TypeRef = null;
            if (call.args.len != 1) {
                tc.raise(call.head.position, "cast expression can only take a single argument", .{});
                return;
            } else {
                child_type = switch (call.args[0]) {
                    .expr => |*ex| ex.getType(),
                    .labelled => common.todoNoReturn("labelled args", .{}),
                    .unpack => common.todoNoReturn("unpack args", .{}),
                    .dirty => return,
                };
            }

            tc.tryCastTo(child_type.?, castTo);
            if (child_type.?.* != castTo) {
                tc.raise(
                    call.head.position,
                    "cannot cast type {f} to type {f}",
                    .{
                        ty.formatView(tc.type_store, child_type.?.*),
                        ty.formatView(tc.type_store, castTo),
                    },
                );
            }

            break :res castTo;
        },
        else => {
            common.todoNoReturn("more callables", .{});
        },
    };
}

pub fn enterType(_: *TypeChecker, _: *node.Type) Ast.ChildDisposition {
    return .skip;
}

pub fn exitTypeDecl(tc: *TypeChecker, type_decl: *node.TypeDecl) void {
    type_decl.type_ref = tc.type_store.intern(&type_decl.type);
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
