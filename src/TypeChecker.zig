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
symbol_of_type: std.AutoHashMapUnmanaged(TypeRef, node.Symbol),

pub fn init(ast: *Ast, code: *Code, store: *ty.Store) TypeChecker {
    return .{
        .ast = ast,
        .code = code,
        .type_store = store,
        .arena = ast.ctx.createLifetime(),
        .global_scope = &ast.root.?.scope,
        .scopes = .init(ast.ctx.allocator),
        .symbol_of_type = .empty,
    };
}

pub fn deinit(tc: *TypeChecker) void {
    tc.arena.deinit();
    tc.scopes.deinit();
    tc.symbol_of_type.deinit(tc.ctx().allocator);
}

pub fn enterSourceFile(tc: *TypeChecker, source_file: *node.SourceFile) void {
    tc.push(&source_file.scope);
}

pub fn exitSourceFile(tc: *TypeChecker, source_file: *node.SourceFile) void {
    tc.pop(&source_file.scope);
}

pub fn enterFunDecl(tc: *TypeChecker, fun_decl: *node.FunDecl) void {
    tc.push(fun_decl.x(.scope));

    // Synthesize a function type to be interned
    const fun_type = node.FunType{
        .head = .{ .position = fun_decl.head.position },
        .params = fun_decl.params,
        .return_type = if (fun_decl.return_type) |*ret| ret else null,
        .linkage = fun_decl.linkage,
    };
    const any_type: node.Type = .{ .fun = fun_type };
    fun_decl.symbol.type = tc.type_store.intern(&any_type);

    tc.symbol_of_type.put(tc.ctx().allocator, fun_decl.symbol.type, .fromNode(fun_decl))
        catch @panic("OOM");
}

pub fn exitFunDecl(tc: *TypeChecker, fun_decl: *node.FunDecl) void {
    tc.pop(fun_decl.x(.scope));

    var params: std.ArrayList(node.Symbol) = .empty;
    defer _ = tc.ctx().scratch.reset(.retain_capacity);

    for (fun_decl.params) |*param| {
        params.append(tc.ctx().scratch.allocator(), .fromNode(param)) catch @panic("OOM");
    }

    fun_decl.symbol.params = tc.ast.own(node.Symbol, params.items);
}

pub fn exitFunParam(tc: *TypeChecker, param: *node.FunParam) void {
    param.x(.type).* = tc.type_store.intern(&param.type);
    param.x(.hint).* = param.type.symbol();
    if (param.unpack) {
        const param_td = tc.type_store.get(param.xv(.type)).data.getUnderlyingType(tc.type_store.*);
        switch (param_td) {
            .@"struct", .tuple, .slice => {},
            else => {
                tc.raise(
                    param.head.position,
                    "cannot declare parameter {s} of type {f} as unpack",
                    .{
                        param.name.text(),
                        ty.formatView(tc.type_store, param.xv(.type)),
                    },
                );
            },
        }
    }
}

pub fn exitFunType(tc: *TypeChecker, fun_type: *node.FunType) void {
    const tp: *node.Type = @fieldParentPtr("fun", fun_type);
    tc.symbol_of_type.put(tc.ctx().allocator, tc.type_store.intern(tp), .fromNode(fun_type))
        catch @panic("OOM");
}

pub fn enterCompStmt(tc: *TypeChecker, comp_stmt: *node.CompStmt) void {
    tc.push(&comp_stmt.scope);
}

pub fn exitCompStmt(tc: *TypeChecker, comp_stmt: *node.CompStmt) void {
    tc.pop(&comp_stmt.scope);
}

pub fn exitTypeDecl(tc: *TypeChecker, type_decl: *node.TypeDecl) void {
    type_decl.x(.id).* = tc.type_store.intern(&type_decl.type);
}

pub fn exitStructField(tc: *TypeChecker, struct_field: *node.StructField) void {
    struct_field.x(.type).* = tc.type_store.intern(&struct_field.type);
}

pub fn enterVarDecl(tc: *TypeChecker, var_decl: *node.VarDecl) void {
    if (var_decl.type) |*t| {
        var_decl.x(.type).* = tc.type_store.intern(t);
    }
    if (var_decl.init_expr) |*init_expr| {
        if (var_decl.type) |*tp| {
            tc.hintType(tp.symbol(), init_expr);
        }
    }
}

pub fn exitVarDecl(tc: *TypeChecker, var_decl: *node.VarDecl) void {
    if (var_decl.init_expr) |*init_expr| {
        if (var_decl.type != null) {
            tc.tryCastTo(init_expr.getType(), var_decl.xv(.type));
            const rhs_type = init_expr.getType().*;

            if (var_decl.xv(.type) != rhs_type) {
                tc.raise(var_decl.type.?.head().position, "declared type {f} differs from right hand-side {f}", .{
                    ty.formatView(tc.type_store, var_decl.xv(.type)),
                    ty.formatView(tc.type_store, rhs_type),
                });
            }
        }
        var_decl.x(.type).* = init_expr.getType().*;
    }

    if (var_decl.xv(.type) == .unit) {
        const pos = if (var_decl.type != null)
            var_decl.type.?.head().position
        else
            var_decl.name.head.position;
        tc.raise(pos, "variable cannot have type unit", .{});
    }
}

pub fn enterDefDecl(tc: *TypeChecker, def_decl: *node.DefDecl) void {
    if (def_decl.type) |*t| {
        def_decl.x(.type).* = tc.type_store.intern(t);
    }
}

pub fn exitDefDecl(tc: *TypeChecker, def_decl: *node.DefDecl) void {
    if (def_decl.type != null) {
        tc.tryCastTo(def_decl.init_expr.getType(), def_decl.xv(.type));
        const rhs_type = def_decl.init_expr.getType().*;

        if (def_decl.xv(.type) != rhs_type) {
            tc.raise(def_decl.type.?.head().position, "declared type {f} differs from right hand-side {f}", .{
                ty.formatView(tc.type_store, def_decl.xv(.type)),
                ty.formatView(tc.type_store, rhs_type),
            });
        }
    }
    def_decl.x(.type).* = def_decl.init_expr.getType().*;

    if (def_decl.xv(.type) == .unit) {
        const pos = if (def_decl.type != null)
            def_decl.type.?.head().position
        else
            def_decl.name.head.position;
        tc.raise(pos, "definition cannot have type unit", .{});
    }
}

fn tryCastTo(tc: *TypeChecker, from_type: *TypeRef, to_type: TypeRef) void {
    const from_ti = tc.type_store.get(from_type.*);
    const to_ti = tc.type_store.get(to_type);
    if (from_ti.isWeak() and to_ti.isLinear()) {
        return;
    }

    const from_und = tc.type_store.getBaseRef(from_type.*);
    const to_und = tc.type_store.getBaseRef(to_type);

    if (from_und == to_und) {
        from_type.* = to_type;
        return;
    }

    switch (from_und) {
        .f32 => switch (to_und) {
            .f32, .f64 => from_type.* = to_type,
            else => {},
        },
        .f64 => switch (to_und) {
            .f64 => from_type.* = to_type,
            else => {},
        },
        .s32 => switch (to_und) {
            .s64, .f32, .f64 => from_type.* = to_type,
            else => {},
        },
        .u32 => switch (to_und) {
            .s64, .u64, .f64 => from_type.* = to_type,
            else => {},
        },
        else => {},
    }
}

pub fn exitTypeExpr(tc: *TypeChecker, type_expr: *node.TypeExpr) void {
    type_expr.type_ref = tc.type_store.internInfoStable(.fromData(.{
        .type_of = tc.type_store.intern(type_expr.type),
    }));
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
            break :type_ tc.type_store.internInfoStable(.fromData(.{ .type_of = tu.toBuiltinType(token.type) }));
        } else return,
    };
}

pub fn exitIdentExpr(tc: *TypeChecker, ident_expr: *node.IdentExpr) void {
    if (ident_expr.is_inferred and ident_expr.hint != null) {
        var hint = ident_expr.hint;
        while (hint) |sym| {
            switch (sym.data) {
                .type => |tp| {
                    if (tp.scope.get(ident_expr.name.text())) |s| {
                        ident_expr.type_ref = s.typeRef();
                        ident_expr.resolves_to = s;
                        return;
                    }
                    if (tp.underlying_type != null) {
                        hint = tp.underlying_type;
                        continue;
                    }
                },
                else => {},
            }
            hint = null;
        }
        ident_expr.type_ref = ident_expr.hint.?.data.type.id;
        tc.raise(
            ident_expr.head.position,
            "undefined {s} of hinted: {f}",
            .{ ident_expr.name.text(), ty.formatView(tc.type_store, ident_expr.type_ref) },
        );
        return;
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

        const lhs_td = tc.type_store.get(lhs_t).data;
        // First see if the selector is accessing a declaration of user
        // defined type
        const field_name = selector_expr.field.text();
        if (lhs_td == .user) {
            if (lhs_td.user.type.scope.get(field_name)) |sym| {
                switch (sym.data) {
                    .@"var" => |v| {
                        selector_expr.type_ref = v.type;
                        return;
                    },
                    .fun => |fun| {
                        selector_expr.type_ref = fun.type;
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
                tc.type_store.internInfoStable(.fromData(.{ .ptr = unary_expr.type_ref }));
        },
        .star => {
            // Derefernce any pointer
            const td = tc.type_store.get(unary_expr.type_ref).data;
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
        .field => |f| {
            tc.raise(name.head.position, "cannot reference struct field {s} in this context", .{name.text()});
            // Not sure how you would raise this error, however I think it
            // is sane to set the type of the reference to be the type of
            // the field itself
            type_ref.* = f.type;
        },
        .enumerator => |en| {
            type_ref.* = en.enclosed_by.typeRef();
        },
        .type => |t| {
            type_ref.* = tc.type_store.internInfoStable(.fromData(.{
                .type_of = tc.type_store.internInfoStable(.fromData(.{
                    .user = .{
                        .name = symbol.name,
                        .source = symbol.source,
                        .type = t,
                    },
                })),
            }));
        },
        .fun => |f| {
            if (f.type == .unset) {
                // This means that we resolved to a defintion which has not
                // been visited yet. This is the case when you do:
                //
                // let x = foo;
                // def foo = 10;
                tc.raise(name.head.position, "undefined here: {s}", .{name.text()});
                type_ref.* = .dirty;
            } else {
                type_ref.* = f.type;
            }
        },
        .@"var" => |v| {
            if (v.kind == .def and v.type == .unset) {
                // This means that we resolved to a defintion which has not
                // been visited yet. This is the case when you do:
                //
                // let x = foo;
                // def foo = 10;
                tc.raise(name.head.position, "undefined here: {s}", .{name.text()});
                type_ref.* = .dirty;
            } else {
                type_ref.* = v.type;
            }
        },
    }
}

// TODO: use Ast.ChildDisposition to order tree walk so that type hints
// are set for function parameters and binary operands.

// Add methods for the CallBindings decoration
// const CallBindingsOps = struct {
//     call_bindings: node.CallBindings,
//
//     pub fn init(al: std.mem.Allocator, sig: ty.Fun.Signature, param_bindings: []const node.Ident) CallBindingsOps {
//         std.debug.assert(sig.params.len != 0);
//         var bindings_ = al.alloc(node.CallBindings.ArgBinding, sig.params.len) catch @panic("OOM");
//         for (0..sig.params.len) |i| {
//             bindings_[i] = .{ .name = param_bindings[i].text() };
//         }
//         return .{
//             .call_bindings = .{
//                 .bindings = bindings_,
//             },
//         };
//     }
//
//     pub fn fromFun(al: std.mem.Allocator, store: ty.Store, fun: ty.Fun) CallBindingsOps {
//         const sig = fun.signature.get(store);
//         return .init(al, sig, fun.bindings);
//     }
//
//     pub const BindResult = union(enum) {
//         failure,
//         success: usize,
//         already_bound: usize,
//     };
//
//     pub fn bind(cb_: *CallBindingsOps, name: []const u8, expr: *node.Expr) BindResult {
//         const cb = &cb_.call_bindings;
//         for (cb.bindings, 0..) |*b, i| {
//             if (std.mem.eql(u8, b.name, name)) {
//                 if (b.expr != null) {
//                     return .{ .already_bound = i };
//                 }
//                 b.expr = expr;
//                 return .{ .success = i };
//             }
//         }
//         return .failure;
//     }
//
//     pub fn bindAt(cb_: *CallBindingsOps, index: usize, expr: *node.Expr) BindResult {
//         const cb = &cb_.call_bindings;
//         if (index >= cb.bindings.len) {
//             return .failure;
//         }
//         if (cb.bindings[index].expr != null) {
//             return .{ .already_bound = index };
//         }
//         cb.bindings[index].expr = expr;
//         return .{ .success = index };
//     }
//
//     pub fn available(cb_: CallBindingsOps) ?usize {
//         const cb = cb_.call_bindings;
//         for (cb.bindings, 0..) |b, i| {
//             if (b.expr == null) {
//                 return i;
//             }
//         }
//         return null;
//     }
//
//     pub fn bindings(cb: CallBindingsOps) []node.CallBindings.ArgBinding {
//         return cb.call_bindings.bindings;
//     }
// };
//
fn synthParamBindingsFromStruct(al: std.mem.Allocator, st: ty.Data.StructType) []node.Ident {
    var st_param_bindings = al.alloc(node.Ident, st.fields.len) catch @panic("OOM");
    st_param_bindings.len = st.fields.len;

    for (st.fields, 0..) |f, fi| {
        // Synthesize identifier
        st_param_bindings[fi] = .{
            .token = .{
                .type = .ident,
                .span = f.name,
            },
        };
    }

    return st_param_bindings;
}

fn synthParamBindingsFromTuple(ast: *Ast, al: std.mem.Allocator, tup: ty.Data.TupleType) []node.Ident {
    var tup_param_bindings = al.alloc(node.Ident, tup.types.len) catch @panic("OOM");
    tup_param_bindings.len = tup.types.len;

    for (0..tup.types.len) |ti| {
        // Synthesize identifier
        tup_param_bindings[ti] = .{
            .token = .{
                .type = .ident,
                .span = ast.num(ti),
            },
        };
    }

    return tup_param_bindings;
}

fn synthSigFromStruct(al: std.mem.Allocator, ret_t: TypeRef, st: ty.Data.StructType) ty.Fun.Signature {
    var st_params = al.alloc(ty.Fun.Param, st.fields.len) catch @panic("OOM");
    st_params.len = st.fields.len;

    for (st.fields, 0..) |f, fi| {
        st_params[fi] = .{
            .type = f.type,
            .unpack = false,
        };
    }

    return .{
        .params = st_params,
        .return_type = ret_t,
    };
}

fn synthSigFromTuple(al: std.mem.Allocator, ret_t: TypeRef, tup: ty.Data.TupleType) ty.Fun.Signature {
    var tup_params = al.alloc(ty.Fun.Param, tup.types.len) catch @panic("OOM");
    tup_params.len = tup.types.len;

    for (tup.types, 0..) |t, ti| {
        tup_params[ti] = .{
            .type = t,
            .unpack = false,
        };
    }

    return .{
        .params = tup_params,
        .return_type = ret_t,
    };
}

fn bindCall(tc: *TypeChecker, callable_t: TypeRef, call: anytype) TypeRef {
    const tp = tc.type_store.get(callable_t);

    if (tp.data == .type_of) {
        const cast_to = tp.data.type_of;
        const cast_to_info = tc.type_store.get(cast_to);

        // See if it is a cast
        out: switch (cast_to_info.data) {
            .user => |user| {
                if (call.args.len != 1) {
                    break :out;
                }
                // See if it is a cast from underlying type
                if (user.type.underlying_type) |und| {
                    switch (call.args[0]) {
                        .expr => |*ex| if (ex.getTypeConst().* == und.data.type.id) {
                            const bindings = tc.argBindingsOfSize(1);
                            bindings[0].expr = ex;
                            call.call_bindings = .{ .bindings = bindings, .kind = .cast };
                            return user.type.id;
                        },
                        else => {},
                    }
                }
            },
            .primitive => {
                continue :out .{ .builtin = cast_to };
            },
            .builtin => |builtin| {
                // Calling a builtin is always a cast so we can just
                // bind the first arg
                const bindings = tc.argBindingsOfSize(1);
                if (call.args.len >= 1) {
                    switch (call.args[0]) {
                        .expr => |*ex| {
                            bindings[0].expr = ex;
                        },
                        else => break :out,
                    }
                }
                call.call_bindings = .{ .bindings = bindings, .kind = .cast };
                return builtin;
            },
            else => {},
        }
    }

    // Call is not a cast, it now must be a function call or type literal
    const fun_symbol_opt = tc.getCallableSymbol(callable_t);
    if (fun_symbol_opt == null) {
        tc.raise(call.head.position, "type {f} of left-hand side is not callable", .{
            ty.formatView(tc.type_store, callable_t),
        });
        return .dirty;
    }
    const fun_symbol = fun_symbol_opt.?.data.fun;

    const bindings = tc.argBindingsOfFun(fun_symbol);
    const main_binder: ArgBinder = .init(bindings);

    var binder = main_binder;
    var state: union(enum) {
        top_level,
        unpack: usize,
    } = .top_level;

    for (call.args, 0..) |*arg, i| {
        again: switch (arg.*) {
            .unpack => |*unpack| {
                if (state == .unpack) {
                    // Heuristic, leave unpack state if encounter unpack arg
                    // as this would not ever be valid for argument to
                    // literal
                    const first = state.unpack;
                    const args = call.args[first..i];
                    const unpack_bindings = binder.bindings;

                    state = .top_level;
                    binder = main_binder;

                    const unpack_call: node.AnonCallExpr = .{
                        .head = .{ .position = call.args[first].at(), .flags = .initOne(.fake) },
                        .args = args,
                        .call_bindings = .{ .bindings = unpack_bindings },
                        .type_ref = binder.nextParam(fun_symbol.params).?.data.@"var".type,
                    };

                    const expr = tc.ast.box(node.Expr{ .anon_call = unpack_call });
                    const info = binder.bindNext(expr);
                    tc.handleBind(expr, unpack_bindings, info);

                    continue :again arg.*;
                }
                const info = binder.bindNext(&unpack.expr);
                tc.handleBind(&unpack.expr, binder.bindings, info);
                if (info == .bound) {
                    const param = fun_symbol.params[info.bound].data.@"var";
                    if (!param.flags.contains(.unpack)) {
                        tc.raise(
                            unpack.head.position,
                            "cannot specify '..'; parameter {s} is not a pack",
                            .{ binder.bindings[info.bound].name },
                        );
                    }
                }
            },
            .labelled => |*labelled| {
                if (binder.nextParam(fun_symbol.params)) |next| {
                    if (next.data.@"var".flags.contains(.unpack)) {
                        const unpack_callable = if (next.data.type.underlying_type) |underlying_type|
                            underlying_type.data.type.id
                        else next.data.type.id;
                        if (tc.getCallableSymbol(unpack_callable)) |unpack_symbol| {
                            state = .{ .unpack = i };
                            const unpack_fun = unpack_symbol.data.fun;
                            const unpack_bindings = tc.argBindingsOfFun(unpack_fun);
                            binder = .init(unpack_bindings);
                            // Continue with the same argument, just different
                            // state
                            continue :again arg.*;
                        } // no need to error as if the pack param is not
                          // callable it will be an error at declaration
                    }
                }
                const label = labelled.label.text();
                const info = binder.bind(label, &labelled.expr);
                tc.handleBind(&labelled.expr, binder.bindings, info);
            },
            .expr => |*expr| {
                if (binder.nextParam(fun_symbol.params)) |next| {
                    if (next.data.@"var".flags.contains(.unpack)) {
                        const unpack_callable = if (next.data.type.underlying_type) |underlying_type|
                            underlying_type.data.type.id
                        else next.data.type.id;
                        if (tc.getCallableSymbol(unpack_callable)) |unpack_symbol| {
                            state = .{ .unpack = i };
                            const unpack_fun = unpack_symbol.data.fun;
                            const unpack_bindings = tc.argBindingsOfFun(unpack_fun);
                            binder = .init(unpack_bindings);
                            // Continue with the same argument, just different
                            // state
                            continue :again arg.*;
                        } // no need to error as if the pack param is not
                          // callable it will be an error at declaration
                    }
                }
                const info = binder.bindNext(expr);
                tc.handleBind(expr, binder.bindings, info);
            },
            .dirty => {},
        }
        if (binder.done() and state == .unpack) {
            const first = state.unpack;
            const args = call.args[first..i];
            const unpack_bindings = binder.bindings;

            state = .top_level;
            binder = main_binder;

            const unpack_call: node.AnonCallExpr = .{
                .head = .{ .position = call.args[first].at(), .flags = .initOne(.fake) },
                .args = args,
                .call_bindings = .{ .bindings = unpack_bindings },
                .type_ref = binder.nextParam(fun_symbol.params).?.data.@"var".type,
            };

            const expr = tc.ast.box(node.Expr{ .anon_call = unpack_call });
            const info = binder.bindNext(expr);
            tc.handleBind(expr, unpack_bindings, info);
        }
    }

    if (state == .unpack) {
        const first = state.unpack;
        const args = call.args[first..];
        const unpack_bindings = binder.bindings;

        state = .top_level;
        binder = main_binder;

        const unpack_call: node.AnonCallExpr = .{
            .head = .{ .position = call.args[first].at(), .flags = .initOne(.fake) },
            .args = args,
            .call_bindings = .{ .bindings = unpack_bindings },
            .type_ref = binder.nextParam(fun_symbol.params).?.data.@"var".type,
        };

        const expr = tc.ast.box(node.Expr{ .anon_call = unpack_call });
        const info = binder.bindNext(expr);
        tc.handleBind(expr, unpack_bindings, info);
    }

    call.call_bindings = .{ .bindings = binder.bindings };

    return tc.type_store.get(fun_symbol.type).data.fun.signature.get(tc.type_store.*).return_type;
}

fn argBindingsOfFun(tc: *TypeChecker, fun: *node.Symbol.Fun)  []node.CallBindings.ArgBinding {
    const al = tc.ast.arena.allocator();
    const args = al.alloc(node.CallBindings.ArgBinding, fun.params.len)
        catch @panic("OOM");
    for (fun.params, 0..) |symbol, i| {
        args[i] = .{
            .name = symbol.name,
            .expr = null,
        };
    }
    return args;
}

fn argBindingsOfSize(tc: *TypeChecker, len: usize) []node.CallBindings.ArgBinding {
    const al = tc.ast.arena.allocator();
    const args = al.alloc(node.CallBindings.ArgBinding, len)
        catch @panic("OOM");
    for (0..len) |i| {
        args[i] = .{
            .name = tc.ast.num(i),
            .expr = null,
        };
    }
    return args;
}

fn getCallableTarget(tc: *TypeChecker, callable_t: TypeRef) ?TypeRef {
    const callable_info = tc.type_store.get(callable_t);
    switch (callable_info.data) {
        .fun => return callable_t,
        .type_of => |lit_type| {
            const lit_type_info = tc.type_store.get(lit_type);
            switch (lit_type_info.data) {
                .user => |*user| {
                    if (user.type.underlying_type) |underlying_type| {
                        return
                            underlying_type.data.type.id;
                    }
                },
                else => {},
            }
        },
        else => {},
    }
    return null;
}

fn getCallableSymbol(tc: *TypeChecker, callable_t: TypeRef) ?node.Symbol {
    if (tc.getCallableTarget(callable_t)) |target| {
        return tc.symbol_of_type.get(target);
    }
    return null;
}

fn getCallableType(tc: *TypeChecker, callable_t: TypeRef) ?ty.Fun {
    const type_info = tc.type_store.get(tc.getCallableTarget(callable_t) orelse return null);
    return type_info.data.fun;
}

fn handleBind(tc: *TypeChecker, ex: *node.Expr, bindings: []node.CallBindings.ArgBinding, info: ArgBinder.BindingInfo) void {
    switch (info) {
        .bound => {},
        .not_bound => {
            tc.raise(ex.at(), "extraneous argument", .{});
        },
        .already_bound => |at| {
            tc.raise(
                ex.at(),
                "cannot specify argument {s} more than once",
                .{ bindings[at].name },
            );
            tc.raise(
                bindings[at].expr.?.at(),
                "note: argument {s} first bound here",
                .{ bindings[at].name },
            );
        },
    }
}

const ArgBinder = struct {
    next: usize = 0,
    bindings: []node.CallBindings.ArgBinding,

    pub fn init(bindings: []node.CallBindings.ArgBinding) ArgBinder {
        return .{
            .bindings = bindings,
        };
    }

    fn bindAt(ab: *ArgBinder, i: usize, arg: *node.Expr) BindingInfo {
        const binding = &ab.bindings[i];
        if (binding.expr) |_| {
            return .{ .already_bound = i };
        }
        binding.expr = arg;
        return .{ .bound = i };
    }

    pub fn nextParam(ab: *const ArgBinder, params: []node.Symbol) ?node.Symbol {
        if (ab.next < params.len) {
            return params[ab.next];
        }
        return null;
    }

    pub fn bind(ab: *ArgBinder, name: []const u8, arg: *node.Expr) BindingInfo {
        for (ab.bindings, 0..) |*binding, i| {
            if (std.mem.eql(u8, binding.name, name)) {
                return ab.bindAt(i, arg);
            }
        }
        return .not_bound;
    }

    pub fn bindNext(ab: *ArgBinder, arg: *node.Expr) BindingInfo {
        if (ab.next >= ab.bindings.len) {
            return .not_bound;
        }
        const res = ab.bindAt(ab.next, arg);
        ab.next += 1;
        var next = ab.next + 1;
        while (next < ab.bindings.len and ab.bindings[ab.next].expr != null) {
            next += 1;
        }
        if (next < ab.bindings.len) {
            ab.next = next;
        }
        return res;
    }

    pub fn done(ab: *const ArgBinder) bool {
        return ab.next >= ab.bindings.len;
    }

    pub const BindingInfo = union(enum) {
        already_bound: usize,
        bound: usize,
        not_bound,
    };
}; 

// const CallArgBinder = struct {
//     tc: *TypeChecker,
//     sig: *node.Symbol.Fun,
//     callable_t: TypeRef,
//     param_bindings: []const node.Ident,
//     bind_ops: CallBindingsOps,
//     item_desc: []const u8 = "parameter",
//
//     fn init(tc: *TypeChecker, item_name: []const u8, callable_t: TypeRef) CallArgBinder {
//         const fun = tc.type_store.get(callable_t).data.fun;
//         const sig = fun.signature.ref;
//         return .{
//             .tc = tc,
//             .sig = (tc.symbol_of_type.get(sig) orelse @panic("no signature to symbol mapping")).data.fun,
//             .callable_t = callable_t,
//             .param_bindings = fun.bindings,
//             .bind_ops = CallBindingsOps.init(
//                 tc.ast.arena.allocator(),
//                 fun.signature.get(tc.type_store.*),
//                 fun.bindings,
//             ),
//             .item_desc = item_name,
//         };
//     }
//
//     fn fromFun(tc: *TypeChecker, fun_t: ty.TypeRefStrict(ty.Fun)) CallArgBinder {
//         const fun = fun_t.get(tc.type_store.*);
//         const sig = fun.signature.ref;
//         return .{
//             .tc = tc,
//             .sig = (tc.symbol_of_type.get(sig) orelse @panic("no signature to symbol mapping")).data.fun,
//             .callable_t = fun_t.ref,
//             .param_bindings = fun.bindings,
//             .bind_ops = CallBindingsOps.fromFun(
//                 tc.ast.arena.allocator(),
//                 tc.type_store.*,
//                 fun,
//             ),
//         };
//     }
//
//     fn bind(c: *CallArgBinder, call_at: Code.Offset, args: []node.CallExprArg) node.CallBindings {
//         var has_error = false;
//         const cb = &c.bind_ops;
//         const tc = c.tc;
//         const sig = c.sig;
//
//         var arg_i: usize = 0;
//         while (arg_i < args.len) : (arg_i += 1) {
//             const arg = &args[arg_i];
//             const i_opt = cb.available();
//             if (i_opt == null) {
//                 tc.raise(
//                     arg.at(),
//                     "extraneous argument",
//                     .{},
//                 );
//                 has_error = true;
//                 break;
//             }
//             const i = i_opt.?;
//             const param = sig.params[i].data.@"var";
//             std.debug.assert(param.kind == .param);
//             const arg_unpack = switch (arg.*) {
//                 .unpack => true,
//                 .labelled => |lab| lab.unpack,
//                 else => false,
//             };
//             const hint = param.hint.?.data.type;
//             if (param.flags.contains(.unpack) and !arg_unpack) {
//                 const param_td = tc.type_store
//                     .get(param.type).data
//                     .getUnderlyingType(tc.type_store.*);
//                 switch (param_td) {
//                     .@"struct" => |st| {
//                         var sub_binder = CallArgBinder.init(tc, "field", hint.id);
//
//                         const bound = std.math.clamp(arg_i + st.fields.len, 0, args.len);
//                         const sub_args = args[arg_i..bound];
//
//                         // We also need to synthesize a call expression
//                         // to bind to the parameter - the call expression
//                         // will store the binding result of the sub check
//                         const fake_token = node.TokenExpr{
//                             .token = .{
//                                 .type = .fake,
//                                 .span = "<fake>",
//                             },
//                             .type_ref = tc.type_store.internInfoStable(.fromData(.{
//                                 .type_of = param.type,
//                             })),
//                         };
//
//                         const fake_call = node.CallExpr{
//                             .head = .{
//                                 .flags = .init(.{ .fake = true }),
//                                 .position = sub_args[0].at(),
//                             },
//                             .args = sub_args,
//                             .callable = tc.ast.box(node.Expr{ .token_expr = fake_token }),
//                             .type_ref = param.type,
//                         };
//
//                         var fake_expr = tc.ast.box(node.Expr{ .call = fake_call });
//
//                         // TODO(default values): if we get an invalid type argument in
//                         // the sub check, it should only be invalid
//                         // if it would not also match the type of the
//                         // parameter after the packed parameter.
//                         //
//                         // This means for example if you have:
//                         //
//                         // type Foo = struct { x: u32 = 11, y: u32 };
//                         //
//                         // fun func(s: str, ..f: Foo, g: str) extern;
//                         //
//                         // ...
//                         // func("xx", y: 12, "")
//                         //
//                         // This should be valid by just assuming that
//                         // 'x' takes on it's default value
//                         fake_expr.call.call_bindings = sub_binder.bind(arg.at(), sub_args);
//
//                         std.debug.assert(cb.bindAt(i, fake_expr) == .success);
//
//                         arg_i = bound - 1;
//                         continue;
//                     },
//                     .tuple => |tup| {
//                         var sub_binder = CallArgBinder.init(tc, "field", hint.id);
//
//                         const bound = std.math.clamp(arg_i + tup.types.len, 0, args.len);
//                         const sub_args = args[arg_i..bound];
//
//                         // We also need to synthesize a call expression
//                         // to bind to the parameter - the call expression
//                         // will store the binding result of the sub check
//                         const fake_token = node.TokenExpr{
//                             .token = .{
//                                 .type = .fake,
//                                 .span = "<fake>",
//                             },
//                             .type_ref = tc.type_store.internInfoStable(.fromData(.{
//                                 .type_of = param.type,
//                             })),
//                         };
//
//                         const fake_call = node.CallExpr{
//                             .args = sub_args,
//                             .callable = tc.ast.box(node.Expr{ .token_expr = fake_token }),
//                             .type_ref = param.type,
//                         };
//
//                         var fake_expr = tc.ast.box(node.Expr{ .call = fake_call });
//                         fake_expr.call.call_bindings = sub_binder.bind(arg.at(), sub_args);
//
//                         std.debug.assert(cb.bindAt(i, fake_expr) == .success);
//
//                         arg_i = bound - 1;
//                         continue;
//                     },
//                     else => unreachable,
//                 }
//             }
//             switch (arg.*) {
//                 .expr => |*ex| {
//                     switch (cb.bindAt(i, ex)) {
//                         .success => {},
//                         .already_bound => |at| {
//                             tc.raise(
//                                 ex.at(),
//                                 "cannot specify {s} {s} twice",
//                                 .{ c.item_desc, cb.bindings()[at].name },
//                             );
//                             tc.raise(
//                                 cb.bindings()[at].expr.?.at(),
//                                 "note: {s} {s} already specified here",
//                                 .{ c.item_desc, cb.bindings()[at].name },
//                             );
//                             has_error = true;
//                         },
//                         .failure => unreachable,
//                     }
//                     tc.hintType(param.hint.?, ex);
//                 },
//                 .unpack => |*un| {
//                     if (!param.flags.contains(.unpack)) {
//                         tc.raise(
//                             un.expr.at(),
//                             "cannot pass unpacked argument to {s} {s}",
//                             .{ c.item_desc, c.param_bindings[i].text() },
//                         );
//                         has_error = true;
//                         continue;
//                     }
//                     switch (cb.bindAt(i, &un.expr)) {
//                         .success => {},
//                         .already_bound => |at| {
//                             tc.raise(
//                                 un.expr.at(),
//                                 "cannot specify {s} {s} twice",
//                                 .{ c.item_desc, cb.bindings()[at].name },
//                             );
//                             tc.raise(
//                                 cb.bindings()[at].expr.?.at(),
//                                 "note: {s} {s} already specified here",
//                                 .{ c.item_desc, cb.bindings()[at].name },
//                             );
//                             has_error = true;
//                         },
//                         .failure => unreachable,
//                     }
//                     tc.hintType(param.hint.?, &un.expr);
//                 },
//                 .labelled => |*lab| {
//                     var param_opt: ?node.Symbol = null;
//                     switch (cb.bind(lab.label.text(), &lab.expr)) {
//                         .success => |at| {
//                             param_opt = sig.params[at];
//                         },
//                         .already_bound => |at| {
//                             tc.raise(
//                                 lab.head.position,
//                                 "cannot specify {s} {s} twice",
//                                 .{ c.item_desc, cb.bindings()[at].name },
//                             );
//                             tc.raise(
//                                 cb.bindings()[at].expr.?.at(),
//                                 "note: {s} {s} is already specified here",
//                                 .{ c.item_desc, cb.bindings()[at].name },
//                             );
//                             has_error = true;
//                         },
//                         .failure => {
//                             tc.raise(
//                                 lab.head.position,
//                                 "unknown {s} {s} of '{f}'",
//                                 .{
//                                     c.item_desc,
//                                     lab.label.text(),
//                                     ty.formatView(tc.type_store, c.callable_t),
//                                 },
//                             );
//                             has_error = true;
//                         },
//                     }
//                     if (param_opt == null) {
//                         continue;
//                     }
//                     const param_ = param_opt.?;
//                     tc.hintType(param_.data.@"var".hint.?, &lab.expr);
//                 },
//                 .dirty => {},
//             }
//         }
//
//         // Now check if the CallBindings have any unbound parameters
//         if (!has_error) { // no need to over-report we already have
//             // semantic errors so this is less likely to
//             // be accurate
//             for (cb.bindings()) |binding| {
//                 if (binding.expr == null) {
//                     tc.raise(
//                         call_at,
//                         "{s} {s} of '{f}' is not specified",
//                         .{
//                             c.item_desc,
//                             binding.name,
//                             ty.formatView(tc.type_store, c.callable_t),
//                         },
//                     );
//                 }
//             }
//         }
//
//         return c.bind_ops.call_bindings;
//     }
// };

// fn bindCall(tc: *TypeChecker, call_t: TypeRef, call: anytype) TypeRef {
//     const callable = tc.type_store.get(call_t).data;
//     return switch (callable) {
//         .fun => res: {
//             var binder = CallArgBinder.fromFun(tc, .{ .ref = call_t });
//             defer _ = tc.ctx().scratch.reset(.retain_capacity);
//             call.call_bindings = binder.bind(call.head.position, call.args);
//             const ret_t = tc.type_store.get(binder.sig.type).data.fun.signature.get(tc.type_store.*).return_type;
//             break :res ret_t;
//         },
//         .type_of => |cast_to| res: {
//             if (call.args.len == 1) {
//                 const arg_type = switch (call.args[0]) {
//                     .unpack => |un| un.expr.getTypeConst().*,
//                     .labelled => |la| la.expr.getTypeConst().*,
//                     .expr => |ex| ex.getTypeConst().*,
//                     .dirty => unreachable,
//                 };
//                 if (arg_type == cast_to) {
//                 }
//             }
//
//             if (!cast_to.isBuiltin()) {
//                 // This means we are trying to call a type: e.g. Point(10, 11)
//                 // TODO validate the arguments
//
//                 // Callable is a user defined type
//                 // Could be:
//                 //
//                 // * Tuple
//                 // * Sum type
//                 // * Struct
//                 // * Primitive type
//
//                 const user_data = tc.type_store.get(cast_to).data;
//                 const canon_user_data = user_data.getUnderlyingType(tc.type_store.*);
//
//                 switch (canon_user_data) {
//                     .@"struct" => {
//                         var binder = CallArgBinder.init(tc, "field", cast_to);
//
//                         call.call_bindings = binder.bind(call.head.position, call.args);
//                     },
//                     .tuple => {
//                         var binder = CallArgBinder.init(tc, "field", cast_to);
//                         call.call_bindings = binder.bind(call.head.position, call.args);
//                     },
//                     // .builtin => {
//                     //     const builtin_param_bindings: []const node.Ident = &.{
//                     //         .{
//                     //             .token = .{
//                     //                 .type = .ident,
//                     //                 .span = tc.ast.num(0),
//                     //             },
//                     //         },
//                     //     };
//                     //     const builtin_sig: ty.Fun.Signature = .initCast(cast_to);
//                     //     var binder = CallArgBinder.init(tc, "cast operand", cast_to, builtin_sig, builtin_param_bindings);
//                     //     call.call_bindings = binder.bind(call.head.position, call.args);
//                     // },
//                     else => {},
//                 }
//
//                 break :res cast_to;
//             }
//
//             // Cast expression to builtin type: e.g. `u32(10)`
//             // First make sure there is only one argument
//             var child_type: ?*TypeRef = null;
//             if (call.args.len != 1) {
//                 tc.raise(call.head.position, "cast expression can only take a single argument", .{});
//                 return .dirty;
//             } else {
//                 child_type = switch (call.args[0]) {
//                     .expr => |*ex| ex.getType(),
//                     .labelled => common.todoNoReturn("labelled args", .{}),
//                     .unpack => common.todoNoReturn("unpack args", .{}),
//                     .dirty => return .dirty,
//                 };
//             }
//
//             const builtin_param_bindings: []const node.Ident = &.{
//                 .{
//                     .token = .{
//                         .type = .ident,
//                         .span = tc.ast.num(0),
//                     },
//                 },
//             };
//             const builtin_sig: ty.Fun.Signature = .initCast(cast_to);
//             var binder = CallArgBinder.init(tc, "cast operand", cast_to, builtin_sig, builtin_param_bindings);
//             call.call_bindings = binder.bind(call.head.position, call.args);
//
//             break :res cast_to;
//         },
//         else => {
//             common.todoNoReturn("more callables: {any}", .{callable});
//         },
//     };
// }

// By default DFS of a call expression is not ideal for type hinting as
// when you enter the call expression you won't have resolved the lhs (callable)
// to be able to have the type information to hint the arguments.
//
// This is why on entry to call expressions we just do a more fine grained
// order of traversing the call expression so that we first resolve the
// callable, then bind it which will push down hints. After this we walk all
// the arguments and they should now be correctly hinted.

pub fn enterCallExpr(tc: *TypeChecker, call: *node.CallExpr) Ast.ChildDisposition {
    Ast.walk(tc, call.callable);
    const tid = call.callable.getType().*;
    if (tid == .dirty) {
        call.type_ref = .dirty;
        return .skip;
    }
    call.type_ref = tc.bindCall(tid, call);
    for (call.args) |*arg| {
        Ast.walk(tc, arg);
    }
    return .skip;
}

// Needs to be inline otherwise I get valgrind errors :O
// inline fn getCallableInfo(tc: *TypeChecker, callable_t: TypeRef) CallableInfo {
//     const callable = tc.type_store.get(callable_t).data;
//     again: switch (callable) {
//         .fun => |fun| {
//             return .{
//                 .item_desc = "parameter",
//                 .param_bindings = fun.bindings,
//                 .signature = fun.signature.get(tc.type_store.*),
//             };
//         },
//         .@"struct" => |st| {
//             const al = tc.ctx().scratch.allocator();
//
//             const st_sig = synthSigFromStruct(al, callable_t, st);
//             const st_param_bindings = synthParamBindingsFromStruct(al, st);
//
//             return .{
//                 .item_desc = "field",
//                 .signature = st_sig,
//                 .param_bindings = st_param_bindings,
//             };
//         },
//         .tuple => |tup| {
//             const al = tc.ctx().scratch.allocator();
//
//             const tup_sig = synthSigFromTuple(al, callable_t, tup);
//             const tup_param_bindings = synthParamBindingsFromTuple(tc.ast, al, tup);
//
//             return .{
//                 .item_desc = "field",
//                 .signature = tup_sig,
//                 .param_bindings = tup_param_bindings,
//             };
//         },
//         .type_of => |cast_to| {
//             continue :again tc.type_store.get(cast_to).data;
//         },
//         .user => |user| {
//             continue :again tc.type_store.get(user.type.id).data;
//         },
//         inline .builtin, .primitive => {
//             const callable_td = tc.type_store.get(callable_t).data;
//             const cast_to = callable_td.type_of;
//             return .{
//                 .item_desc = "cast operand",
//                 .signature = ty.Fun.Signature.initCast(cast_to),
//                 .param_bindings = &.{
//                     .{
//                         .token = .{
//                             .type = .ident,
//                             .span = tc.ast.num(0),
//                         },
//                     },
//                 },
//             };
//         },
//         else => |x| common.todoNoReturn("callable info: {any}", .{x}),
//     }
//     unreachable;
// }

pub fn exitCallExpr(tc: *TypeChecker, call: *node.CallExpr) void {
    if (call.call_bindings == null) {
        return;
    }

    // Now we can actually check the arguments match to what they were
    // bound to.
    const cb = call.call_bindings.?;

    for (cb.bindings) |b| {
        if (b.expr == null) {
            continue;
        }

        const ex = b.expr.?;

        if (ex.head().flags.contains(.fake)) {
            // Run exit hook on unpack param
            tc.exitCallExpr(&ex.call);
        }

        const callable_t = call.callable.getType().*;
        const callable_td = tc.type_store.get(callable_t).data;
        const callable_type = tc.getCallableType(callable_t) orelse return;
        defer _ = tc.ctx().scratch.reset(.retain_capacity);

        for (callable_type.bindings, 0..) |pb, i| {
            if (std.mem.eql(u8, b.name, pb.text())) {
                const param = callable_type.signature.get(tc.type_store.*).params[i];
                // TODO support cast of user defined types (not just builtins)
                if (callable_td.isBuiltinCallable(tc.type_store.*)) {
                    tc.tryCastTo(ex.getType(), param.type);
                    if (ex.getType().* != param.type) {
                        tc.raise(
                            call.head.position,
                            "cannot cast type {f} to type {f}",
                            .{
                                ty.formatView(tc.type_store, ex.getType().*),
                                ty.formatView(tc.type_store, param.type),
                            },
                        );
                    }
                    break;
                }
                if (ex.getType().* != param.type) {
                    tc.raise(
                        ex.at(),
                        "invalid argument type {f}; argument {s} of '{f}' expects type {f}",
                        .{
                            ty.formatView(tc.type_store, ex.getType().*),
                            pb.text(),
                            ty.formatView(tc.type_store, callable_t),
                            ty.formatView(tc.type_store, param.type),
                        },
                    );
                }
            }
        }
    }
}

pub fn enterAnonCallExpr(tc: *TypeChecker, anon_call: *node.AnonCallExpr) void {
    anon_call.type_ref = tc.bindCall(tc.type_store.internInfoStable(.fromData(.{
        .type_of = anon_call.hint.?.data.type.id,
    })), anon_call);
}

pub fn exitAnonCallExpr(tc: *TypeChecker, anon_call: *node.AnonCallExpr) void {
    if (anon_call.type_ref == .unset) {
        tc.raise(
            anon_call.head.position,
            "could not deduce the type of anonymous call",
            .{},
        );
        return;
    }
    if (anon_call.call_bindings == null) {
        return;
    }
    // Now we can actually check the arguments match to what they were
    // bound to.
    const cb = anon_call.call_bindings.?;

    for (cb.bindings) |b| {
        if (b.expr == null) {
            continue;
        }

        const ex = b.expr.?;
        const ex_t = ex.getType().*;

        const callable_t = anon_call.type_ref;
        const callable_type = tc.getCallableType(callable_t) orelse return;
        defer _ = tc.ctx().scratch.reset(.retain_capacity);

        for (callable_type.bindings, 0..) |pb, i| {
            if (std.mem.eql(u8, b.name, pb.text())) {
                const param = callable_type.signature.get(tc.type_store.*).params[i];
                if (ex_t != param.type) {
                    tc.raise(
                        ex.at(),
                        "invalid argument type {f}; {s} {s} of '{f}' expects type {f}",
                        .{
                            ty.formatView(tc.type_store, ex_t),
                            "parameter", // TODO need to figure this out so I can change
                            // to "field" in case of struct and tuple
                            pb.text(),
                            ty.formatView(tc.type_store, callable_t),
                            ty.formatView(tc.type_store, param.type),
                        },
                    );
                }
            }
        }
    }
}

pub fn exitType(tc: *TypeChecker, t: *node.Type) void {
    if (t.isWeak() and !t.isLinear()) {
        tc.raise(t.at(), "cannot specify non-linear type as 'weak'", .{});
    }
}

pub fn exitTupleType(tc: *TypeChecker, tuple_type: *node.TupleType) void {
    tuple_type.callable = .{
        .head = tuple_type.head.toFake(),
        .linkage = .internal,
        .return_type = @fieldParentPtr("tuple", tuple_type),
    };

    const scope = tuple_type.callable.x(.scope);
    var params = tc.ast.arena.allocator().alloc(node.FunParam, tuple_type.types.len)
        catch @panic("OOM");

    for (tuple_type.types, 0..) |*tp, i| {
        params[i] = .{
            .head = tp.head.toFake(),
            .name = .{
                .head = tp.head.toFake(),
                .token = .{ .span = tc.ast.num(i) },
            },
            .type = tp.type,
            .symbol = .{ .type = tp.xv(.id), .kind = .param },
        };
        const param: *node.FunParam = &params[params.len - 1];
        param.x(.hint).* = tp.type.symbol();
        _ = scope.insert(tc.ctx().allocator, .fromNode(param));
    }

    tuple_type.callable.params = params;
    tuple_type.callable.x(.id).* = tc.type_store.intern(&node.Type{ .fun = tuple_type.callable });
}

pub fn exitStructType(tc: *TypeChecker, struct_type: *node.StructType) void {
    struct_type.callable = .{
        .head = struct_type.head.toFake(),
        .linkage = .internal,
        .return_type = @fieldParentPtr("struct", struct_type),
    };

    const scope = struct_type.callable.x(.scope);
    var params = tc.ast.arena.allocator().alloc(node.FunParam, struct_type.fields.len)
        catch @panic("OOM");

    for (struct_type.fields, 0..) |*f, i| {
        params[i] = .{
            .head = f.head.toFake(),
            .name = .{
                .head = f.head.toFake(),
                .token = .{ .span = tc.ast.num(i) },
            },
            .type = f.type,
            .symbol = .{ .type = f.xv(.type), .kind = .param },
        };
        const param: *node.FunParam = &params[params.len - 1];
        param.symbol.hint = f.type.symbol();
        _ = scope.insert(tc.ctx().allocator, .fromNode(param));
    }

    struct_type.callable.params = params;
    struct_type.callable.x(.id).* = tc.type_store.intern(&node.Type{ .fun = struct_type.callable });

    if (struct_type.head.flags.contains(.linear)) {
        // If the type is linear, no need to check the fields for linearity
        return;
    }

    const encl: *node.Type = @fieldParentPtr("struct", struct_type);
    const encl_t = tc.type_store.intern(encl);

    // Enclosing type is not linear, make sure no fields are linear
    for (struct_type.fields) |f| {
        if (f.type.isLinear()) {
            tc.raise(struct_type.head.position, "type '{f}' must be marked linear as it's field {s} is linear", .{
                ty.formatView(tc.type_store, encl_t),
                f.name.text(),
            });
        }
    }
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

fn hintType(_: *TypeChecker, hint: node.Symbol, ex: *node.Expr) void {
    again: switch (ex.*) {
        .call => |*call| {
            if (call.callable.* == .ident_expr) {
                continue :again call.callable.*;
            }
        },
        .anon_call => |*ac| {
            ac.hint = hint;
        },
        .ident_expr => |*id| {
            if (id.is_inferred) {
                id.hint = hint;
            }
        },
        else => {},
    }
}
