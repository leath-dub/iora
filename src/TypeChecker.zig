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
    const any_type: node.Type = .{ .fun = fun_type };
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

pub fn exitTypeDecl(tc: *TypeChecker, type_decl: *node.TypeDecl) void {
    type_decl.type_ref = tc.type_store.intern(&type_decl.type);
}

pub fn exitStructField(tc: *TypeChecker, struct_field: *node.StructField) void {
    struct_field.type_ref = tc.type_store.intern(&struct_field.type);
}

pub fn enterVarDecl(tc: *TypeChecker, var_decl: *node.VarDecl) void {
    if (var_decl.type) |*t| {
        var_decl.type_ref = tc.type_store.intern(t);
    }
    if (var_decl.init_expr) |*init_expr| {
        tc.hintType(var_decl.type_ref, init_expr);
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

pub fn exitTypeExpr(tc: *TypeChecker, type_expr: *node.TypeExpr) void {
    type_expr.type_ref = tc.type_store.internDataStable(.{
        .type_of = tc.type_store.intern(type_expr.type),
    });
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
    if (ident_expr.is_inferred and ident_expr.hint != .unset) {
        const hint = tc.type_store.get(ident_expr.hint);
        again: switch (hint) {
            .user => |td| {
                if (td.scope.get(ident_expr.name.text())) |sym| {
                    ident_expr.type_ref = switch (sym.data) {
                        .enumerator => unreachable,
                        inline else => |x| x.type_ref,
                    };
                    return;
                }
                continue :again tc.type_store.get(td.type_ref);
            },
            .@"enum" => |en| {
                for (en.enumerators) |enumerator| {
                    if (std.mem.eql(u8, enumerator, ident_expr.name.text())) {
                        break :again;
                    }
                }
                tc.raise(
                    ident_expr.head.position,
                    "undefined enumerator {s} of hinted: {f}",
                    .{
                        ident_expr.name.text(),
                        ty.formatView(tc.type_store, ident_expr.hint),
                    },
                );
            },
            else => {
                tc.raise(
                    ident_expr.head.position,
                    "undefined {s} of hinted: {f}",
                    .{ ident_expr.name.text(), ty.formatView(tc.type_store, ident_expr.hint) },
                );
            },
        }
        ident_expr.type_ref = ident_expr.hint;
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

    pub fn init(al: std.mem.Allocator, sig: ty.Fun.Signature, param_bindings: []const node.Ident) CallBindingsOps {
        std.debug.assert(sig.params.len != 0);
        var bindings_ = al.alloc(node.CallBindings.ArgBinding, sig.params.len) catch @panic("OOM");
        for (0..sig.params.len) |i| {
            bindings_[i] = .{ .name = param_bindings[i].text() };
        }
        return .{
            .call_bindings = .{
                .bindings = bindings_,
            },
        };
    }

    pub fn fromFun(al: std.mem.Allocator, store: ty.Store, fun: ty.Fun) CallBindingsOps {
        const sig = fun.signature.get(store);
        return .init(al, sig, fun.bindings);
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

const CallArgBinder = struct {
    tc: *TypeChecker,
    sig: ty.Fun.Signature,
    callable_t: TypeRef,
    param_bindings: []const node.Ident,
    bind_ops: CallBindingsOps,
    item_desc: []const u8 = "parameter",

    fn init(tc: *TypeChecker, item_name: []const u8, callable_t: TypeRef, sig: ty.Fun.Signature, param_bindings: []const node.Ident) CallArgBinder {
        return .{
            .tc = tc,
            .sig = sig,
            .callable_t = callable_t,
            .param_bindings = param_bindings,
            .bind_ops = CallBindingsOps.init(
                tc.ast.arena.allocator(),
                sig,
                param_bindings,
            ),
            .item_desc = item_name,
        };
    }

    fn fromFun(tc: *TypeChecker, fun_t: ty.TypeRefStrict(ty.Fun)) CallArgBinder {
        const fun = fun_t.get(tc.type_store.*);
        const sig = fun.signature.get(tc.type_store.*);
        return .{
            .tc = tc,
            .sig = sig,
            .callable_t = fun_t.ref,
            .param_bindings = fun.bindings,
            .bind_ops = CallBindingsOps.fromFun(
                tc.ast.arena.allocator(),
                tc.type_store.*,
                fun,
            ),
        };
    }

    fn bind(c: *CallArgBinder, call_at: Code.Offset, args: []node.CallExprArg) node.CallBindings {
        var has_error = false;
        const cb = &c.bind_ops;
        const tc = c.tc;
        const sig = c.sig;

        var arg_i: usize = 0;
        while (arg_i < args.len) : (arg_i += 1) {
            const arg = &args[arg_i];
            const i_opt = cb.available();
            if (i_opt == null) {
                tc.raise(
                    arg.at(),
                    "extraneous argument",
                    .{},
                );
                has_error = true;
                break;
            }
            const i = i_opt.?;
            const param = sig.params[i];
            const arg_unpack = switch (arg.*) {
                .unpack => true,
                .labelled => |lab| lab.unpack,
                else => false,
            };
            if (param.unpack and !arg_unpack) {
                const param_td = tc.type_store
                    .get(param.type)
                    .getUnderlyingType(tc.type_store.*);
                switch (param_td) {
                    .@"struct" => |st| {
                        // Synthesize a function signature for the
                        // struct initialization
                        const al = tc.ctx().scratch.allocator();
                        defer _ = tc.ctx().scratch.reset(.retain_capacity);

                        const st_sig = synthSigFromStruct(al, param.type, st);
                        const st_param_bindings = synthParamBindingsFromStruct(al, st);
                        var sub_binder = CallArgBinder.init(tc, "field", param.type, st_sig, st_param_bindings);

                        const bound = std.math.clamp(arg_i + st.fields.len, 0, args.len);
                        const sub_args = args[arg_i..bound];

                        // We also need to synthesize a call expression
                        // to bind to the parameter - the call expression
                        // will store the binding result of the sub check
                        const fake_token = node.TokenExpr{
                            .token = .{
                                .type = .synthesized,
                                .span = "<fake>",
                            },
                            .type_ref = tc.type_store.internDataStable(.{
                                .type_of = param.type,
                            }),
                        };

                        const fake_call = node.CallExpr{
                            .head = .{
                                .flags = .init(.{ .fake = true }),
                                .position = sub_args[0].at(),
                            },
                            .args = sub_args,
                            .callable = tc.ast.box(node.Expr{ .token_expr = fake_token }),
                            .type_ref = param.type,
                        };

                        var fake_expr = tc.ast.box(node.Expr{ .call = fake_call });

                        // TODO(default values): if we get an invalid type argument in
                        // the sub check, it should only be invalid
                        // if it would not also match the type of the
                        // parameter after the packed parameter.
                        //
                        // This means for example if you have:
                        //
                        // type Foo = struct { x: u32 = 11, y: u32 };
                        //
                        // fun func(s: str, ..f: Foo, g: str) extern;
                        //
                        // ...
                        // func("xx", y: 12, "")
                        //
                        // This should be valid by just assuming that
                        // 'x' takes on it's default value
                        fake_expr.call.call_bindings = sub_binder.bind(arg.at(), sub_args);

                        std.debug.assert(cb.bindAt(i, fake_expr) == .success);

                        arg_i = bound - 1;
                        continue;
                    },
                    .tuple => |tup| {
                        // Synthesize a function signature for the
                        // struct initialization
                        const al = tc.ctx().scratch.allocator();
                        defer _ = tc.ctx().scratch.reset(.retain_capacity);

                        const st_sig = synthSigFromTuple(al, param.type, tup);
                        const st_param_bindings = synthParamBindingsFromTuple(tc.ast, al, tup);
                        var sub_binder = CallArgBinder.init(tc, "field", param.type, st_sig, st_param_bindings);

                        const bound = std.math.clamp(arg_i + tup.types.len, 0, args.len);
                        const sub_args = args[arg_i..bound];

                        // We also need to synthesize a call expression
                        // to bind to the parameter - the call expression
                        // will store the binding result of the sub check
                        const fake_token = node.TokenExpr{
                            .token = .{
                                .type = .synthesized,
                                .span = "<synthesized>",
                            },
                            .type_ref = tc.type_store.internDataStable(.{
                                .type_of = param.type,
                            }),
                        };

                        const fake_call = node.CallExpr{
                            .args = sub_args,
                            .callable = tc.ast.box(node.Expr{ .token_expr = fake_token }),
                            .type_ref = param.type,
                        };

                        var fake_expr = tc.ast.box(node.Expr{ .call = fake_call });
                        fake_expr.call.call_bindings = sub_binder.bind(arg.at(), sub_args);

                        std.debug.assert(cb.bindAt(i, fake_expr) == .success);

                        arg_i = bound - 1;
                        continue;
                    },
                    else => unreachable,
                }
            }
            switch (arg.*) {
                .expr => |*ex| {
                    switch (cb.bindAt(i, ex)) {
                        .success => {},
                        .already_bound => |at| {
                            tc.raise(
                                ex.at(),
                                "cannot specify {s} {s} twice",
                                .{ c.item_desc, cb.bindings()[at].name },
                            );
                            tc.raise(
                                cb.bindings()[at].expr.?.at(),
                                "note: {s} {s} already specified here",
                                .{ c.item_desc, cb.bindings()[at].name },
                            );
                            has_error = true;
                        },
                        .failure => unreachable,
                    }
                    tc.hintType(param.type, ex);
                },
                .unpack => |*un| {
                    if (!param.unpack) {
                        tc.raise(
                            un.expr.at(),
                            "cannot pass unpacked argument to {s} {s}",
                            .{ c.item_desc, c.param_bindings[i].text() },
                        );
                        has_error = true;
                        continue;
                    }
                    switch (cb.bindAt(i, &un.expr)) {
                        .success => {},
                        .already_bound => |at| {
                            tc.raise(
                                un.expr.at(),
                                "cannot specify {s} {s} twice",
                                .{ c.item_desc, cb.bindings()[at].name },
                            );
                            tc.raise(
                                cb.bindings()[at].expr.?.at(),
                                "note: {s} {s} already specified here",
                                .{ c.item_desc, cb.bindings()[at].name },
                            );
                            has_error = true;
                        },
                        .failure => unreachable,
                    }
                    tc.hintType(param.type, &un.expr);
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
                                "cannot specify {s} {s} twice",
                                .{ c.item_desc, cb.bindings()[at].name },
                            );
                            tc.raise(
                                cb.bindings()[at].expr.?.at(),
                                "note: {s} {s} is already specified here",
                                .{ c.item_desc, cb.bindings()[at].name },
                            );
                            has_error = true;
                        },
                        .failure => {
                            tc.raise(
                                lab.head.position,
                                "unknown {s} {s} of '{f}'",
                                .{
                                    c.item_desc,
                                    lab.label.text(),
                                    ty.formatView(tc.type_store, c.callable_t),
                                },
                            );
                            has_error = true;
                        },
                    }
                    if (param_opt == null) {
                        continue;
                    }
                    const param_ = param_opt.?;
                    tc.hintType(param_.type, &lab.expr);
                },
                .dirty => {},
            }
        }

        // Now check if the CallBindings have any unbound parameters
        if (!has_error) { // no need to over-report we already have
            // semantic errors so this is less likely to
            // be accurate
            for (cb.bindings()) |binding| {
                if (binding.expr == null) {
                    tc.raise(
                        call_at,
                        "{s} {s} of '{f}' is not specified",
                        .{
                            c.item_desc,
                            binding.name,
                            ty.formatView(tc.type_store, c.callable_t),
                        },
                    );
                }
            }
        }

        return c.bind_ops.call_bindings;
    }
};

fn bindCall(tc: *TypeChecker, call_t: TypeRef, call: anytype) TypeRef {
    const callable = tc.type_store.get(call_t);
    return switch (callable) {
        .fun => res: {
            var binder = CallArgBinder.fromFun(tc, .{ .ref = call_t });
            defer _ = tc.ctx().scratch.reset(.retain_capacity);
            call.call_bindings = binder.bind(call.head.position, call.args);
            break :res binder.sig.return_type;
        },
        .type_of => |cast_to| res: {
            if (!cast_to.isBuiltin()) {
                // This means we are trying to call a type: e.g. Point(10, 11)
                // TODO validate the arguments

                // Callable is a user defined type
                // Could be:
                //
                // * Tuple
                // * Sum type
                // * Struct
                // * Primitive type

                const user_data = tc.type_store.get(cast_to);
                const canon_user_data = user_data.getUnderlyingType(tc.type_store.*);

                switch (canon_user_data) {
                    .@"struct" => |st| {
                        const al = tc.ctx().scratch.allocator();
                        defer _ = tc.ctx().scratch.reset(.retain_capacity);

                        const st_sig = synthSigFromStruct(al, cast_to, st);
                        const st_param_bindings = synthParamBindingsFromStruct(al, st);
                        var binder = CallArgBinder.init(tc, "field", cast_to, st_sig, st_param_bindings);

                        call.call_bindings = binder.bind(call.head.position, call.args);
                    },
                    .tuple => |tup| {
                        const al = tc.ctx().scratch.allocator();
                        defer _ = tc.ctx().scratch.reset(.retain_capacity);

                        const tup_sig = synthSigFromTuple(al, cast_to, tup);
                        const tup_param_bindings = synthParamBindingsFromTuple(tc.ast, al, tup);
                        var binder = CallArgBinder.init(tc, "field", cast_to, tup_sig, tup_param_bindings);

                        call.call_bindings = binder.bind(call.head.position, call.args);
                    },
                    else => {},
                }

                break :res cast_to;
            }

            // Cast expression to builtin type: e.g. `u32(10)`
            // First make sure there is only one argument
            var child_type: ?*TypeRef = null;
            if (call.args.len != 1) {
                tc.raise(call.head.position, "cast expression can only take a single argument", .{});
                return .dirty;
            } else {
                child_type = switch (call.args[0]) {
                    .expr => |*ex| ex.getType(),
                    .labelled => common.todoNoReturn("labelled args", .{}),
                    .unpack => common.todoNoReturn("unpack args", .{}),
                    .dirty => return .dirty,
                };
            }

            const builtin_param_bindings: []const node.Ident = &.{
                .{
                    .token = .{
                        .type = .ident,
                        .span = tc.ast.num(0),
                    },
                },
            };
            const builtin_sig: ty.Fun.Signature = .initCast(cast_to);
            var binder = CallArgBinder.init(tc, "cast operand", cast_to, builtin_sig, builtin_param_bindings);
            call.call_bindings = binder.bind(call.head.position, call.args);

            break :res cast_to;
        },
        else => {
            common.todoNoReturn("more callables: {any}", .{callable});
        },
    };
}

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

const CallableInfo = struct {
    item_desc: []const u8,
    signature: ty.Fun.Signature,
    param_bindings: []const node.Ident,
};

// Needs to be inline otherwise I get valgrind errors :O
inline fn getCallableInfo(tc: *TypeChecker, callable_t: TypeRef) CallableInfo {
    const callable = tc.type_store.get(callable_t);
    again: switch (callable) {
        .fun => |fun| {
            return .{
                .item_desc = "parameter",
                .param_bindings = fun.bindings,
                .signature = fun.signature.get(tc.type_store.*),
            };
        },
        .@"struct" => |st| {
            const al = tc.ctx().scratch.allocator();

            const st_sig = synthSigFromStruct(al, callable_t, st);
            const st_param_bindings = synthParamBindingsFromStruct(al, st);

            return .{
                .item_desc = "field",
                .signature = st_sig,
                .param_bindings = st_param_bindings,
            };
        },
        .tuple => |tup| {
            const al = tc.ctx().scratch.allocator();

            const tup_sig = synthSigFromTuple(al, callable_t, tup);
            const tup_param_bindings = synthParamBindingsFromTuple(tc.ast, al, tup);

            return .{
                .item_desc = "field",
                .signature = tup_sig,
                .param_bindings = tup_param_bindings,
            };
        },
        .type_of => |cast_to| {
            continue :again tc.type_store.get(cast_to);
        },
        .user => |user| {
            continue :again tc.type_store.get(user.type_ref);
        },
        .primitive => {
            const callable_td = tc.type_store.get(callable_t);
            const cast_to = callable_td.type_of;
            return .{
                .item_desc = "cast operand",
                .signature = ty.Fun.Signature.initCast(cast_to),
                .param_bindings = &.{
                    .{
                        .token = .{
                            .type = .ident,
                            .span = tc.ast.num(0),
                        },
                    },
                },
            };
        },
        else => |x| common.todoNoReturn("{any}", .{x}),
    }
    unreachable;
}

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
        const callable_td = tc.type_store.get(callable_t);
        const callable_info = tc.getCallableInfo(callable_t);
        defer _ = tc.ctx().scratch.reset(.retain_capacity);

        for (callable_info.param_bindings, 0..) |pb, i| {
            if (std.mem.eql(u8, b.name, pb.text())) {
                const param = callable_info.signature.params[i];
                if (callable_td == .type_of and callable_td.type_of.isBuiltin()) {
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
                        "invalid argument type {f}; {s} {s} of '{f}' expects type {f}",
                        .{
                            ty.formatView(tc.type_store, ex.getType().*),
                            callable_info.item_desc,
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
    anon_call.type_ref = tc.bindCall(tc.type_store.internDataStable(.{
        .type_of = anon_call.hint,
    }), anon_call);
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
        const callable_info = tc.getCallableInfo(callable_t);
        defer _ = tc.ctx().scratch.reset(.retain_capacity);

        for (callable_info.param_bindings, 0..) |pb, i| {
            if (std.mem.eql(u8, b.name, pb.text())) {
                const param = callable_info.signature.params[i];
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

fn hintType(_: *TypeChecker, hint: TypeRef, ex: *node.Expr) void {
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
