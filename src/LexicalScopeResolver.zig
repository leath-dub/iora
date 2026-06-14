const std = @import("std");

const Ast = @import("Ast.zig");
const node = @import("node.zig");
const Code = @import("Code.zig");
const GeneralContext = @import("GeneralContext.zig");
const common = @import("common.zig");
const util = @import("util.zig");

const LexicalScopeResolver = @This();

ast: *Ast,
code: *Code,
arena: std.heap.ArenaAllocator,
global_scope: *node.Scope,
scopes: util.ChunkedStack(*node.Scope),
label_scopes: util.ChunkedStack(*node.LabelScope),
in_global_type: bool = false,

pub fn init(ast: *Ast, code: *Code) LexicalScopeResolver {
    return .{
        .ast = ast,
        .code = code,
        .arena = ast.ctx.createLifetime(),
        .global_scope = &ast.root.?.scope,
        .scopes = .init(ast.ctx.allocator),
        .label_scopes = .init(ast.ctx.allocator),
    };
}

pub fn deinit(lr: *LexicalScopeResolver) void {
    lr.arena.deinit();
    lr.scopes.deinit();
    lr.label_scopes.deinit();
}

pub fn enterSourceFile(lr: *LexicalScopeResolver, source_file: *node.SourceFile) void {
    lr.push(&source_file.scope);
}

pub fn exitSourceFile(lr: *LexicalScopeResolver, source_file: *node.SourceFile) void {
    lr.pop(&source_file.scope);
}

pub fn enterFunDecl(lr: *LexicalScopeResolver, fun_decl: *node.FunDecl) void {
    defer {
        lr.push(fun_decl.x(.scope));
        lr.pushLabelScope(fun_decl.x(.label_scope));

        // Forward insert all labels
        var label_resolver = LabelInserter{ .lr = lr };
        Ast.walk(&label_resolver, &fun_decl.body);
    }

    var target_scope = lr.top();
    if (target_scope == lr.global_scope) {
        return;
    }

    if (fun_decl.type_name) |*type_name| {
        const result = common.resolve(lr.top(), type_name);
        const type_scope = if (result) |symbol| switch (symbol.data) {
            .type => |t| &t.scope,
            else => {
                lr.raise(
                    type_name.head.position,
                    "{s} does not resolve to a type",
                    .{
                        type_name.text(),
                    },
                );
                return;
            },
        } else {
            lr.raise(type_name.head.position, "undefined: {s}", .{type_name.text()});
            return;
        };
        target_scope = type_scope;
    }

    // TODO: restrict non-local Foo__bar if Foo::bar exists
    // TODO: restrict name of non-local function to not include ' (e.g. func')
    if (target_scope.insert(lr.ctx().allocator, node.Symbol.fromNode(fun_decl))) |existing| {
        lr.raise(
            fun_decl.name.head.position,
            "{s} redeclared in this block; other declaration at {f}",
            .{
                fun_decl.name.text(),
                lr.code.target(existing.source),
            },
        );
    }
}

pub fn enterFunType(lr: *LexicalScopeResolver, fun_type: *node.FunType) void {
    lr.push(fun_type.x(.scope));
}

pub fn exitFunType(lr: *LexicalScopeResolver, fun_type: *node.FunType) void {
    lr.pop(fun_type.x(.scope));
}

const LabelInserter = struct {
    lr: *LexicalScopeResolver,

    pub fn enterLabelledStmt(li: *LabelInserter, labelled_stmt: *node.LabelledStmt) void {
        const lr = li.lr;
        lr.insertLabel(labelled_stmt);
    }

    pub fn enterFunDecl(_: *LabelInserter, _: *node.FunDecl) Ast.ChildDisposition {
        return .skip;
    }
};

pub fn enterDefDecl(lr: *LexicalScopeResolver, def_decl: *node.DefDecl) void {
    var target_scope = lr.top();
    if (def_decl.type_name) |*type_name| {
        const result = common.resolve(lr.top(), type_name);
        const type_scope = if (result) |symbol| switch (symbol.data) {
            .type => |t| &t.scope,
            else => {
                lr.raise(
                    type_name.head.position,
                    "{s} does not resolve to a type",
                    .{
                        type_name.text(),
                    },
                );
                return;
            },
        } else {
            lr.raise(type_name.head.position, "undefined: {s}", .{type_name.text()});
            return;
        };
        target_scope = type_scope;
    }

    // Should have already been resolved by `ModuleScopeResolver`
    if (target_scope == lr.global_scope) {
        return;
    }

    if (target_scope.insert(lr.ctx().allocator, node.Symbol.fromNode(def_decl))) |existing| {
        lr.raise(
            def_decl.name.head.position,
            "{s} redeclared in this block; other declaration at {f}",
            .{
                def_decl.name.text(),
                lr.code.target(existing.source),
            },
        );
    }
}

pub fn exitFunDecl(lr: *LexicalScopeResolver, fun_decl: *node.FunDecl) void {
    lr.pop(fun_decl.x(.scope));
    lr.popLabelScope(fun_decl.x(.label_scope));
}

pub fn enterSumType(lr: *LexicalScopeResolver, sum_type: *node.SumType) void {
    sum_type.symbol = .{};
    lr.push(sum_type.x(.scope));
}

pub fn exitSumType(lr: *LexicalScopeResolver, sum_type: *node.SumType) void {
    lr.pop(sum_type.x(.scope));
}

pub fn enterTupleType(lr: *LexicalScopeResolver, tuple_type: *node.TupleType) void {
    lr.push(tuple_type.x(.scope));
}

pub fn exitTupleType(lr: *LexicalScopeResolver, tuple_type: *node.TupleType) void {
    const scope = lr.top();
    if (!lr.in_global_type) {
        for (tuple_type.types, 0..) |*ty, index| {
            std.debug.assert(scope.insert(lr.ctx().allocator, .{
                .name = lr.ast.num(index),
                .data = node.Symbol.Data.fromAlt(&ty.symbol),
            }) == null);
        }
    }
    lr.pop(tuple_type.x(.scope));
}

pub fn enterStructType(lr: *LexicalScopeResolver, struct_type: *node.StructType) void {
    lr.push(struct_type.x(.scope));
}

pub fn exitStructType(lr: *LexicalScopeResolver, struct_type: *node.StructType) void {
    lr.pop(struct_type.x(.scope));
}

pub fn enterCompStmt(lr: *LexicalScopeResolver, comp_stmt: *node.CompStmt) void {
    lr.push(&comp_stmt.scope);
}

pub fn exitCompStmt(lr: *LexicalScopeResolver, comp_stmt: *node.CompStmt) void {
    lr.pop(&comp_stmt.scope);
}

pub fn enterEnumType(lr: *LexicalScopeResolver, enum_type: *node.EnumType) Ast.ChildDisposition {
    lr.push(enum_type.x(.scope));
    if (!lr.in_global_type) {
        for (enum_type.alts) |*alt| {
            alt.symbol.enclosed_by = .fromNode(enum_type);
            lr.insert(node.Symbol{
                .name = alt.name.text(),
                .data = .fromAlt(&alt.symbol),
            });
        }
    }
    return .skip; // no need to visit children as they are handled above
}

pub fn exitEnumType(lr: *LexicalScopeResolver, enum_type: *node.EnumType) void {
    lr.pop(enum_type.x(.scope));
}

pub fn enterTypeDecl(lr: *LexicalScopeResolver, type_decl: *node.TypeDecl) void {
    // Set flag so we only resolve expression context names under
    // global types (e.g. default values). This is so we don't try to
    // re-resolve types already resolved by ModuleScopeResolver.
    if (!lr.in_global_type) {
        lr.in_global_type = lr.top() == lr.global_scope;
    }
    if (!lr.in_global_type) {
        lr.insert(type_decl);
        type_decl.x(.underlying_type).* = type_decl.type.symbol();
    }
    lr.push(type_decl.x(.scope));
}

pub fn exitTypeDecl(lr: *LexicalScopeResolver, type_decl: *node.TypeDecl) void {
    lr.pop(type_decl.x(.scope));
    if (!lr.in_global_type) {
        // Amend type context when we realise that the enum was a child of
        // a distinct type
        if (type_decl.type == .@"enum") {
            const en = &type_decl.type.@"enum";
            var it = en.x(.scope).entries.iterator();
            while (it.next()) |ent| {
                ent.value_ptr.data.enumerator.enclosed_by = .fromNode(type_decl);
            }
        }
    }
    if (lr.in_global_type and lr.top() == lr.global_scope) {
        lr.in_global_type = false;
    }
}

pub fn exitFunParam(lr: *LexicalScopeResolver, fun_param: *node.FunParam) void {
    lr.insert(fun_param);
}

pub fn exitVarDecl(lr: *LexicalScopeResolver, var_decl: *node.VarDecl) void {
    if (lr.top() != lr.global_scope) {
        lr.insert(var_decl);
    }
}

pub fn enterIdentType(lr: *LexicalScopeResolver, ident_type: *node.IdentType) void {
    if (lr.in_global_type) {
        return;
    }

    const name = &ident_type.name;
    const position = name.head.position;

    ident_type.resolves_to = common.resolve(lr.top(), name);
    if (ident_type.resolves_to) |symbol| {
        if (symbol.data != .type) {
            lr.raise(position, "expected {s} to be a type", .{name.text()});
        }
    } else {
        lr.raise(position, "undefined type: {s}", .{name.text()});
    }
}

pub fn exitSelectorType(lr: *LexicalScopeResolver, sel: *node.SelectorType) void {
    if (lr.in_global_type) {
        return;
    }

    const res = switch (sel.type.*) {
        .ident => |id| id.resolves_to,
        .selector => |inner_sel| inner_sel.resolves_to,
        else => null,
    };
    const field = &sel.field;
    if (res) |symbol| {
        common.resolveTypeSelector(symbol, field, &sel.resolves_to);
    }
    if (sel.resolves_to == null) {
        lr.raise(field.head.position, "undefined: {s}", .{field.text()});
    }
}

pub fn enterIdentExpr(lr: *LexicalScopeResolver, ident_expr: *node.IdentExpr) void {
    if (ident_expr.is_inferred) {
        // inferred names are deferred to type checking
        return;
    }
    ident_expr.resolves_to = common.resolve(lr.top(), &ident_expr.name);
    if (ident_expr.resolves_to == null) {
        lr.raise(ident_expr.head.position, "undefined: {s}", .{ident_expr.name.text()});
    }
}

pub fn exitSelectorExpr(lr: *LexicalScopeResolver, selector_expr: *node.SelectorExpr) void {
    const res = switch (selector_expr.value.*) {
        .ident_expr => |id| id.resolves_to,
        .selector_expr => |sel| sel.resolves_to,
        else => null,
    };
    const field = &selector_expr.field;
    if (res) |symbol| {
        if (symbol.data == .@"var") {
            // This cannot be resolved at this point as we do not have the
            // types resolved for variable declarations. Resolution like this
            // needs to be done along with type checking in TypeChecker.zig
            return;
        }
        common.resolveTypeSelector(symbol, field, &selector_expr.resolves_to);
    }
    if (selector_expr.resolves_to == null) {
        lr.raise(field.head.position, "undefined: {s}", .{field.text()});
    }
}

pub fn enterBranchStmt(lr: *LexicalScopeResolver, branch: *node.BranchStmt) void {
    if (branch.label_name) |label_name| {
        if (lr.topLabelScope().entries.get(label_name.text())) |label| {
            branch._label = label;
        } else {
            lr.raise(
                label_name.head.position,
                "undefined label: {s}",
                .{label_name.text()},
            );
        }
    }
}

pub fn enterStructField(lr: *LexicalScopeResolver, struct_field: *node.StructField) void {
    lr.insert(struct_field);
}

fn push(lr: *LexicalScopeResolver, ref: *node.Scope) void {
    ref.parent = lr.topOrNull();
    lr.scopes.push(ref) catch @panic("OOM");
}

fn pop(lr: *LexicalScopeResolver, scope: *node.Scope) void {
    std.debug.assert(lr.scopes.pop() == scope);
}

fn pushLabelScope(lr: *LexicalScopeResolver, ref: *node.LabelScope) void {
    lr.label_scopes.push(ref) catch @panic("OOM");
}

fn popLabelScope(lr: *LexicalScopeResolver, scope: *node.LabelScope) void {
    std.debug.assert(lr.label_scopes.pop() == scope);
}

fn top(lr: *LexicalScopeResolver) *node.Scope {
    return lr.topOrNull().?;
}

fn topOrNull(lr: *LexicalScopeResolver) ?*node.Scope {
    return lr.scopes.top();
}

fn topLabelScope(lr: *LexicalScopeResolver) *node.LabelScope {
    return lr.label_scopes.top().?;
}

fn insert(lr: *LexicalScopeResolver, symbol_: anytype) void {
    const position = if (@TypeOf(symbol_) != node.Symbol)
        symbol_.name.head.position
    else
        symbol_.source;
    const symbol = if (@TypeOf(symbol_) != node.Symbol) node.Symbol.fromNode(symbol_) else symbol_;
    if (lr.top().insert(lr.ctx().allocator, symbol)) |existing| {
        lr.raise(
            position,
            "{s} redeclared in this block; other declaration at {f}",
            .{
                symbol.name,
                lr.code.target(existing.source),
            },
        );
    }
}

fn insertLabel(lr: *LexicalScopeResolver, symbol: *node.LabelledStmt) void {
    if (lr.topLabelScope().insert(lr.ctx().allocator, symbol)) |existing| {
        lr.raise(
            symbol.name.head.position,
            "{s} redeclared in this block; other declaration at {f}",
            .{
                symbol.name.text(),
                lr.code.target(existing.head.position),
            },
        );
    }
}

fn ctx(lr: *LexicalScopeResolver) *GeneralContext {
    return lr.ast.ctx;
}

inline fn raise(lr: *LexicalScopeResolver, at: Code.Offset, comptime fmt: []const u8, args: anytype) void {
    lr.code.raise(lr.ctx().error_out, at, fmt, args) catch unreachable;
}
