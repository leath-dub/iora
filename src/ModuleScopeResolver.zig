const std = @import("std");

const Ast = @import("Ast.zig");
const node = @import("node.zig");
const Code = @import("Code.zig");
const GeneralContext = @import("GeneralContext.zig");
const common = @import("common.zig");
const util = @import("util.zig");

const ModuleScopeResolver = @This();

const Pass = enum {
    global_scope,
    type_scope,
    alias_cycle,
};

ast: *Ast,
code: *Code,
arena: std.heap.ArenaAllocator,
global_scope: *node.Scope,
pass: Pass = .global_scope,
scopes: util.ChunkedStack(*node.Scope),

pub fn init(ast: *Ast, code: *Code) ModuleScopeResolver {
    return .{
        .ast = ast,
        .code = code,
        .arena = ast.ctx.createLifetime(),
        .global_scope = &ast.root.?.scope,
        .scopes = .init(ast.ctx.allocator),
    };
}

pub fn deinit(mr: *ModuleScopeResolver) void {
    mr.arena.deinit();
    mr.scopes.deinit();
}

pub fn enterSourceFile(mr: *ModuleScopeResolver, source_file: *node.SourceFile) void {
    mr.global_scope = &source_file.scope;
    mr.push(mr.global_scope);
}

pub fn exitSourceFile(mr: *ModuleScopeResolver, source_file: *node.SourceFile) void {
    mr.pop(mr.global_scope);
    switch (mr.pass) {
        .global_scope => mr.pass = .type_scope,
        .type_scope => mr.pass = .alias_cycle,
        .alias_cycle => return,
    }
    if (mr.pass == .alias_cycle) {
        var detector = AliasCycleDetector{ .mr = mr };
        Ast.walk(&detector, source_file);
    } else {
        Ast.walk(mr, source_file);
    }
}

pub fn enterDecl(mr: *ModuleScopeResolver, decl: *node.Decl) Ast.ChildDisposition {
    switch (mr.pass) {
        .global_scope => {
            switch (decl.*) {
                .@"var" => |*var_decl| {
                    mr.insert(var_decl);
                },
                .type => |*type_decl| {
                    mr.insert(type_decl);
                },
                .fun => |*fun_decl| {
                    if (fun_decl.type_name == null) {
                        mr.insert(fun_decl);
                    }
                },
                .def => |*def_decl| {
                    if (def_decl.type_name == null) {
                        mr.insert(def_decl);
                    }
                },
                else => {},
            }
            return .skip;
        },
        .type_scope => {
            return switch (decl.*) {
                .type => .walk,
                .fun => |*fun_decl| ret: {
                    std.debug.assert(mr.top() == mr.global_scope);

                    if (fun_decl.type_name == null) {
                        break :ret .skip;
                    }

                    const type_name = fun_decl.type_name.?;
                    if (common.resolve(mr.top(), &type_name)) |symbol| {
                        if (symbol.data != .type_decl) {
                            mr.raise(
                                type_name.head.position,
                                "expected {s} to be a type",
                                .{type_name.text()},
                            );
                            break :ret .skip;
                        }

                        mr.push(&symbol.data.type_decl.scope);
                        defer mr.pop(&symbol.data.type_decl.scope);

                        mr.insert(fun_decl);
                    } else {
                        mr.raise(
                            type_name.head.position,
                            "undefined: {s}",
                            .{type_name.text()},
                        );
                    }

                    break :ret .skip;
                },
                else => .skip,
            };
        },
        .alias_cycle => return .walk,
    }
}

pub fn exitSelectorType(mr: *ModuleScopeResolver, sel: *node.SelectorType) void {
    const res = switch (sel.type.*) {
        .ident => |id| id.resolves_to,
        .selector => |inner_sel| inner_sel.resolves_to,
        else => null,
    };
    const field = &sel.field;
    if (res) |symbol| {
        resolveSelector(symbol, field, &sel.resolves_to);
    }
    if (sel.resolves_to == null) {
        mr.raise(field.head.position, "undefined: {s}", .{field.text()});
    }
}

fn resolveSelector(symbol: node.Symbol, field: *const node.Ident, out: *?node.Symbol) void {
    var final: ?node.Symbol = symbol;
    defer out.* = final;

    if (final) |s| {
        again: switch (s.data) {
            .type_decl => |td| {
                // First try the type scope
                final = common.resolveLocal(&td.scope, field);

                // Next try local sub-scope
                if (final == null) {
                    const fallback_scope = switch (td.type) {
                        .tuple => |*tup| &tup.scope,
                        .sum => |*sum| &sum.scope,
                        .@"enum" => |*en| &en.scope,
                        .selector => |*sel| blk: {
                            if (sel.resolves_to) |ss| {
                                continue :again ss.data;
                            }
                            break :blk null;
                        },
                        else => null,
                    };
                    if (fallback_scope) |fs| {
                        final = common.resolveLocal(fs, field);
                    }
                }
            },
            else => {},
        }
    }
}

pub fn enterSumType(mr: *ModuleScopeResolver, sum_type: *node.SumType) void {
    mr.push(&sum_type.scope);
}

pub fn exitSumType(mr: *ModuleScopeResolver, sum_type: *node.SumType) void {
    mr.pop(&sum_type.scope);
}

pub fn enterTupleType(mr: *ModuleScopeResolver, tuple_type: *node.TupleType) void {
    mr.push(&tuple_type.scope);
}

pub fn exitTupleType(mr: *ModuleScopeResolver, tuple_type: *node.TupleType) void {
    const scope = mr.top();
    for (tuple_type.types, 0..) |*ty, index| {
        std.debug.assert(scope.insert(mr.ctx().allocator, .{
            .name = mr.ast.num(index),
            .data = node.Symbol.Data.fromSymbolLike(ty),
        }) == null);
    }
    mr.pop(&tuple_type.scope);
}

pub fn enterStructType(mr: *ModuleScopeResolver, struct_type: *node.StructType) void {
    mr.push(&struct_type.scope);
}

pub fn exitStructType(mr: *ModuleScopeResolver, struct_type: *node.StructType) void {
    mr.pop(&struct_type.scope);
}

pub fn enterCompStmt(mr: *ModuleScopeResolver, comp_stmt: *node.CompStmt) void {
    mr.push(&comp_stmt.scope);
}

pub fn exitCompStmt(mr: *ModuleScopeResolver, comp_stmt: *node.CompStmt) void {
    mr.pop(&comp_stmt.scope);
}

pub fn enterEnumType(mr: *ModuleScopeResolver, enum_type: *node.EnumType) Ast.ChildDisposition {
    mr.push(&enum_type.scope);
    for (enum_type.alts) |*alt| {
        mr.insert(node.Symbol{
            .name = alt.name.text(),
            .data = node.Symbol.Data.fromSymbolLike(alt),
            .type_ctx = .{ .enum_type = enum_type },
        });
    }
    return .skip; // no need to visit children as they are handled above
}

pub fn exitTypeDecl(_: *ModuleScopeResolver, type_decl: *node.TypeDecl) void {
    // Amend type context when we realise that the enum was a child of
    // a distinct type
    if (type_decl.type == .@"enum") {
        const en = type_decl.type.@"enum";
        var it = en.scope.entries.iterator();
        while (it.next()) |ent| {
            ent.value_ptr.type_ctx = .{ .type_decl = type_decl };
        }
    }
}

pub fn exitEnumType(mr: *ModuleScopeResolver, enum_type: *node.EnumType) void {
    mr.pop(&enum_type.scope);
}

pub fn enterIdentType(mr: *ModuleScopeResolver, ident_type: *node.IdentType) void {
    if (mr.pass != .type_scope) {
        return;
    }

    const name = &ident_type.name;
    const position = name.head.position;

    ident_type.resolves_to = common.resolve(mr.top(), name);
    if (ident_type.resolves_to) |symbol| {
        if (symbol.data != .type_decl) {
            mr.raise(position, "expected {s} to be a type", .{name.text()});
        }
    } else {
        mr.raise(position, "undefined type: {s}", .{name.text()});
    }
}

fn isAlias(type_decl: *node.TypeDecl) bool {
    return switch (type_decl.type) {
        .ident, .selector => true,
        else => false,
    };
}

const AliasCycleDetector = struct {
    mr: *ModuleScopeResolver,

    pub fn enterTypeDecl(ad: *AliasCycleDetector, type_decl: *node.TypeDecl) void {
        const mr = ad.mr;
        std.debug.assert(mr.pass == .alias_cycle);

        if (isAlias(type_decl)) {
            type_decl.head.flags.insert(.resolving);
        }
    }

    pub fn exitTypeDecl(ad: *AliasCycleDetector, type_decl: *node.TypeDecl) void {
        const mr = ad.mr;
        std.debug.assert(mr.pass == .alias_cycle);

        if (isAlias(type_decl)) {
            var rhs = &type_decl.type;
            while (switch (rhs.*) {
                .ident => |*id| id.resolves_to,
                .selector => |*sel| sel.resolves_to,
                else => null,
            }) |symbol| {
                switch (symbol.data) {
                    .type_decl => |sub_decl| {
                        if (sub_decl.head.flags.contains(.resolving)) {
                            mr.raise(sub_decl.head.position, "alias cycle detected", .{});
                            break;
                        }
                        rhs = &symbol.data.type_decl.type;
                    },
                    else => break,
                }
            }
            type_decl.head.flags.remove(.resolving);
        }
    }
};

fn ctx(mr: *ModuleScopeResolver) *GeneralContext {
    return mr.ast.ctx;
}

fn top(mr: *ModuleScopeResolver) *node.Scope {
    return mr.topOrNull().?;
}

fn topOrNull(mr: *ModuleScopeResolver) ?*node.Scope {
    return mr.scopes.top();
}

fn push(mr: *ModuleScopeResolver, ref: *node.Scope) void {
    ref.parent = mr.topOrNull();
    mr.scopes.push(ref) catch @panic("OOM");
}

fn pop(mr: *ModuleScopeResolver, scope: *node.Scope) void {
    std.debug.assert(mr.scopes.pop() == scope);
}

fn insert(mr: *ModuleScopeResolver, symbol_: anytype) void {
    const position = if (@TypeOf(symbol_) != node.Symbol)
        symbol_.name.head.position
    else
        symbol_.head().position;
    const symbol = if (@TypeOf(symbol_) != node.Symbol) node.Symbol.fromSymbolLike(symbol_) else symbol_;
    if (mr.top().insert(mr.ctx().allocator, symbol)) |existing| {
        mr.raise(
            position,
            "{s} redeclared in this block; other declaration at {f}",
            .{
                symbol.name,
                mr.code.target(existing.head().position),
            },
        );
    }
}

inline fn raise(mr: *ModuleScopeResolver, at: Code.Offset, comptime fmt: []const u8, args: anytype) void {
    mr.code.raise(mr.ctx().error_out, at, fmt, args) catch unreachable;
}
