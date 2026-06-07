const std = @import("std");
const node = @import("node.zig");
const GeneralContext = @import("GeneralContext.zig");
const ty = @import("type.zig");
const TypeRef = @import("type_ref.zig").TypeRef;
const util = @import("util.zig");
const common = @import("common.zig");
const Ast = @import("Ast.zig");

const ir = @import("ir.zig");

const IrBuilder = @This();

ctx: *GeneralContext,
type_store: *ty.Store,
fun_units: util.ChunkedStack(*ir.FunUnit),

pub fn enterFunDecl(ib: *IrBuilder, fun_decl: *node.FunDecl) void {
    if (fun_decl.body == null) {
        return;
    }

    ib.beginUnit(&fun_decl.unit);
}

pub fn enterFunParam(ib: *IrBuilder, fun_param: *node.FunParam) void {
    common.todo(fun_param.type_ref == .u64, "only u64 types currently supported", .{});
    const unit = ib.currentUnit();
    unit.blockPtr(unit.current).assign(
        ib.ctx.allocator,
        allocParam(unit, fun_param),
        unit.allocRegister(ir.instruction1(.let, .{ .value = .{ .u64 = 0 } })),
    );
}

pub fn exitVarDecl(ib: *IrBuilder, var_decl: *node.VarDecl) void {
    common.todo(var_decl.type_ref == .u64, "only u64 types currently supported", .{});
    const unit = ib.currentUnit();
    var value: ir.Operand = .{ .value = .{ .u64 = 0 } };
    if (var_decl.init_expr) |ex| {
        value = .{ .register = ex.registerConst().* };
    }
    unit.blockPtr(unit.current).assign(
        ib.ctx.allocator,
        allocVar(unit, var_decl),
        unit.allocRegister(ir.instruction1(.let, value)),
    );
}

pub fn enterIfStmt(ib: *IrBuilder, if_stmt: *node.IfStmt) Ast.ChildDisposition {
    const unit = ib.currentUnit();
    common.todo(if_stmt.else_arm != null, "must have else arm for now", .{});

    const then_block = ib.allocBlock();
    const else_block = ib.allocBlock();
    const join_block = ib.allocBlock();

    unit.blockPtr(unit.current).addSuccessor(ib.ctx.allocator, then_block);
    unit.blockPtr(unit.current).addSuccessor(ib.ctx.allocator, else_block);

    unit.blockPtr(then_block).addPredecessor(ib.ctx.allocator, unit.current);
    unit.blockPtr(else_block).addPredecessor(ib.ctx.allocator, unit.current);

    unit.current = then_block;
    Ast.walk(ib, &if_stmt.then_arm);
    unit.blockPtr(then_block).addSuccessor(ib.ctx.allocator, join_block);

    unit.current = else_block;
    Ast.walk(ib, &if_stmt.else_arm);
    unit.blockPtr(else_block).addSuccessor(ib.ctx.allocator, join_block);

    unit.blockPtr(join_block).addPredecessor(ib.ctx.allocator, then_block);
    unit.blockPtr(join_block).addPredecessor(ib.ctx.allocator, else_block);

    unit.current = join_block;

    return .skip;
}

pub fn exitCallExpr(ib: *IrBuilder, call_expr: *node.CallExpr) void {
    // TODO A LOT: here
    const callable_td = ib.type_store.get(call_expr.callable.getType().*).data;
    if (callable_td.isBuiltinCallable(ib.type_store.*)) {
        // It is builtin cast, just forward the register of the subexpression
        std.debug.assert(call_expr.call_bindings.?.bindings.len == 1);
        call_expr.register = call_expr.call_bindings.?.bindings[0].expr.?.registerConst().*;
    }
}

pub fn exitTokenExpr(ib: *IrBuilder, token_expr: *node.TokenExpr) void {
    if (ib.type_store.get(token_expr.type_ref).data == .type_of) {
        return;
    }
    common.todo(token_expr.type_ref == .u64, "only u64 types currently supported", .{});
    switch (token_expr.token.type) {
        .int_lit => {
            const lit = token_expr.token.lit.?.int;
            const unit = ib.currentUnit();
            token_expr.register = unit.blockPtr(unit.current).add(
                ib.ctx.allocator,
                unit.allocRegister(ir.instruction1(.let, .{ .value = .{ .u64 = lit.value } })),
            );
        },
        else => unreachable,
    }
}

pub fn exitBinExpr(ib: *IrBuilder, bin_expr: *node.BinExpr) void {
    switch (bin_expr.op.type) {
        .plus, .minus, .star, .slash, .dequal, .lt, .gt => {
            const op: ir.Operation = switch (bin_expr.op.type) {
                .plus => .add,
                .minus => .sub,
                .star => .mul,
                .slash => .div,
                .dequal => .eq,
                .lt => .lt,
                .gt => .gt,
                else => unreachable,
            };
            const unit = ib.currentUnit();
            bin_expr.register = unit.blockPtr(unit.current).add(
                ib.ctx.allocator,
                unit.allocRegister(
                    ir.instruction2(
                        op,
                        .{ .register = bin_expr.left.register().* },
                        .{ .register = bin_expr.right.register().* },
                    ),
                ),
            );
        },
        else => {},
    }
}

pub fn exitIdentExpr(ib: *IrBuilder, ident_expr: *node.IdentExpr) void {
    common.todo(!ident_expr.is_inferred, "inferred names not supported yet", .{});
    switch (ident_expr.resolves_to.?.data) {
        .var_decl => |vd| {
            const unit = ib.currentUnit();
            if (unit.blockPtr(unit.current).values.get(vd.id)) |reg| {
                ident_expr.register = reg;
            } else {
                // TODO: We need to search in predecessors
                common.todoNoReturn("referenced before assigned in this block: {s}", .{vd.name.text()});
            }
        },
        .fun_param => |fp| {
            const unit = ib.currentUnit();
            if (unit.blockPtr(unit.current).values.get(fp.id)) |reg| {
                ident_expr.register = reg;
            } else {
                // TODO: We need to search in predecessors
                common.todoNoReturn("referenced before assigned in this block: {s}", .{fp.name.text()});
            }
        },
        else => |x| common.todoNoReturn("todo ident cg: {any}", .{x}),
    }
}

pub fn exitFunDecl(ib: *IrBuilder, fun_decl: *node.FunDecl) void {
    ib.endUnit(&fun_decl.unit);
}

pub fn init(ctx: *GeneralContext, type_store: *ty.Store) IrBuilder {
    return .{
        .ctx = ctx,
        .type_store = type_store,
        .fun_units = .init(ctx.allocator),
    };
}

pub fn deinit(ib: *IrBuilder) void {
    ib.fun_units.deinit();
}

fn currentUnit(ib: *IrBuilder) *ir.FunUnit {
    return ib.fun_units.top().?;
}

fn allocVar(unit: *ir.FunUnit, var_decl: *node.VarDecl) ir.Var {
    var_decl.id = unit.allocVar();
    return var_decl.id;
}

fn allocDef(unit: *ir.FunUnit, def_decl: *node.DefDecl) ir.Var {
    def_decl.id = unit.allocVar();
    return def_decl.id;
}

fn allocParam(unit: *ir.FunUnit, fun_param: *node.FunParam) ir.Var {
    fun_param.id = unit.allocVar();
    return fun_param.id;
}

fn allocBlock(ib: *IrBuilder) ir.BlockRef {
    return ib.currentUnit().allocBlock(ib.ctx.allocator);
}

fn beginUnit(ib: *IrBuilder, unit: *ir.FunUnit) void {
    ib.fun_units.push(unit) catch @panic("OOM");
    unit.current = ib.allocBlock();
}

fn endUnit(ib: *IrBuilder, unit: *ir.FunUnit) void {
    std.debug.assert(ib.fun_units.pop() == unit);
}
