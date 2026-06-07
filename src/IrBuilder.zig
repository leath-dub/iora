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

pub fn enterIfStmt(ib: *IrBuilder, if_stmt: *node.IfStmt) Ast.ChildDisposition {
    const unit = ib.currentUnit();

    const then_block = ib.allocBlock();
    const else_block = ib.allocBlock();
    const join_block = ib.allocBlock();

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
                unit.allocRegister(ir.instruction1(.let, .{ .value = .{ .u64 = lit.value } })));
        },
        else => unreachable,
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
