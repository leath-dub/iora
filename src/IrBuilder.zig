//! This is an implementation of Braun et al.
//! "Simple and Efficient Construction of Static Single Assignment Form":
//!     https://c9x.me/compile/bib/braun13cc.pdf

// TODO: add on the fly optimizations

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
arena: std.heap.ArenaAllocator,

pub fn enterFunDecl(ib: *IrBuilder, fun_decl: *node.FunDecl) void {
    if (fun_decl.body == null) {
        return;
    }

    ib.beginUnit(fun_decl.x(.unit));
}

pub fn enterFunParam(ib: *IrBuilder, fun_param: *node.FunParam) void {
    common.todo(fun_param.xv(.type) == .u64, "only u64 types currently supported", .{});
    const unit = ib.currentUnit();
    const user = unit.blockPtr(unit.current).assign(
        ib.ctx.allocator,
        allocParam(unit, fun_param),
        unit.allocRegister(ib.ins(1, .let, .{.{ .value = .{ .u64 = 0 } }})),
    );
    unit.registerUsers(ib.ctx.allocator, unit.current, user);
}

pub fn exitVarDecl(ib: *IrBuilder, var_decl: *node.VarDecl) void {
    common.todo(var_decl.xv(.type) == .u64, "only u64 types currently supported", .{});
    const unit = ib.currentUnit();
    var value: ir.Operand = .{ .value = .{ .u64 = 0 } };
    if (var_decl.init_expr) |ex| {
        value = .{ .register = ex.registerConst().* };
    }
    const user = unit.blockPtr(unit.current).assign(
        ib.ctx.allocator,
        allocVar(unit, var_decl),
        unit.allocRegister(ib.ins(1, .let, .{value})),
    );
    unit.registerUsers(ib.ctx.allocator, unit.current, user);
    if (value == .register) {
        unit.addUser(ib.ctx.allocator, value.register, unit.current, user);
    }
}

pub fn enterIfStmt(ib: *IrBuilder, if_stmt: *node.IfStmt) Ast.ChildDisposition {
    const unit = ib.currentUnit();

    if (if_stmt.else_arm) |*else_arm| {
        const then_block = ib.allocBlock();
        const else_block = ib.allocBlock();
        const join_block = ib.allocBlock();
        const entry_block = unit.current;

        Ast.walk(ib, &if_stmt.cond.expr);
        const cond = if_stmt.cond.expr.register().*;
        _ = unit.blockPtr(entry_block).add(ib.ctx.allocator, ib.ins(3, .br, .{ .{ .register = cond }, .{ .block = then_block }, .{ .block = else_block } }));
        unit.registerUsers(ib.ctx.allocator, entry_block, unit.blockPtr(entry_block).lastInstruction());

        unit.blockPtr(entry_block).addSuccessor(ib.ctx.allocator, then_block);
        unit.blockPtr(entry_block).addSuccessor(ib.ctx.allocator, else_block);

        unit.blockPtr(then_block).addPredecessor(ib.ctx.allocator, entry_block);
        unit.blockPtr(else_block).addPredecessor(ib.ctx.allocator, entry_block);

        ib.sealBlock(unit, then_block);
        ib.sealBlock(unit, else_block);

        unit.current = then_block;
        Ast.walk(ib, &if_stmt.then_arm);
        const then_exit_block = unit.current;
        unit.blockPtr(then_exit_block).addSuccessor(ib.ctx.allocator, join_block);
        unit.blockPtr(join_block).addPredecessor(ib.ctx.allocator, then_exit_block);

        unit.current = else_block;
        Ast.walk(ib, else_arm);
        const else_exit_block = unit.current;
        unit.blockPtr(else_exit_block).addSuccessor(ib.ctx.allocator, join_block);
        unit.blockPtr(join_block).addPredecessor(ib.ctx.allocator, else_exit_block);

        ib.sealBlock(unit, join_block);
        unit.current = join_block;
    } else {
        const then_block = ib.allocBlock();
        const join_block = ib.allocBlock();
        const entry_block = unit.current;

        Ast.walk(ib, &if_stmt.cond.expr);
        const cond = if_stmt.cond.expr.register().*;
        _ = unit.blockPtr(entry_block).add(ib.ctx.allocator, ib.ins(3, .br, .{ .{ .register = cond }, .{ .block = then_block }, .{ .block = join_block } }));
        unit.registerUsers(ib.ctx.allocator, entry_block, unit.blockPtr(entry_block).lastInstruction());

        unit.blockPtr(entry_block).addSuccessor(ib.ctx.allocator, then_block);
        unit.blockPtr(then_block).addPredecessor(ib.ctx.allocator, entry_block);

        ib.sealBlock(unit, then_block);

        unit.current = then_block;
        Ast.walk(ib, &if_stmt.then_arm);
        const then_exit_block = unit.current;
        unit.blockPtr(then_exit_block).addSuccessor(ib.ctx.allocator, join_block);
        unit.blockPtr(join_block).addPredecessor(ib.ctx.allocator, then_exit_block);

        unit.blockPtr(entry_block).addSuccessor(ib.ctx.allocator, join_block);
        unit.blockPtr(join_block).addPredecessor(ib.ctx.allocator, entry_block);

        ib.sealBlock(unit, join_block);
        unit.current = join_block;
    }

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
                unit.allocRegister(ib.ins(1, .let, .{.{ .value = .{ .u64 = lit.value } }})),
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
                    ib.ins(
                        2,
                        op,
                        .{
                            .{ .register = bin_expr.left.register().* },
                            .{ .register = bin_expr.right.register().* },
                        },
                    ),
                ),
            );
        },
        else => {},
    }
}

pub fn exitAssign(ib: *IrBuilder, assign: *node.Assign) void {
    common.todo(assign.lvalue == .ident_expr, "currently assignment to plain ident is only supported", .{});
    const id = assign.lvalue.ident_expr.resolves_to.?.varId().?;
    const unit = ib.currentUnit();
    const user = unit.blockPtr(unit.current).assign(
        ib.ctx.allocator,
        id,
        unit.allocRegister(
            ib.ins(1, .let, .{.{ .register = assign.rvalue.registerConst().* }}),
        ),
    );
    unit.registerUsers(ib.ctx.allocator, unit.current, user);
}

// This hook does equivalent work of `readVariable` function as defined
// by Braun et al.
pub fn exitIdentExpr(ib: *IrBuilder, ident_expr: *node.IdentExpr) void {
    common.todo(!ident_expr.is_inferred, "inferred names not supported yet", .{});
    ident_expr.register = ib.readVar(ib.currentUnit().current, ident_expr.resolves_to.?.varId().?);
}

pub fn exitFunDecl(ib: *IrBuilder, fun_decl: *node.FunDecl) void {
    ib.endUnit(fun_decl.x(.unit));
}

pub fn init(ctx: *GeneralContext, type_store: *ty.Store) IrBuilder {
    return .{
        .ctx = ctx,
        .type_store = type_store,
        .fun_units = .init(ctx.allocator),
        .arena = .init(ctx.allocator),
    };
}

pub fn deinit(ib: *IrBuilder) void {
    ib.fun_units.deinit();
    ib.arena.deinit();
}

fn currentUnit(ib: *IrBuilder) *ir.FunUnit {
    return ib.fun_units.top().?;
}

fn readVar(ib: *IrBuilder, blk_ref: ir.BlockRef, id: ir.Var) ir.Register {
    const blk = ib.currentUnit().blockPtr(blk_ref);
    if (blk.values.get(id)) |reg| {
        return reg;
    }
    return ib.readVarRec(blk_ref, id);
}

fn readVarRec(ib: *IrBuilder, blk_ref: ir.BlockRef, id: ir.Var) ir.Register {
    const unit = ib.currentUnit();
    const blk = unit.blockPtr(blk_ref);
    var reg: ir.Register = .nil;
    if (!unit.sealed_blocks.contains(blk_ref)) {
        reg = blk.add(
            ib.ctx.allocator,
            unit.allocRegister(ib.ins(0, .phi, .{})),
        );
        unit.registerUsers(ib.ctx.allocator, blk_ref, blk.lastInstruction());
        blk.addIncompletePhi(ib.ctx.allocator, id, reg);
    } else if (blk.predecessors.items.len == 1) {
        // Single predeccessor, no phi node needed
        reg = ib.readVar(blk.predecessors.items[0], id);
    } else {
        // Break cycles with operandless phi
        reg = blk.add(
            ib.ctx.allocator,
            unit.allocRegister(ib.ins(0, .phi, .{})),
        );
        unit.registerUsers(ib.ctx.allocator, blk_ref, blk.lastInstruction());
        blk.writeVar(ib.ctx.allocator, id, reg);
        reg = ib.addPhiOperands(blk_ref, id, reg);
    }
    blk.writeVar(ib.ctx.allocator, id, reg);
    return reg;
}

fn addPhiOperands(ib: *IrBuilder, blk_ref: ir.BlockRef, id: ir.Var, phi: ir.Register) ir.Register {
    const unit = ib.currentUnit();
    const blk = unit.blockPtr(blk_ref);

    const phi_ins = blk.getInstructionRef(phi);
    const ins_ = blk.getByRef(phi_ins);
    std.debug.assert(ins_.args.len == 0);

    var args: std.AutoHashMapUnmanaged(ir.Register, void) = .empty;
    defer _ = ib.ctx.scratch.reset(.retain_capacity);

    for (blk.predecessors.items) |pred| {
        const reg = ib.readVar(pred, id);
        args.put(ib.ctx.scratch.allocator(), reg, {}) catch @panic("OOM");
        unit.addUser(ib.ctx.allocator, reg, pred, phi_ins);
    }

    var it = args.keyIterator();
    var args_v: std.ArrayList(ir.Operand) = .empty;
    while (it.next()) |r| {
        args_v.append(ib.ctx.scratch.allocator(), .{ .register = r.* }) catch @panic("OOM");
    }

    ins_.args = ib.arena.allocator().dupe(ir.Operand, args_v.items) catch @panic("OOM");

    // 'tryRemoveTrivialPhi' allocates in scratch allocator
    defer _ = ib.ctx.scratch.reset(.retain_capacity);
    return ib.tryRemoveTrivialPhi(blk_ref, phi);
}

fn tryRemoveTrivialPhi(ib: *IrBuilder, blk_ref: ir.BlockRef, phi: ir.Register) ir.Register {
    const unit = ib.currentUnit();
    const blk = unit.blockPtr(blk_ref);

    var same: ir.Register = .nil;
    const ins_ref = blk.getInstructionRef(phi);
    const ins_ = blk.getByRef(ins_ref);
    for (ins_.args) |arg| {
        if (arg.register == same or arg.register == phi) {
            continue;
        }
        if (same != .nil) {
            return phi;
        }
        same = arg.register;
    }

    // if (same == .nil) {
    //     // Read before assigned
    // }

    var users_opt: ?[]ir.User = null;
    if (unit.users.get(phi)) |users| {
        users_opt = ib.ctx.scratch.allocator().dupe(ir.User, users.items) catch @panic("OOM");
    }

    unit.replaceUsers(ib.ctx.allocator, phi, same);

    if (users_opt) |users| {
        for (users) |user| {
            if (user.in == blk_ref and user.ins == ins_ref) {
                continue;
            }
            const user_ins = unit.blockPtr(user.in).getByRef(user.ins);
            if (user_ins.op == .phi) {
                _ = ib.tryRemoveTrivialPhi(user.in, user_ins.id);
            }
        }
    }

    return same;
}

fn sealBlock(ib: *IrBuilder, fu: *ir.FunUnit, blk_ref: ir.BlockRef) void {
    const blk = fu.blockPtr(blk_ref);
    var it = blk.incomplete_phis.iterator();
    while (it.next()) |e| {
        _ = ib.addPhiOperands(blk_ref, e.key_ptr.*, e.value_ptr.*);
    }
    fu.sealed_blocks.put(ib.ctx.allocator, blk_ref, {}) catch @panic("OOM");
}

fn ins(ib: *IrBuilder, comptime arg_count: usize, op: ir.Operation, args: [arg_count]ir.Operand) ir.Instruction {
    if (arg_count == 0) {
        return .{ .op = op };
    }
    return .{
        .op = op,
        .args = ib.arena.allocator().dupe(ir.Operand, &args) catch @panic("OOM"),
    };
}

fn allocVar(unit: *ir.FunUnit, var_decl: *node.VarDecl) ir.Var {
    var_decl.x(.id).* = unit.allocVar();
    return var_decl.xv(.id);
}

fn allocDef(unit: *ir.FunUnit, def_decl: *node.DefDecl) ir.Var {
    def_decl.x(.id).* = unit.allocVar();
    return def_decl.xv(.id);
}

fn allocParam(unit: *ir.FunUnit, fun_param: *node.FunParam) ir.Var {
    fun_param.x(.id).* = unit.allocVar();
    return fun_param.xv(.id);
}

fn allocBlock(ib: *IrBuilder) ir.BlockRef {
    return ib.currentUnit().allocBlock(ib.ctx.allocator);
}

fn beginUnit(ib: *IrBuilder, unit: *ir.FunUnit) void {
    ib.fun_units.push(unit) catch @panic("OOM");
    unit.current = ib.allocBlock();
    // First block has no predecessors so is always sealed by default
    ib.sealBlock(unit, unit.current);
}

fn endUnit(ib: *IrBuilder, unit: *ir.FunUnit) void {
    std.debug.assert(ib.fun_units.pop() == unit);
}
