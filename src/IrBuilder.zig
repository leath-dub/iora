const std = @import("std");
const node = @import("node.zig");
const GeneralContext = @import("GeneralContext.zig");
const ty = @import("type.zig");
const TypeRef = @import("type_ref.zig").TypeRef;
const util = @import("util.zig");
const common = @import("common.zig");

const ir = @import("ir.zig");

const IrBuilder = @This();

ctx: *GeneralContext,
block_arena: *std.heap.ArenaAllocator,
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
    unit.current.assign(
        ib.ctx.allocator,
        fun_param.name.text(),
        unit.allocRegister(ir.instruction1(.def, .{ .value = .{ .u64 = 0 } })),
    );
}

pub fn exitFunDecl(ib: *IrBuilder, fun_decl: *node.FunDecl) void {
    ib.endUnit(&fun_decl.unit);
}

pub fn init(ctx: *GeneralContext, type_store: *ty.Store, block_arena: *std.heap.ArenaAllocator) IrBuilder {
    return .{
        .ctx = ctx,
        .type_store = type_store,
        .fun_unit = .init(ctx.allocator),
        .block_arena = block_arena,
    };
}

pub fn deinit(ib: *IrBuilder) void {
    ib.fun_units.deinit();
}

fn beginUnit(ib: *IrBuilder, unit: *ir.FunUnit) void {
    unit.start = ib.block_arena.allocator().create(ir.Block) catch @panic("OOM");
    unit.current = unit.start;
    ib.fun_units.push(unit);
}

fn endUnit(ib: *IrBuilder, unit: *ir.FunUnit) void {
    std.debug.assert(ib.fun_units.pop().? == unit);
}
