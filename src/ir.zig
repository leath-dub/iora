/// SSA based IR
const std = @import("std");
const GeneralContext = @import("GeneralContext.zig");

pub const Value = union(enum) {
    u64: u64,
    s64: i64,
    u32: u32,
    s32: i32,
    u16: u16,
    s16: i16,
    u8: u8,
    s8: i8,

    pub fn format(
        v: Value,
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (v) {
            inline else => |value, tag| {
                try writer.print("{t} {d}", .{ tag, value });
            },
        }
    }
};

pub const Register = enum(u32) {
    nil,
    _,

    pub fn format(
        r: Register,
        w: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        if (r == .nil) {
            try w.writeAll("nil");
        } else {
            try w.print("{d}", .{@intFromEnum(r)});
        }
    }
};

pub const Operation = enum {
    nil,
    let,
    add,
    sub,
    mul,
    div,
    jmp, // unconditional jump
    br, // conditional jump
    lt, // checks <
    gt, // checks >
    eq, // checks equality
    call,
    phi,
};

pub const operand_count = std.EnumMap(Operation, i8).init(.{
    .let = 1,
    .add = 2,
    .sub = 2,
    .mul = 2,
    .jmp = 1,
    .br = 2,
    .lt = 2,
    .gt = 2,
    .eq = 2,
    .call = -1,
    .phi = -1,
});

pub const Operand = union(enum) {
    value: Value,
    label: []const u8,
    symbol: []const u8,
    register: Register,

    pub fn format(
        o: Operand,
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (o) {
            .value => |v| try v.format(writer),
            .label => |l| try writer.print(":{s}", .{l}),
            .symbol => |s| try writer.print("{s}", .{s}),
            .register => |r| try writer.print("${f}", .{r}),
        }
    }
};

pub const Instruction = struct {
    op: Operation,
    id: Register = .nil,
    args: []Operand = &.{},

    pub fn format(
        inst: Instruction,
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        if (inst.op == .nil) {
            try writer.print("<optimized out>", .{});
            return;
        }
        try writer.print("{s}", .{@tagName(inst.op)});
        if (inst.id != .nil) {
            try writer.print(" ${d}", .{inst.id});
        }
        for (inst.args, 0..) |arg, i| {
            if (i != 0 or inst.id != .nil) {
                try writer.writeByte(',');
            }
            try writer.print(" {f}", .{arg});
        }
    }
};

pub const InstructionRef = u32;
pub const BlockRef = u32;

pub const User = struct {
    in: BlockRef,
    ins: InstructionRef,
};

pub const FunUnit = struct {
    blocks: std.ArrayList(Block) = .empty,
    sealed_blocks: std.AutoHashMapUnmanaged(BlockRef, void) = .empty,
    users: std.AutoHashMapUnmanaged(Register, std.ArrayList(User)) = .empty,
    current: BlockRef = 0,
    last_register: u32 = 0,
    last_var: u32 = 0,

    pub fn allocRegister(fu: *FunUnit, _inst: Instruction) Instruction {
        fu.last_register += 1;
        var inst = _inst;
        inst.id = @enumFromInt(fu.last_register);
        return inst;
    }

    pub fn allocVar(fu: *FunUnit) Var {
        fu.last_var += 1;
        return @enumFromInt(fu.last_var);
    }

    pub fn allocBlock(fu: *FunUnit, al: std.mem.Allocator) BlockRef {
        fu.blocks.append(al, .{}) catch @panic("OOM");
        return @intCast(fu.blocks.items.len - 1);
    }

    pub fn blockPtr(fu: *FunUnit, id: BlockRef) *Block {
        return &fu.blocks.items[id];
    }

    pub fn blockPtrConst(fu: *const FunUnit, id: BlockRef) *const Block {
        return &fu.blocks.items[id];
    }

    pub fn registerUsers(fu: *FunUnit, al: std.mem.Allocator, in: BlockRef, user: InstructionRef) void {
        const ins = fu.blockPtr(in).getByRef(user);
        fu.addUser(al, ins.id, in, user); // add self usage
        for (ins.args) |arg| {
            if (arg == .register) {
                fu.addUser(al, arg.register, in, user);
            }
        }
    }

    pub fn addUser(fu: *FunUnit, al: std.mem.Allocator, use: Register, in: BlockRef, user: InstructionRef) void {
        const res = fu.users.getOrPut(al, use) catch @panic("OOM");
        if (!res.found_existing) {
            res.value_ptr.* = .empty;
        }
        res.value_ptr.append(al, .{ .in = in, .ins = user }) catch @panic("OOM");
    }

    pub fn replaceUsers(fu: *FunUnit, al: std.mem.Allocator, use: Register, rep: Register) void {
        const users_opt = fu.users.getPtr(use);
        if (users_opt == null) {
            return;
        }
        const users = users_opt.?;

        for (users.items) |*user| {
            const in = fu.blockPtr(user.in);
            const ins = in.getByRef(user.ins);
            if (ins.id == use) {
                // Remove the phi node itself
                ins.op = .nil;
                continue;
            }
            // Replace any arguments pointing to this phi node
            for (ins.args) |*arg| {
                if (arg.* == .register and arg.register == use) {
                    arg.register = rep;
                }
            }
        }

        // Remove users entry
        users.deinit(al);
        _ = fu.users.remove(use);
    }

    pub fn deinit(fu: *FunUnit, al: std.mem.Allocator) void {
        for (fu.blocks.items) |*blk| {
            blk.deinit(al);
        }
        fu.blocks.deinit(al);
        fu.sealed_blocks.deinit(al);
        var it = fu.users.iterator();
        while (it.next()) |e| {
            e.value_ptr.*.deinit(al);
        }
        fu.users.deinit(al);
    }

    pub fn format(
        fu: FunUnit,
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        if (fu.blocks.items.len == 0) {
            try writer.writeAll("<empty function unit>");
            return;
        }
        try (BlockFormatter{ .ref = 0, .unit = &fu }).format(writer);
    }

    pub const hidden = true;
    pub const dont_walk = true;
};

// Unique identifier for a given variable inside a FunUnit
pub const Var = enum(u32) {
    nil,
    _,

    pub fn format(
        v: Var,
        w: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        if (v == .nil) {
            try w.writeAll("nil");
        } else {
            try w.print("{d}", .{@intFromEnum(v)});
        }
    }
};

pub const Block = struct {
    instructions: std.ArrayList(Instruction) = .empty,
    values: std.AutoHashMapUnmanaged(Var, Register) = .empty,
    incomplete_phis: std.AutoHashMapUnmanaged(Var, Register) = .empty,
    predecessors: std.ArrayList(BlockRef) = .empty,
    successors: std.ArrayList(BlockRef) = .empty,

    pub fn deinit(blk: *Block, al: std.mem.Allocator) void {
        blk.instructions.deinit(al);
        blk.values.deinit(al);
        blk.incomplete_phis.deinit(al);
        blk.predecessors.deinit(al);
        blk.successors.deinit(al);
    }

    pub fn add(blk: *Block, al: std.mem.Allocator, inst: Instruction) Register {
        blk.instructions.append(al, inst) catch @panic("OOM");
        return inst.id;
    }

    pub fn getByRefConst(blk: *const Block, id: InstructionRef) *const Instruction {
        return &blk.instructions.items[@intCast(id)];
    }

    pub fn getByRef(blk: *Block, id: InstructionRef) *Instruction {
        return &blk.instructions.items[@intCast(id)];
    }

    pub fn getInstructionRef(blk: *Block, reg: Register) InstructionRef {
        for (blk.instructions.items, 0..) |ins, i| {
            if (ins.id == reg) {
                return @intCast(i);
            }
        }
        unreachable;
    }

    pub fn lastInstruction(blk: *Block) InstructionRef {
        return @intCast(blk.instructions.items.len - 1);
    }

    pub fn assign(blk: *Block, al: std.mem.Allocator, v: Var, inst: Instruction) InstructionRef {
        std.debug.assert(inst.id != .nil);
        _ = blk.add(al, inst);
        blk.values.put(al, v, inst.id) catch @panic("OOM");
        return blk.lastInstruction();
    }

    pub fn addPredecessor(blk: *Block, al: std.mem.Allocator, pred: BlockRef) void {
        blk.predecessors.append(al, pred) catch @panic("OOM");
    }

    pub fn addSuccessor(blk: *Block, al: std.mem.Allocator, succ: BlockRef) void {
        blk.successors.append(al, succ) catch @panic("OOM");
    }

    pub fn addIncompletePhi(blk: *Block, al: std.mem.Allocator, id: Var, reg: Register) void {
        blk.incomplete_phis.put(al, id, reg) catch @panic("OOM");
    }
};

pub const BlockFormatter = struct {
    ref: BlockRef,
    unit: *const FunUnit,

    pub fn format(
        bf: BlockFormatter,
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try writer.print("Block({d}):\n", .{bf.ref});
        const blk = bf.unit.blockPtrConst(bf.ref);
        for (blk.instructions.items) |inst| {
            try writer.print("  {f}\n", .{inst});
        }
        for (blk.successors.items) |succ| {
            try (BlockFormatter{ .ref = succ, .unit = bf.unit }).format(writer);
        }
    }
};
