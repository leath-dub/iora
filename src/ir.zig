/// SSA based IR
const std = @import("std");

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
                try writer.print("{t} {d}", .{tag, value});
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
    arg, // adds argument to next function call
    call,
    phi,
};

pub const operand_count = std.EnumMap(Operation, u8).init(.{
    .let = 1,
    .add = 2,
    .sub = 2,
    .mul = 2,
    .jmp = 1,
    .br = 2,
    .lt = 2,
    .gt = 2,
    .eq = 2,
    .arg = 1,
    .call = 1,
    .phi = 1, // phi in our IR only takes one operand as we represent
    // phi(x, y, z) as
    //
    // phi x
    // phi y
    // phi z
});

pub const Operand = union(enum) {
    nil,
    value: Value,
    label: []const u8,
    symbol: []const u8,
    register: Register,

    pub fn format(
        o: Operand,
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (o) {
            .nil => try writer.writeAll("nil"),
            .value => |v| try v.format(writer),
            .label => |l| try writer.print(":{s}", .{l}),
            .symbol => |s| try writer.print("{s}", .{s}),
            .register => |r| try writer.print("%{f}", .{r}),
        }
    }
};

pub const Instruction = struct {
    op: Operation,
    id: Register = .nil,
    args: [2]Operand = .{ .nil, .nil },

    pub fn format(
        inst: Instruction,
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try writer.print("{s}", .{@tagName(inst.op)});
        if (inst.id != .nil) {
            try writer.print(" %{d},", .{inst.id});
        }
        try writer.print(" {f}", .{inst.args[0]});
        if (inst.args[1] != .nil) {
            try writer.print(", {f}", .{inst.args[0]});
        }
    }
};

pub fn instruction0(op: Operation) Instruction {
    return .{ .op = op };
}

pub fn instruction1(op: Operation, arg: Operand) Instruction {
    return .{ .op = op, .args = .{ arg, .nil } };
}

pub fn instruction2(op: Operation, arg0: Operand, arg1: Operand) Instruction {
    return .{ .op = op, .args = .{ arg0, arg1 } };
}

pub const BlockRef = u32;

pub const FunUnit = struct {
    blocks: std.ArrayList(Block) = .empty,
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

    pub fn deinit(fu: *FunUnit, al: std.mem.Allocator) void {
        for (fu.blocks.items) |*blk| {
            blk.deinit(al);
        }
        fu.blocks.deinit(al);
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
    predecessors: std.ArrayList(BlockRef) = .empty,
    successors: std.ArrayList(BlockRef) = .empty,

    pub fn deinit(blk: *Block, al: std.mem.Allocator) void {
        blk.instructions.deinit(al);
        blk.values.deinit(al);
        blk.predecessors.deinit(al);
        blk.successors.deinit(al);
    }

    pub fn add(blk: *Block, al: std.mem.Allocator, inst: Instruction) Register {
        blk.instructions.append(al, inst) catch @panic("OOM");
        return inst.id;
    }

    pub fn assign(blk: *Block, al: std.mem.Allocator, v: Var, inst: Instruction) void {
        std.debug.assert(inst.id != .nil);
        _ = blk.add(al, inst);
        blk.values.put(al, v, inst.id) catch @panic("OOM");
    }

    pub fn addPredecessor(blk: *Block, al: std.mem.Allocator, pred: BlockRef) void {
        blk.predecessors.append(al, pred) catch @panic("OOM");
    }

    pub fn addSuccessor(blk: *Block, al: std.mem.Allocator, succ: BlockRef) void {
        blk.successors.append(al, succ) catch @panic("OOM");
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
