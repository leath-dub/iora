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
};

pub const Register = enum(usize) {
    nil,
    _,
};

pub const Operation = enum {
    def, // define a constant
    add,
    sub,
    mul,
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
    .def = 1,
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
};

pub const Instruction = struct {
    op: Operation,
    id: Register = .nil,
    args: [2]Operand = .{ .nil, .nil },
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

pub const FunUnit = struct {
    start: *Block = undefined,
    current: *Block = undefined,
    register: Register = .nil,

    pub fn allocRegister(fu: *FunUnit, _inst: Instruction) Instruction {
        fu.register += 1;
        var inst = _inst;
        inst.id = fu.register;
        return inst;
    }
};

pub const Block = struct {
    instructions: std.ArrayList(Instruction) = .empty,
    value_from_ident: std.StringHashMapUnmanaged(Register) = .empty,
    predecessors: std.ArrayList(*Block) = .empty,
    successors: std.ArrayList(*Block) = .empty,

    pub fn deinit(blk: *Block, al: std.mem.Allocator) void {
        blk.instructions.deinit(al);
        blk.value_from_ident.deinit(al);
        blk.predecessors.deinit(al);
        blk.successors.deinit(al);
    }

    pub fn add(blk: *Block, al: std.mem.Allocator, inst: Instruction) void {
        blk.instructions.append(al, inst) catch @panic("OOM");
    }

    pub fn assign(blk: *Block, al: std.mem.Allocator, ident: []const u8, inst: Instruction) void {
        std.debug.assert(inst.id != .nil);
        blk.add(al, inst);
        blk.value_from_ident.put(al, ident, inst.id);
    }

    pub fn addPredecessor(blk: *Block, al: std.mem.Allocator, pred: *Block) void {
        blk.predecessors.append(al, pred) catch @panic("OOM");
    }

    pub fn addSuccessor(blk: *Block, al: std.mem.Allocator, succ: *Block) void {
        blk.successors.append(al, succ) catch @panic("OOM");
    }
};
