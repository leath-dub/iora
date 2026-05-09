//! This is a general context object that is passed to many sub routines for dependency
//! injection of most commonly shared resources.

const std = @import("std");
const Io = std.Io;
const heap = std.heap;
const fs = std.fs;
const mem = std.mem;

const assert = std.debug.assert;

io: Io,
error_out: *Io.Writer,
allocator: mem.Allocator,
scratch: *heap.ArenaAllocator,

const Self = @This();

pub fn createLifetime(c: *Self) heap.ArenaAllocator {
    return heap.ArenaAllocator.init(c.allocator);
}

pub const Default = struct {
    var writer_buf: [4096]u8 = undefined;

    io: Io,
    error_out: Io.File.Writer,
    debug_alloc: std.heap.DebugAllocator(.{}),
    scratch: ?std.heap.ArenaAllocator = null,

    pub fn init(io: Io) Default {
        return .{
            .io = io,
            .error_out = Io.File.stderr().writer(io, &writer_buf),
            .debug_alloc = .{},
        };
    }

    pub fn deinit(dc: *Default) void {
        if (dc.scratch != null) {
            dc.scratch.?.deinit();
        }
        if (dc.debug_alloc.deinit() == .leak) {
            assert(dc.debug_alloc.detectLeaks() == 0);
        }
        dc.error_out.interface.flush() catch unreachable;
    }

    pub fn general(dc: *Default) Self {
        if (dc.scratch == null) {
            dc.scratch = heap.ArenaAllocator.init(dc.debug_alloc.allocator());
        }
        return .{
            .io = dc.io,
            .error_out = &dc.error_out.interface,
            .allocator = dc.debug_alloc.allocator(),
            .scratch = &dc.scratch.?,
        };
    }
};

pub const Testing = struct {
    var writer_buf: [4096]u8 = undefined;

    io: Io,
    error_out: Io.File.Writer,
    scratch: ?std.heap.ArenaAllocator = null,

    pub fn init(io: Io) Testing {
        return .{
            .io = io,
            .error_out = Io.File.stderr().writer(io, &writer_buf),
        };
    }

    pub fn deinit(tc: *Testing) void {
        tc.error_out.interface.flush() catch unreachable;
    }

    pub fn general(tc: *Testing) Self {
        if (tc.scratch == null) {
            tc.scratch = heap.ArenaAllocator.init(std.testing.allocator);
        }
        return .{
            .io = tc.io,
            .error_out = &tc.error_out.interface,
            .allocator = std.testing.allocator,
            .scratch = &tc.scratch.?,
        };
    }
};
