//! This is a general context object that is passed to many sub routines for dependency
//! injection of most commonly shared resources.

const std = @import("std");
const Io = std.Io;
const heap = std.heap;
const fs = std.fs;
const mem = std.mem;

const assert = std.debug.assert;

error_out: *Io.Writer,
allocator: mem.Allocator,

const Self = @This();

pub fn createLifetime(c: *Self) heap.ArenaAllocator {
    return heap.ArenaAllocator.init(c.allocator);
}

pub const Default = struct {
    var writer_buf: [4096]u8 = undefined;

    error_out: fs.File.Writer,
    debug_alloc: std.heap.DebugAllocator(.{}),

    pub fn init() Default {
        return .{
            .error_out = fs.File.stderr().writer(&writer_buf),
            .debug_alloc = .{},
        };
    }

    pub fn deinit(dc: *Default) void {
        if (dc.debug_alloc.deinit() == .leak) {
            assert(!dc.debug_alloc.detectLeaks());
        }
        dc.error_out.interface.flush() catch unreachable;
    }

    pub fn general(dc: *Default) Self {
        return .{
            .error_out = &dc.error_out.interface,
            .allocator = dc.debug_alloc.allocator(),
        };
    }
};
