const std = @import("std");
const heap = std.heap;
const fs = std.fs;
const log = std.log;

const Code = @import("Code.zig");
const Lexer = @import("Lexer.zig");

const Cli = struct {
    input_path: []const u8,
};

fn parseArgs() error{ ParseFailed }!Cli {
    var cli: Cli = undefined;

    var args = std.process.args();
    _ = args.next().?;

    if (args.next()) |file_arg| {
        cli.input_path = file_arg[0..];
    } else {
        log.err("usage: iotac <file to compile>", .{});
        return error.ParseFailed;
    }

    return cli;
}

fn openErrorMsg(e: fs.File.OpenError) ?[]const u8 {
    return switch (e) {
        error.FileNotFound => "file not found",
        error.AccessDenied => "access denied",
        else => null,
    };
}

pub fn main() !void {
    var dbg_alloc = heap.DebugAllocator(.{}){};
    defer {
        if (dbg_alloc.deinit() == .leak) {
            std.debug.assert(!dbg_alloc.detectLeaks());
        }
    }
    const allocator = dbg_alloc.allocator();

    const cli = parseArgs() catch std.process.exit(1);
    const input_file = fs.cwd().openFile(cli.input_path, .{}) catch |e| {
        const msg = openErrorMsg(e) orelse @errorName(e);
        log.err("opening file {s}: {s}\n", .{cli.input_path, msg});
        std.process.exit(1);
    };
    defer input_file.close();

    var arena = heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const text = try input_file.readToEndAlloc(allocator, std.math.maxInt(usize));
    const code = try Code.init(&arena, try arena.allocator().dupe(u8, text));
    allocator.free(text);

    var lexer = Lexer.init(&arena, code);
    var tok = lexer.peek();
    while (tok.type != .eof) : ({ lexer.consume(); tok = lexer.peek(); }) {
        std.debug.print("{any}\n", .{tok});
    }
}
