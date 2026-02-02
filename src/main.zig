const std = @import("std");
const heap = std.heap;
const fs = std.fs;
const log = std.log;

const Code = @import("Code.zig");
const Lexer = @import("Lexer.zig");
const Ast = @import("Ast.zig");
const node = @import("node.zig");
const GeneralContext = @import("GeneralContext.zig");

const Cli = struct {
    input_path: []const u8,
};

fn parseArgs() error{ParseFailed}!Cli {
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
    var default_ctx = GeneralContext.Default.init();
    defer default_ctx.deinit();

    var ctx = default_ctx.general();

    const cli = parseArgs() catch std.process.exit(1);
    const input_file = fs.cwd().openFile(cli.input_path, .{}) catch |e| {
        const msg = openErrorMsg(e) orelse @errorName(e);
        log.err("opening file {s}: {s}\n", .{ cli.input_path, msg });
        std.process.exit(1);
    };
    defer input_file.close();

    var cst = ctx.createLifetime();
    defer cst.deinit();

    const text = try input_file.readToEndAlloc(ctx.allocator, std.math.maxInt(usize));
    const code = try Code.init(&cst, cli.input_path, try cst.allocator().dupe(u8, text));
    ctx.allocator.free(text);

    var lexer = Lexer.init(&ctx, &cst, code);
    var tok = lexer.peek();
    while (tok.type != .eof) : ({
        lexer.consume();
        tok = lexer.peek();
    }) {
        std.debug.print("{any}\n", .{tok});
    }

    var ast = Ast.init(&lexer);
    defer ast.deinit();

    // _ = try ast.add(node.VarDecl{ .binding = .{ .handle = 0 } }, 0, .{ .position = 30 });
    const ref = try ast.startNode(node.VarDecl{ .binding = .{ .handle = 0 } });
    defer ast.endNode(ref.handle);
}
