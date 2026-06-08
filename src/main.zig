const std = @import("std");
const heap = std.heap;
const fs = std.fs;
const log = std.log;
const mem = std.mem;

const Code = @import("Code.zig");
const Lexer = @import("Lexer.zig");
const Ast = @import("Ast.zig");
const Parser = @import("Parser.zig");
const node = @import("node.zig");
const GeneralContext = @import("GeneralContext.zig");
const ty = @import("type.zig");
const ir = @import("ir.zig");

// Semantic passes
const ModuleScopeResolver = @import("ModuleScopeResolver.zig");
const LexicalScopeResolver = @import("LexicalScopeResolver.zig");
const TypeChecker = @import("TypeChecker.zig");
const IrBuilder = @import("IrBuilder.zig");
// const CBackend = @import("CBackend.zig");

const Cli = struct {
    input_path: []const u8,
};

fn parseArgs(init: *const std.process.Init) error{ParseFailed}!Cli {
    var cli: Cli = undefined;

    var args = init.minimal.args.iterate();
    _ = args.next().?;

    if (args.next()) |file_arg| {
        cli.input_path = file_arg[0..];
    } else {
        log.err("usage: iorac <file to compile>", .{});
        return error.ParseFailed;
    }

    return cli;
}

fn openErrorMsg(e: std.Io.File.OpenError) ?[]const u8 {
    return switch (e) {
        error.FileNotFound => "file not found",
        error.AccessDenied => "access denied",
        else => null,
    };
}

fn invokeListener(ast: *Ast, code: *const Code, listener: anytype) !void {
    if (@typeInfo(@TypeOf(listener)) != .pointer) {
        @compileError("listener must be a pointer");
    }
    Ast.walk(listener, &ast.root.?);
    if (@hasDecl(@TypeOf(listener.*), "deinit")) {
        listener.deinit();
    }
    if (code.errors != 0) {
        std.debug.print("{f}", .{ast});
        return error.SemanticAnalysisFailed;
    }
}

pub fn main(init: std.process.Init) !void {
    var default_ctx = GeneralContext.Default.init(init.io);
    defer default_ctx.deinit();

    var ctx = default_ctx.general();

    const cli = parseArgs(&init) catch std.process.exit(1);
    const input_file = std.Io.Dir.cwd().openFile(ctx.io, cli.input_path, .{}) catch |e| {
        const msg = openErrorMsg(e) orelse @errorName(e);
        log.err("opening file {s}: {s}\n", .{ cli.input_path, msg });
        std.process.exit(1);
    };
    defer input_file.close(ctx.io);

    var cst = ctx.createLifetime();
    defer cst.deinit();

    var rdr = input_file.reader(ctx.io, &.{});
    const text = try rdr.interface.allocRemaining(ctx.allocator, .unlimited);
    var code = try Code.init(&cst, cli.input_path, try cst.allocator().dupe(u8, text));
    ctx.allocator.free(text);

    var parser = Parser.init(&ctx, Lexer.init(&ctx, &cst, &code));
    var ast = parser.parse();
    defer ast.deinit();

    if (code.errors != 0) {
        std.debug.print("{f}", .{ast});
        return error.SyntaxAnalysisFailed;
    }

    var msr = ModuleScopeResolver.init(&ast, &code);
    try invokeListener(&ast, &code, &msr);

    var lsr = LexicalScopeResolver.init(&ast, &code);
    try invokeListener(&ast, &code, &lsr);

    var type_store = ty.Store.init(&ctx);
    defer type_store.deinit();

    var tc = TypeChecker.init(&ast, &code, &type_store);
    try invokeListener(&ast, &code, &tc);

    var ib = IrBuilder.init(&ctx, &type_store);
    defer ib.deinit();
    Ast.walk(&ib, &ast.root.?);

    var buf: [4096]u8 = undefined;
    var stdout = std.Io.File.stdout().writer(ctx.io, &buf);

    const FunUnitCleaner = struct {
        ctx: *GeneralContext,
        debug: *std.Io.Writer,

        pub fn enterFunDecl(fuc: *@This(), fun_decl: *node.FunDecl) void {
            (ir.FunUnitFormatter{ .ctx = fuc.ctx, .unit = fun_decl.unit }).format(fuc.debug) catch @panic("io error");
            fun_decl.unit.deinit(fuc.ctx.allocator);
        }
    };

    defer {
        var fuc: FunUnitCleaner = .{ .ctx = &ctx, .debug = &stdout.interface };
        Ast.walk(&fuc, &ast.root.?);
        stdout.flush() catch unreachable;
    }

    // var cb = CBackend.init(&ctx, &type_store);
    // defer cb.deinit();
    // Ast.walk(&cb, &ast.root.?);

    // var buf: [4096]u8 = undefined;
    // var stdout = std.Io.File.stdout().writer(ctx.io, &buf);
    // defer stdout.flush() catch unreachable;
    // try cb.emit(&stdout.interface);

    std.debug.print("{f}", .{ast});
}
