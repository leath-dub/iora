const std = @import("std");
const heap = std.heap;
const log = std.log;

const Code = @import("Code.zig");

pub const TokenType = enum {
    eof,
    synthesized,
    illegal,
    empty_string,
    lbracket,
    rbracket,
    lparen,
    rparen,
    lbrace,
    rbrace,
    colon,
    semicolon,
    scope,
    comma,
    bang,
    qmark,
    dot,
    ddot,
    plus,
    plus_equal,
    minus,
    minus_equal,
    btick,
    star,
    star_equal,
    slash,
    slash_equal,
    equal,
    dequal,
    not_equal,
    lt,
    lt_equal,
    gt,
    gt_equal,
    pipe,
    pipe_equal,
    amper,
    amper_equal,
    perc,
    perc_equal,
    lshift,
    lshift_equal,
    rshift,
    rshift_equal,
    inc,
    dec,
    arrow,
    rune_lit,
    string_lit,
    integer_lit,
    floating_lit,
    identifier,
    kw_not,
    kw_and,
    kw_or,
    kw_fun,
    kw_if,
    kw_while,
    kw_case,
    kw_else,
    kw_for,
    kw_defer,
    kw_let,
    kw_var,
    kw_type,
    kw_struct,
    kw_enum,
    kw_s8,
    kw_u8,
    kw_s16,
    kw_u16,
    kw_s32,
    kw_u32,
    kw_s64,
    kw_u64,
    kw_f32,
    kw_f64,
    kw_return,
    kw_string,
    kw_unit,
    kw_bool,
    kw_true,
    kw_false,
    kw_extern,
};

pub const Base = enum {
    hex,
    decimal,
    binary,
    octal,
};

pub const IntegerSuffix = struct {
    bits: u8,
    signed: bool,
};

pub const FloatingSuffix = struct {
    bits: u8,
};

pub const Suffix = union(enum) {
    integer: IntegerSuffix,
    floating: FloatingSuffix,
};

pub const IntegerLit = struct {
    span: []const u8,
    base: Base,
    value: u64,
    suffix: IntegerSuffix = .{ .bits = 32, .signed = false },
};

pub const Exponent = struct {
    span: []const u8,
    value: u64,
    signed: bool,
};

pub const FloatingLit = struct {
    span: []const u8,
    integer: IntegerLit,
    fractional: ?IntegerLit = null,
    exponent: ?Exponent = null,
    suffix: FloatingSuffix = .{ .bits = 32 },
};

pub const RuneLit = struct {
    value: u32,
};

pub const StringLit = struct {
    value: []const u8,
};

pub const Token = struct {
    type: TokenType,
    span: []const u8,
    literal: union(enum) {
        integer: IntegerLit,
        floating: FloatingLit,
        rune: RuneLit,
        string: StringLit,
        none: void,
    } = .none,

    pub fn offset(t: Token, code: Code) usize {
        return @as(usize, @intFromPtr(code.text.ptr)) - @as(usize, @intFromPtr(t.span.ptr));
    }
    pub fn after(t: Token, code: Code) usize {
        return t.offset(code) + t.span.len;
    }
};

code: Code,
arena: *heap.ArenaAllocator,
cache: ?Token = null,
cursor: usize = 0,

const Lexer = @This();

pub fn init(arena: *heap.ArenaAllocator, code: Code) Lexer {
    var l: Lexer = .{
        .code = code,
        .arena = arena,
    };
    l.skipWhitespace();
    return l;
}

pub fn skipWhitespace(l: *Lexer) void {
    var in_comment = false;
    l.cursor += out: for (l.code.text[l.cursor..], 0..) |ch, shift| {
        if (in_comment) {
            if (ch != '\n') {
                continue;
            }
            in_comment = false;
        }
        switch (ch) {
            '\n', '\t', '\r', ' ' => {},
            '/' => if (l.ahead('/')) {
                in_comment = true;
            },
            else => break :out shift,
        }
    } else unreachable;
}

fn token(l: Lexer, ty: TokenType, len: usize) Token {
    return .{
        .type = ty,
        .span = l.code.text[l.cursor..][0..len],
    };
}

fn scan(l: Lexer, fwd: usize) ?u8 {
    if (l.cursor + fwd < l.code.text.len) {
        return l.code.text[l.cursor + fwd];
    }
    return null;
}

fn ahead_n(l: Lexer, ch: u8, fwd: usize) bool {
    if (l.scan(fwd)) |sc| {
        return ch == sc;
    }
    return false;
}

fn ahead(l: Lexer, ch: u8) bool {
    return l.ahead_n(ch, 1);
}

fn current(l: Lexer) u8 {
    return l.code.text[l.cursor];
}

pub fn peek(l: *Lexer) Token {
    if (l.cache) |tok| {
        if (tok.offset(l.code) == l.cursor) {
            return tok;
        }
    }
    if (l.cursor >= l.code.text.len) {
        return l.token(.eof, 0);
    }

    return switch (l.current()) {
        '[' => l.token(.lbracket, 1),
        ']' => l.token(.rbracket, 1),
        '(' => l.token(.lparen, 1),
        ')' => l.token(.rparen, 1),
        '{' => l.token(.lbrace, 1),
        '}' => l.token(.rbrace, 1),
        ';' => l.token(.semicolon, 1),
        '?' => l.token(.qmark, 1),
        ',' => l.token(.comma, 1),
        '`' => l.token(.comma, 1),
        '!' => if (l.ahead('=')) l.token(.not_equal, 2)
               else l.token(.bang, 1),
        '*' => if (l.ahead('=')) l.token(.star_equal, 2)
               else l.token(.star, 1),
        ':' => if (l.ahead(':')) l.token(.scope, 2)
               else l.token(.colon, 1),
        '=' => if (l.ahead('=')) l.token(.dequal, 2)
               else l.token(.equal, 1),
        '/' => if (l.ahead('=')) l.token(.slash_equal, 2)
               else l.token(.slash, 1),
        '.' => if (l.ahead('.')) l.token(.ddot, 2)
               else l.token(.dot, 1),
        '<' => switch (l.scan(1) orelse '\x00') {
            '<' => if (l.ahead_n('=', 2)) l.token(.lshift_equal, 3)
                   else l.token(.lshift, 2),
            '=' => l.token(.lt_equal, 2),
            else => l.token(.lt, 1),
        },
        '>' => switch (l.scan(1) orelse '\x00') {
            '>' => if (l.ahead_n('=', 2)) l.token(.rshift_equal, 3)
                   else l.token(.rshift, 2),
            '=' => l.token(.gt_equal, 2),
            else => l.token(.gt, 1),
        },
        '+' => switch (l.scan(1) orelse '\x00') {
            '+' => l.token(.inc, 2),
            '=' => l.token(.plus_equal, 2),
            else => l.token(.plus, 1),
        },
        '-' => switch (l.scan(1) orelse '\x00') {
            '-' => l.token(.dec, 2),
            '=' => l.token(.minus_equal, 2),
            else => l.token(.minus, 1),
        },
        '0' ... '9' => l.lexNumber(),
        else => unreachable,
    };
}

pub fn consume(l: *Lexer) void {
    l.cursor = l.cache.?.after(l.code);
    l.skipWhitespace();
}

const LexicalScan = struct {
    lexer: Lexer,
    scan: usize,

    fn init(l: Lexer, shift: usize) LexicalScan {
        return .{
            .lexer = l,
            .scan = shift,
        };
    }

    fn text(lc: LexicalScan) []const u8 {
        return lc.lexer.code.text[lc.lexer.cursor + lc.scan..];
    }

    fn shiftBy(lc: *LexicalScan, by: usize) void {
        lc.scan += by;
    }

    fn reset(lc: *LexicalScan) void {
        lc.scan = 0;
    }

    fn current(lc: LexicalScan) u8 {
        if (lc.text().len > 0) {
            return lc.text()[0];
        }
        return '\x00';
    }

    fn poke(lc: *LexicalScan, ahead_: usize) u8 {
        if (lc.scan + ahead_ < lc.text().len) {
            defer lc.scan += ahead_;
            return lc.text()[lc.scan + ahead_ - 1];
        }
        return '\x00';
    }

    fn parseInt(lc: *LexicalScan, base: Base) ?IntegerLit {
        var value: u64 = 0;
        var span: []const u8 = undefined;
        var bits: u8 = 32;

        switch (base) {
            .hex => {
                var shift: usize = 0;

                out: for (lc.text()) |ch| {
                    switch (ch) {
                        '0' ... '9' => {
                            value <<= 4;
                            value |= ch - '0';
                        },
                        'a' ... 'f' => {
                            value <<= 4;
                            value |= ch - 'a' + 10;
                        },
                        'A' ... 'F' => {
                            value <<= 4;
                            value |= ch - 'A' + 10;
                        },
                        else => if (shift == 0 or ch != '_') {
                            break :out;
                        },
                    }
                    shift += 1;
                }

                if (shift == 0) {
                    return null;
                }

                if (64 - @clz(value) > bits) {
                    bits = 64;
                }

                span = lc.text()[0..shift];
                lc.shiftBy(shift);
            },
            .binary => {
                var shift: usize = 0;

                for (lc.text()) |ch| {
                    switch (ch) {
                        '0', '1' => {
                            value <<= 1;
                            value |= ch - '0';
                        },
                        else => if (shift == 0 or ch != '_') {
                            break;
                        },
                    }
                    shift += 1;
                }

                if (shift == 0) {
                    return null;
                }

                if (64 - @clz(value) > bits) {
                    bits = 64;
                }

                span = lc.text()[0..shift];
                lc.shiftBy(shift);
            },
            .octal => {
                var shift: usize = 0;

                for (lc.text()) |ch| {
                    switch (ch) {
                        '0' ... '7' => {
                            value <<= 3;
                            value |= ch - '0';
                        },
                        else => if (shift == 0 or ch != '_') {
                            break;
                        },
                    }
                    shift += 1;
                }

                if (shift == 0) {
                    return null;
                }

                if (64 - @clz(value) > bits) {
                    bits = 64;
                }

                span = lc.text()[0..shift];
                lc.shiftBy(shift);
            },
            .decimal => {
                var shift: usize = 0;

                for (lc.text()) |ch| {
                    switch (ch) {
                        '0' ... '9' => {
                            value *= 10;
                            value += ch - '0';
                        },
                        else => if (shift == 0 or ch != '_') {
                            break;
                        },
                    }
                    shift += 1;
                }

                if (shift == 0) {
                    return null;
                }

                if (64 - @clz(value) > bits) {
                    bits = 64;
                }

                span = lc.text()[0..shift];
                lc.shiftBy(shift);
            },
        }

        return .{
            .base = base,
            .value = value,
            .span = span,
            .suffix = .{
                .bits = bits,
                .signed = false,
            },
        };
    }

    fn parseBinaryExponent(lc: *LexicalScan) ?Exponent {
        var shift: usize = 0;

        const signed = switch (lc.current()) {
            '+', '-' => out: {
                shift += 1;
                break :out lc.current() == '-';
            },
            else => false,
        };

        const before = shift;

        var value: u64 = 0;
        for (lc.text()[shift..]) |ch| {
            switch (ch) {
                '0', '1' => {
                    value <<= 1;
                    value |= ch - '0';
                },
                else => break,
            }
            shift += 1;
        }

        // The numeric part is non optional, if it fails to parse, anything
        // before is no longer consumed as the whole token fails to parse
        if (shift == before) {
            return null;
        }

        const span = lc.text()[0..shift];
        lc.shiftBy(shift);

        return .{
            .span = span,
            .value = value,
            .signed = signed,
        };
    }

    fn parseDecimalExponent(lc: *LexicalScan) ?Exponent {
        var shift: usize = 0;

        const signed = switch (lc.current()) {
            '+', '-' => out: {
                shift += 1;
                break :out lc.current() == '-';
            },
            else => false,
        };

        const before = shift;

        var value: u64 = 0;
        for (lc.text()[shift..]) |ch| {
            switch (ch) {
                '0' ... '9' => {
                    value *= 10;
                    value += ch - '0';
                },
                else => break,
            }
            shift += 1;
        }

        // The numeric part is non optional, if it fails to parse, anything
        // before is no longer consumed as the whole token fails to parse
        if (shift == before) {
            return null;
        }

        const span = lc.text()[0..shift];
        lc.shiftBy(shift);

        return .{
            .span = span,
            .value = value,
            .signed = signed,
        };
    }
};

fn lexNumber(l: Lexer) Token {
    var lc = LexicalScan.init(l, 0);

    var int: ?IntegerLit = null;
    if (lc.current() == '0') {
        int = switch (lc.poke(2)) {
            'x' => lc.parseInt(.hex),
            'o' => lc.parseInt(.octal),
            'b' => lc.parseInt(.binary),
            else => null,
        };

        if (int == null) {
            // Didn't successfully parse a valid integer.
            // Means it should just be a plain '0' - only interpret as
            // single digit zero value
            lc.reset();
            int = .{
                .span = lc.text()[0..1],
                .base = .decimal,
                .value = 0,
            };
            lc.shiftBy(1);
        } else {
            // extend the span to include the prefix
            int.?.span = l.code.text[l.cursor..][0..lc.scan];
        }
    } else {
        int = lc.parseInt(.decimal);
        if (int != null) {
            // extend the span to include the prefix
            int.?.span = l.code.text[l.cursor..][0..lc.scan];
        }
    }

    std.debug.assert(int != null);

    var fract: ?IntegerLit = null;
    if (lc.current() == '.') {
        lc.shiftBy(1);
        if (lc.parseInt(int.?.base)) |fr| {
            fract = fr;
        } else {
            lc.scan -= 1;
        }
    }

    var exponent: ?Exponent = null;
    switch (lc.current()) {
        'e', 'E' => {
            if (int.?.base == .decimal) {
                // Only valid for decimal integers
                lc.shiftBy(1);

                if (lc.parseDecimalExponent()) |exp| {
                    exponent = exp;
                } else {
                    lc.scan -= 1;
                }
            }
        },
        'p', 'P' => {
            if (int.?.base == .hex) {
                // Only valid for hexidecimal integers
                lc.shiftBy(1);
                if (lc.parseBinaryExponent()) |exp| {
                    exponent = exp;
                } else {
                    lc.scan -= 1;
                }
            }
        },
        else => {},
    }

    var integer_suffix: ?IntegerSuffix = null;
    if (fract == null) {
        // Only try parse integer suffix, if no fractional part
        switch (lc.current()) {
            's', 'u' => {
                const p = lc.current();

                lc.shiftBy(1);

                var bits: u8 = 32;

                if (lc.current() == '8') {
                    lc.shiftBy(1);
                    bits = 8;
                }

                if (bits != 8 and lc.text().len >= 2) {
                    const num = lc.text()[0..2];
                    if (std.mem.eql(u8, num, "16")) {
                        lc.shiftBy(2);
                        bits = 16;
                    } else if (std.mem.eql(u8, num, "32")) {
                        lc.shiftBy(2);
                        bits = 32;
                    } else if (std.mem.eql(u8, num, "64")) {
                        lc.shiftBy(2);
                        bits = 64;
                    }
                }

                integer_suffix = .{ 
                    .bits = bits,
                    .signed = p == 's',
                };
            },
            else => {},
        }
    }

    var floating_suffix: ?FloatingSuffix = null;
    if (integer_suffix == null and (int.?.base == .decimal or int.?.base == .hex)) {
        if (lc.current() == 'f') {
            if (lc.text().len >= 2) {
                const num = lc.text()[1..][0..2];

                var bits: ?u8 = null;
                if (std.mem.eql(u8, num, "32")) {
                    lc.shiftBy(3);
                    bits = 32;
                } else if (std.mem.eql(u8, num, "64")) {
                    lc.shiftBy(3);
                    bits = 64;
                }

                if (bits) |b| {
                    floating_suffix = .{
                        .bits = b,
                    };
                }
            }
        }
    }

    // At this point we have all the information needed to determine whether
    // the literal is a float or integer.

    const is_float = floating_suffix != null or fract != null;
    const is_integer = !is_float;

    if (is_integer) {
        var final_lit: IntegerLit = .{
            .span = l.code.text[l.cursor..][0..lc.scan],
            .base = int.?.base,
            .value = int.?.value,
            .suffix = int.?.suffix,
        };

        if (integer_suffix) |sfx| {
            final_lit.suffix = sfx;
        }

        if (exponent) |exp| {
            if (exp.signed) {
                log.err("TODO error, signed exponent in integer literal", .{});
            }
            final_lit.value = std.math.powi(u64, final_lit.value, exp.value) catch {
                log.err("TODO error over/under flow (cannot be represented error)", .{});
                unreachable;
            };
        }

        // TODO: check that the value is representable

        return .{
            .type = .integer_lit,
            .span = final_lit.span,
            .literal = .{ .integer = final_lit },
        };
    } 

    if (is_float) {
        var final_lit: FloatingLit = .{
            .span = l.code.text[l.cursor..][0..lc.scan],
            .integer = int.?,
            .fractional = fract,
            .exponent = exponent,
        };

        if (floating_suffix) |sfx| {
            final_lit.suffix = sfx;
        }

        // TODO: if hex float then it must have binary exponent
        // TODO: check that the value is representable

        return .{
            .type = .floating_lit,
            .span = final_lit.span,
            .literal = .{ .floating = final_lit },
        };
    }

    unreachable;
}

fn lex1(arena: *heap.ArenaAllocator, text: []const u8) Token {
    const code = Code.init(arena, text) catch unreachable;
    var lexer = Lexer.init(arena, code);
    return lexer.peek();
}

fn expectInt(arena: *heap.ArenaAllocator, text: []const u8, value: u64, base: Base, suffix: ?IntegerSuffix) !void {
    const tok = lex1(arena, text);

    try std.testing.expectEqual(.integer_lit, tok.type);
    try std.testing.expectEqual(.integer, std.meta.activeTag(tok.literal));
    if (tok.literal == .integer) {
        try std.testing.expectEqual(base, tok.literal.integer.base);
        try std.testing.expectEqual(value, tok.literal.integer.value);
        try std.testing.expectEqual(suffix orelse IntegerSuffix { .bits = 32, .signed = false }, tok.literal.integer.suffix);
        try std.testing.expectEqual(text, tok.literal.integer.span);
    }

    try std.testing.expectEqual(text, tok.span);
}

fn expectFloat(arena: *heap.ArenaAllocator, text: []const u8, value: u64, fract: ?u64, exp: ?i64, base: Base, suffix: ?FloatingSuffix) !void {
    const tok = lex1(arena, text);

    try std.testing.expectEqual(.floating_lit, tok.type);
    try std.testing.expectEqual(.floating, std.meta.activeTag(tok.literal));
    if (tok.literal == .floating) {
        try std.testing.expectEqual(base, tok.literal.floating.integer.base);
        try std.testing.expectEqual(value, tok.literal.floating.integer.value);
        if (fract) |fr| {
            try std.testing.expectEqual(fr, tok.literal.floating.fractional.?.value);
        } else {
            try std.testing.expectEqual(null, tok.literal.floating.fractional);
        }
        if (exp) |ex| {
            try std.testing.expectEqual(@abs(ex), tok.literal.floating.exponent.?.value);
            try std.testing.expectEqual(ex < 0, tok.literal.floating.exponent.?.signed);
        } else {
            try std.testing.expectEqual(null, tok.literal.floating.exponent);
        }
        try std.testing.expectEqual(suffix orelse FloatingSuffix { .bits = 32 }, tok.literal.floating.suffix);
        try std.testing.expectEqual(text, tok.literal.floating.span);
    }

    try std.testing.expectEqual(text, tok.span);
}

test "integer lexing" {
    const testing = std.testing;
    var arena = heap.ArenaAllocator.init(testing.allocator);

    try expectInt(&arena, "0", 0, .decimal, null);
    try expectInt(&arena, "20", 20, .decimal, null);
    try expectInt(&arena, "12u32", 12, .decimal, null);
    try expectInt(&arena, "0s32", 0, .decimal, .{ .bits = 32, .signed = true });
    try expectInt(&arena, "33s8", 33, .decimal, .{ .bits = 8, .signed = true });
    try expectInt(&arena, "0u64", 0, .decimal, .{ .bits = 64, .signed = false });
    try expectInt(&arena, "27u", 27, .decimal, .{ .bits = 32, .signed = false });
    try expectInt(&arena, "1s", 1, .decimal, .{ .bits = 32, .signed = true });
    try expectInt(&arena, "0x10", 0x10, .hex, null);
    try expectInt(&arena, "0x40_00s", 0x4000, .hex, .{ .bits = 32, .signed = true });
    try expectInt(&arena, "0b1011", 11, .binary, null);
    try expectInt(&arena, "0o120s8", 0o120, .octal, .{ .bits = 8, .signed = true });
    try expectInt(&arena, "10e2", try std.math.powi(u64, 10, 2), .decimal, null);
    try expectInt(&arena, "10e+10", try std.math.powi(u64, 10, 10), .decimal, null);
    try expectInt(&arena, "0xeFFFF_FFFFF", 0xEFFFFFFFFF, .hex, .{ .bits = 64, .signed = false });

    // Make sure lexer only parses complete valid tokens. For example in the case
    // below since no number follows the first '.', it is not a valid floating
    // literal, so overall only the first integer is lexed by the number parser.
    const tok = lex1(&arena, "10..10");
    try testing.expectEqualStrings("10", tok.span);
    try testing.expectEqual(.integer_lit, tok.type);
    try testing.expectEqual(.integer, std.meta.activeTag(tok.literal));
    try testing.expectEqual(10, tok.literal.integer.value);
    try testing.expectEqual(IntegerSuffix { .bits = 32, .signed = false }, tok.literal.integer.suffix);
}

test "floating point lexing" {
    const testing = std.testing;
    var arena = heap.ArenaAllocator.init(testing.allocator);

    try expectFloat(&arena, "0.0", 0, 0, null, .decimal, null);
    try expectFloat(&arena, "20f32", 20, null, null, .decimal, null);
    try expectFloat(&arena, "0.2e-3", 0, 2, -3, .decimal, null);
    try expectFloat(&arena, "0xaa.20p10f64", 0xaa, 0x20, 2, .hex, .{ .bits = 64 });
    try expectFloat(&arena, "34.0", 34, 0, null, .decimal, .{ .bits = 32 });
    try expectFloat(&arena, "34.3f64", 34, 3, null, .decimal, .{ .bits = 64 });

    const tok = lex1(&arena, "10.0->10");
    try testing.expectEqualStrings("10.0", tok.span);
    try testing.expectEqual(.floating_lit, tok.type);
    try testing.expectEqual(.floating, std.meta.activeTag(tok.literal));
    try testing.expectEqual(10, tok.literal.floating.integer.value);
    try testing.expectEqual(0, tok.literal.floating.fractional.?.value);
    try testing.expectEqual(FloatingSuffix { .bits = 32 }, tok.literal.floating.suffix);
}
