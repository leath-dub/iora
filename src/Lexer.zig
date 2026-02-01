const std = @import("std");
const heap = std.heap;
const log = std.log;
const unicode = std.unicode;
const math = std.math;

const Code = @import("Code.zig");
const GeneralContext = @import("GeneralContext.zig");
const unicode_utils = @import("unicode.zig");

pub const TokenType = enum {
    eof,
    synthesized,
    illegal,
    invalid,
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
    int_lit,
    float_lit,
    ident,
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

const Keyword = en: {
    const field_names = std.meta.fieldNames(TokenType);

    var index: usize = 0;
    var fields: [field_names.len]std.builtin.Type.EnumField = undefined;

    for (std.meta.fieldNames(TokenType)) |tt| {
        if (std.mem.startsWith(u8, tt, "kw_")) {
            fields[index] = .{ .name = tt[3..], .value = index };
            index += 1;
        }
    }

    break :en @Type(.{ .@"enum" = .{
        .tag_type = @typeInfo(TokenType).@"enum".tag_type,
        .decls = &.{},
        .fields = fields[0..index],
        .is_exhaustive = true,
    } });
};

pub const Base = enum {
    decimal,
    binary,
    octal,
    hex,
};

pub const IntLit = struct {
    base: Base,
    value: u64 = 0,
    suffix: ?Code.Offset = null,
};

pub const FloatLit = struct {
    const LexFlag = enum {
        int,
        fract,
    };
    int: u64 = 0,
    fract: u64 = 0,
    exp: union(enum) {
        signed: u64,
        unsigned: u64,
    } = .{ .unsigned = 1 },
    // Needed for parse time to disambiguite foo.0 from being token sequence
    // <ident> <dot> <int lit> or <ident> <float lit>
    lex_flags: std.enums.EnumSet(LexFlag) = .{},
};

pub const Lit = union(enum) {
    int: IntLit,
    float: FloatLit,
};

pub const Token = struct {
    type: TokenType,
    span: []const u8,
    lit: ?Lit = null,

    pub fn offset(t: Token, code: Code) usize {
        return @as(usize, @as(usize, @intFromPtr(t.span.ptr) - @intFromPtr(code.text.ptr)));
    }
    pub fn after(t: Token, code: Code) usize {
        return t.offset(code) + t.span.len;
    }
};

ctx: *GeneralContext,
code: Code,
arena: *heap.ArenaAllocator,
cache: ?Token = null,
cursor: usize = 0,

const Lexer = @This();

pub fn init(ctx: *GeneralContext, arena: *heap.ArenaAllocator, code: Code) Lexer {
    var l: Lexer = .{
        .ctx = ctx,
        .code = code,
        .arena = arena,
    };
    l.skipWhitespace();
    return l;
}

pub fn skipWhitespace(l: *Lexer) void {
    var in_comment = false;

    while (l.cursor < l.code.text.len) : (l.cursor += 1) {
        const c = l.current();
        if (in_comment) {
            if (c != '\n') {
                continue;
            }
            in_comment = false;
        }
        switch (c) {
            '\n', '\t', '\r', ' ' => {},
            '/' => if (l.ahead('/')) {
                in_comment = true;
            },
            else => break,
        }
    }
}

fn token(l: Lexer, ty: TokenType, len: usize) Token {
    return .{
        .type = ty,
        .span = l.code.text[l.cursor..][0..len],
    };
}

fn tokenLit(l: Lexer, ty: TokenType, len: usize, lit: Lit) Token {
    var tok = l.token(ty, len);
    tok.lit = lit;
    return tok;
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

fn illegalToken(l: *Lexer) Token {
    const illegal: u8 = l.current();
    if (0x21 <= illegal and illegal <= 0x7e) {
        // It is in the printable range of ascii characters, raise error formatting
        // it as a {c}
        l.code.raise(l.ctx.error_out, l.cursor, "illegal character '{c}'", .{illegal}) catch unreachable;
    } else {
        // Print as hex otherwise
        l.code.raise(l.ctx.error_out, l.cursor, "illegal character 0x{x}", .{illegal}) catch unreachable;
    }
    return l.token(.illegal, 1);
}

const Runes = struct {
    text: []const u8,
    consumed: usize = 0,

    pub fn next(it: *Runes) !?u32 {
        if (it.text.len == 0) {
            return null;
        }
        const rune_len = try unicode.utf8ByteSequenceLength(it.text[0]);
        const rune = try unicode.utf8Decode(it.text[0..rune_len]);
        it.consumed += rune_len;
        it.text = it.text[rune_len..];
        return rune;
    }
};

pub fn peek(l: *Lexer) Token {
    if (l.cache) |tok| {
        if (tok.offset(l.code) == l.cursor) {
            return tok;
        }
    }
    if (l.cursor >= l.code.text.len) {
        return l.token(.eof, 0);
    }

    l.cache = switch (l.current()) {
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
        '!' => if (l.ahead('=')) l.token(.not_equal, 2) else l.token(.bang, 1),
        '*' => if (l.ahead('=')) l.token(.star_equal, 2) else l.token(.star, 1),
        ':' => if (l.ahead(':')) l.token(.scope, 2) else l.token(.colon, 1),
        '=' => if (l.ahead('=')) l.token(.dequal, 2) else l.token(.equal, 1),
        '/' => if (l.ahead('=')) l.token(.slash_equal, 2) else l.token(.slash, 1),
        '|' => if (l.ahead('=')) l.token(.pipe_equal, 2) else l.token(.pipe, 1),
        '.' => l.lexFloat()
            catch if (l.ahead('.')) l.token(.ddot, 2) else l.token(.dot, 1),
        '<' => switch (l.scan(1) orelse '\x00') {
            '<' => if (l.ahead_n('=', 2)) l.token(.lshift_equal, 3) else l.token(.lshift, 2),
            '=' => l.token(.lt_equal, 2),
            else => l.token(.lt, 1),
        },
        '>' => switch (l.scan(1) orelse '\x00') {
            '>' => if (l.ahead_n('=', 2)) l.token(.rshift_equal, 3) else l.token(.rshift, 2),
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
        '0'...'9' => l.lexFloat()
            catch l.lexInt()
            catch l.token(.invalid, 1),
        else => out: {
            const ident = l.lexIdent();
            const tt_opt = if (std.meta.stringToEnum(Keyword, ident.span)) |kw| switch (kw) {
                inline else => |tag| @field(TokenType, "kw_" ++ @tagName(tag)),
            } else null;
            break :out if (tt_opt) |tt|
                l.token(tt, ident.span.len)
            else
                ident;
        },
    };

    return l.cache.?;
}

pub fn consume(l: *Lexer) void {
    l.cursor = l.cache.?.after(l.code);
    l.skipWhitespace();
}

const LexError = error{LexFailed};

const Digits = struct {
    len: usize,
    value: u64,
    overflow: bool,

    pub fn zero() Digits {
        return .{ .len = 1, .value = 0, .overflow = false };
    }

    fn incorporateDecimalDigit(d: *Digits, digit: u8) bool {
        overflow: switch (digit) {
            '0'...'9' => {
                const mul = math.mul(u64, d.value, 10) catch break :overflow;
                const sum = math.add(u64, mul, digit - '0') catch break :overflow;
                d.len += 1;
                d.value = sum;
                return true;
            },
            '_' => {
                d.len += 1;
                return true;
            },
            else => return false,
        }
        d.len += 1;
        d.overflow = true;
        return true;
    }

    fn incorporateHexDigit(d: *Digits, digit: u8) bool {
        const result, const overflow = @shlWithOverflow(d.value, 4);
        if (digit == '_') {
            d.len += 1;
            return true;
        }
        if (switch (digit) {
            '0'...'9' => digit - '0',
            'a'...'f' => 10 + digit - 'a',
            'A'...'F' => 10 + digit - 'A',
            else => null,
        }) |digit_value| {
            d.len += 1;
            if (overflow == 1) {
                d.overflow = true;
            }
            if (!d.overflow) {
                d.value = result | digit_value;
            }
            return true;
        }
        return false;
    }

    fn incorporateBinaryDigit(d: *Digits, digit: u8) bool {
        const result, const overflow = @shlWithOverflow(d.value, 1);
        if (digit == '_') {
            d.len += 1;
            return true;
        }
        if (switch (digit) {
            '0', '1' => digit - '0',
            else => null,
        }) |digit_value| {
            d.len += 1;
            if (overflow == 1) {
                d.overflow = true;
            }
            if (!d.overflow) {
                d.value = result | digit_value;
            }
            return true;
        }
        return false;
    }

    fn incorporateOctalDigit(d: *Digits, digit: u8) bool {
        const result, const overflow = @shlWithOverflow(d.value, 3);
        if (digit == '_') {
            d.len += 1;
            return true;
        }
        if (switch (digit) {
            '0'...'7' => digit - '0',
            else => null,
        }) |digit_value| {
            d.len += 1;
            if (overflow == 1) {
                d.overflow = true;
            }
            if (!d.overflow) {
                d.value = result | digit_value;
            }
            return true;
        }
        return false;
    }
};

fn lexHexFloat(l: *Lexer) LexError!Token {
    var offset: usize = 2; // start at offset 2 to trim '0x'
    var float_lit = FloatLit{};

    std.debug.assert(l.ahead_n('0', 0));
    std.debug.assert(l.ahead_n('x', 1));

    // Lex the mantissa

    var int_digits: ?Digits = null;
    var fract_digits: ?Digits = null;

    {
        var digits = std.mem.zeroes(Digits);
        if (digits.incorporateHexDigit(l.scan(offset) orelse '\x00')) {
            int_digits = try l.lexDigits(.hex, offset);
            offset += int_digits.?.len;
            float_lit.int = int_digits.?.value;
        }
    }

    if (l.ahead_n('.', offset)) {
        offset += 1;
        fract_digits = l.lexDigits(.hex, offset) catch out: {
            // Failed to lex digits. This is only acceptible if we did specify
            // the integer part. Otherwise we would be allowing `0x.` as a valid
            // floating point literal for 0, which is just odd :).
            if (int_digits != null) {
                break :out null;
            }
            return error.LexFailed;
        };
        if (fract_digits) |fd| {
            offset += fd.len;
            float_lit.fract += fd.value;
        }
    }

    if (int_digits != null) {
        float_lit.lex_flags.insert(.int);
    }

    if (fract_digits != null) {
        float_lit.lex_flags.insert(.fract);
    }

    // Lex the exponent
    if (!l.ahead_n('p', offset)) {
        return error.LexFailed;
    }
    offset += 1;

    var signed = false;

    if (l.ahead_n('-', offset)) {
        offset += 1;
        signed = true;
    } else if (l.ahead_n('+', offset)) {
        offset += 1;
    }

    const exp = try l.lexDigits(.decimal, offset);
    offset += exp.len;

    if (signed) {
        float_lit.exp = .{ .signed = exp.value };
    } else {
        float_lit.exp = .{ .unsigned = exp.value };
    }

    return l.tokenLit(.float_lit, offset, .{ .float = float_lit });
}

fn lexDecimalFloat(l: *Lexer) LexError!Token {
    var offset: usize = 0;
    var float_lit = FloatLit{};

    var int_digits: ?Digits = null;
    var fract_digits: ?Digits = null;

    // Lex the mantissa
    {
        var digits = std.mem.zeroes(Digits);
        if (digits.incorporateDecimalDigit(l.scan(offset) orelse '\x00')) {
            int_digits = try l.lexDigits(.decimal, offset);
            offset += int_digits.?.len;
            float_lit.int = int_digits.?.value;
        }
    }

    const decimal_point = l.ahead_n('.', offset);
    if (decimal_point) {
        offset += 1;
        fract_digits = l.lexDigits(.decimal, offset) catch out: {
            // Only acceptible to have no digits after '.' in case the
            // integer part was specified - this is so we cant allow just '.' to
            // lex as a floating point literal.
            if (int_digits != null) {
                break :out null;
            }
            return error.LexFailed;
        };
        if (fract_digits) |fd| {
            offset += fd.len;
            float_lit.fract += fd.value;
        }
    }

    if (int_digits != null) {
        float_lit.lex_flags.insert(.int);
    }

    if (fract_digits != null) {
        float_lit.lex_flags.insert(.fract);
    }

    // Lex the exponent
    if (l.ahead_n('e', offset)) {
        offset += 1;
        var signed = false;

        if (l.ahead_n('-', offset)) {
            offset += 1;
            signed = true;
        } else if (l.ahead_n('+', offset)) {
            offset += 1;
        }

        const exp = try l.lexDigits(.decimal, offset);
        offset += exp.len;

        if (signed) {
            float_lit.exp = .{ .signed = exp.value };
        } else {
            float_lit.exp = .{ .unsigned = exp.value };
        }
    } else {
        // We only allow no exponent if '.' was specified. This is to
        // disambiguite between integer literal and float literal.
        if (!decimal_point) {
            return error.LexFailed;
        }
    }

    return l.tokenLit(.float_lit, offset, .{ .float = float_lit });
}

fn lexFloat(l: *Lexer) LexError!Token {
    if (l.current() == '0') {
        const next = l.scan(1) orelse '\x00';
        if (next == 'x') {
            return l.lexHexFloat();
        }
    }
    return l.lexDecimalFloat();
}

fn lexDigits(l: *Lexer, comptime base: Base, start: usize) LexError!Digits {
    const handleDigit = comptime switch (base) {
        .decimal => Digits.incorporateDecimalDigit,
        .binary => Digits.incorporateBinaryDigit,
        .octal => Digits.incorporateOctalDigit,
        .hex => Digits.incorporateHexDigit,
    };

    var digits = Digits{ .len = 0, .value = 0, .overflow = false };
    var next = l.scan(start + digits.len) orelse '\x00';
    while (handleDigit(&digits, next)) {
        next = l.scan(start + digits.len) orelse '\x00';
    }

    if (digits.len == 0) {
        return error.LexFailed;
    }

    if (digits.overflow) {
        const offset = l.cursor + start;
        l.code.raise(l.ctx.error_out, offset, "number '{s}' is too large to be stored as u64", .{ l.code.text[offset..][0..digits.len] }) catch unreachable;
    }

    return digits;
}

fn lexInt(l: *Lexer) LexError!Token {
    var offset: usize = 0;
    var int_lit = IntLit{
        .base = .decimal,
    };

    const zero_start = l.current() == '0';

    if (zero_start) {
        const next = l.scan(1) orelse '\x00';
        int_lit.base = switch (next) {
            'b' => .binary,
            'o' => .octal,
            'x' => .hex,
            else => .decimal,
        };

        if (int_lit.base != .decimal) {
            offset += 2;
        } else {
            var digits = std.mem.zeroes(Digits);
            if (digits.incorporateOctalDigit(l.scan(2) orelse '\x00')) {
                int_lit.base = .octal;
                offset += 1;
            }
        }
    }

    var digits = Digits.zero();
    if (!zero_start or int_lit.base != .decimal) {
        digits = switch (int_lit.base) {
            inline else => |base| try l.lexDigits(base, offset),
        };
    }
    offset += digits.len;

    int_lit.suffix = switch (l.scan(offset) orelse '\x00') {
        'u', 's' => off: {
            defer offset += 1;
            break :off l.cursor + offset;
        },
        else => null,
    };

    int_lit.value = digits.value;
    return l.tokenLit(.int_lit, offset, .{ .int = int_lit });
}

fn lexIdent(l: *Lexer) Token {
    var runes = Runes{ .text = l.code.text[l.cursor..] };
    const first = runes.next() catch null orelse return l.illegalToken();

    if (!unicode_utils.isIdentStart(first)) {
        return l.illegalToken();
    }

    while (runes.next() catch null) |rune| {
        if (!unicode_utils.isIdentContinue(rune)) {
            break;
        }
    }

    var tok = Token{ .type = .ident, .span = l.code.text[l.cursor..][0..runes.consumed - 1] };
    // Allow trailing ' in identifier
    const after = tok.after(l.code);
    if (after < l.code.text.len and l.code.text[after] == '\'') {
        tok.span.len += 1;
    }

    return tok;
}

fn float(int: u64, fract: u64, exp: i64, comptime lex_flags: anytype) FloatLit {
    var float_lit = FloatLit{ .int = int, .fract = fract, .lex_flags = .initMany(&lex_flags) };
    if (exp < 0) {
        float_lit.exp = .{ .signed = @abs(exp) };
    } else {
        float_lit.exp = .{ .unsigned = @intCast(exp) };
    }
    return float_lit;
}

const LexerTest = struct {
    tc: GeneralContext.Testing,
    gc: GeneralContext,
    syntax: heap.ArenaAllocator,

    pub fn setUp(t: *LexerTest) void {
        t.tc = GeneralContext.Testing.init();
        t.gc = t.tc.general();
        t.syntax = t.gc.createLifetime();
    }

    pub fn tearDown(t: *LexerTest) void {
        t.syntax.deinit();
        t.tc.deinit();
    }

    pub fn expectIntLit(t: *LexerTest, text: []const u8, value: u64, suffix: ?u8) !void {
        var lexer = Lexer.init(
            &t.gc,
            &t.syntax,
            try .init(&t.syntax, "<test input>", text));
        const tok = lexer.peek();
        try std.testing.expectEqual(.int_lit, tok.type);
        try std.testing.expect(tok.lit != null);
        try std.testing.expectEqual(.int, @as(std.meta.Tag(Lit), tok.lit.?));
        var suffix_got: ?u8 = null;
        if (tok.lit.?.int.suffix) |off| {
            suffix_got = text[off];
        }
        try std.testing.expectEqual(suffix, suffix_got);
        try std.testing.expectEqual(value, tok.lit.?.int.value);
    }

    pub fn expectFloatLit(t: *LexerTest, text: []const u8, float_lit: FloatLit) !void {
        var lexer = Lexer.init(
            &t.gc,
            &t.syntax,
            try .init(&t.syntax, "<test input>", text));
        const tok = lexer.peek();
        try std.testing.expectEqual(.float_lit, tok.type);
        try std.testing.expect(tok.lit != null);
        try std.testing.expectEqual(.float, @as(std.meta.Tag(Lit), tok.lit.?));
        try std.testing.expectEqual(float_lit, tok.lit.?.float);
    }

    pub fn expectTokenTypes(t: *LexerTest, text: []const u8, types: []const TokenType) !void {
        var lexer = Lexer.init(
            &t.gc,
            &t.syntax,
            try .init(&t.syntax, "<test input>", text));
        for (types) |ty| {
            try std.testing.expectEqual(ty, lexer.peek().type);
            lexer.consume();
        }
    }
};

test "integer lexing" {
    var t: LexerTest = undefined;
    t.setUp();
    defer t.tearDown();

    // Data taken from: https://go.dev/ref/spec
    try t.expectIntLit("42", 42, null);
    try t.expectIntLit("4_2", 42, null);
    try t.expectIntLit("0600", 0o600, null);
    try t.expectIntLit("0_600", 0o600, null);
    try t.expectIntLit("0o600", 0o600, null);
    try t.expectIntLit("0xBadFace", 0xbadface, null);
    try t.expectIntLit("0xBad_Face", 0xbadface, null);
    try t.expectIntLit("0x_67_7a_2f_cc_40_c6", 0x677a2fcc40c6, null);
    // try t.expectIntLit("170141183460469231731687303715884105727", 170141183460469231731687303715884105727, null);
    // try t.expectIntLit("170141183_460469_231731_687303_715884_105727", 170141183460469231731687303715884105727, null);

}

test "floating point lexing" {
    var t: LexerTest = undefined;
    t.setUp();
    defer t.tearDown();

    // Data taken from: https://go.dev/ref/spec
    try t.expectFloatLit("0.", float(0, 0, 1, .{ .int }));
    try t.expectFloatLit("072.40", float(72, 40, 1, .{ .int, .fract }));
    try t.expectFloatLit("2.71828", float(2, 71828, 1, .{ .int, .fract }));
    try t.expectFloatLit("1.e+0", float(1, 0, 0, .{ .int }));
    try t.expectFloatLit("6.67428e-11", float(6, 67428, -11, .{ .int, .fract }));
    try t.expectFloatLit("1e6", float(1, 0, 6, .{ .int }));
    try t.expectFloatLit(".25", float(0, 25, 1, .{ .fract }));
    try t.expectFloatLit(".12345e+5", float(0, 12345, 5, .{ .fract }));
    try t.expectFloatLit("1_5.", float(15, 0, 1, .{ .int }));
    try t.expectFloatLit("0.15e+0_2", float(0, 15, 2, .{ .int, .fract }));
    try t.expectFloatLit("0x1p-2", float(1, 0, -2, .{ .int }));
    try t.expectFloatLit("0x2.p10", float(2, 0, 10, .{ .int }));
    try t.expectFloatLit("0x1.Fp+0", float(1, 0xF, 0, .{ .int, .fract }));
    try t.expectFloatLit("0x.8p-1", float(0, 8, -1, .{ .fract }));
    try t.expectFloatLit("0x_1FFFp-16", float(0x1FFF, 0, -16, .{ .int }));

    // Negative test cases
    try t.expectTokenTypes("0x15e-2", &.{ .int_lit, .minus, .int_lit });
    try t.expectTokenTypes("0x.p1", &.{ .invalid, .ident, .dot, .ident, .int_lit });
    try t.expectTokenTypes("0X.0", &.{ .int_lit, .ident, .float_lit });
    try t.expectTokenTypes("0x1.5e-2", &.{ .int_lit, .float_lit }); // 0x1, .5e-2
}
