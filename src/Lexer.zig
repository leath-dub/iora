const std = @import("std");
const heap = std.heap;
const log = std.log;
const unicode = std.unicode;

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

pub const Lit = union(enum) {
    int: IntLit,
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
        // TODO: add attempt to parse float here
        '.' => if (l.ahead('.')) l.token(.ddot, 2) else l.token(.dot, 1),
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
        // TODO: order of lexing
        //
        // * try lex float
        // * try lex int
        '0'...'9' => l.lexInt() catch l.token(.invalid, 1),
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
    pub fn zero() Digits {
        return .{ .len = 1, .value = 0 };
    }
};

fn handleDecimalDigit(value: u64, digit: u8) ?u64 {
    return switch (digit) {
        '0'...'9' => (value * 10) + (digit - '0'),
        '_' => value,
        else => null,
    };
}

fn handleBinaryDigit(value: u64, digit: u8) ?u64 {
    return switch (digit) {
        '0', '1' => (value << 1) | (digit - '0'),
        '_' => value,
        else => null,
    };
}

fn handleOctalDigit(value: u64, digit: u8) ?u64 {
    return switch (digit) {
        '0'...'7' => (value << 3) | (digit - '0'),
        '_' => value,
        else => null,
    };
}

fn handleHexDigit(value: u64, digit: u8) ?u64 {
    return switch (digit) {
        '0'...'9' => (value << 4) | (digit - '0'),
        'a'...'f' => (value << 4) | (10 + digit - 'a'),
        'A'...'F' => (value << 4) | (10 + digit - 'A'),
        '_' => value,
        else => null,
    };
}

fn lexDigits(l: Lexer, comptime base: Base, start: usize) LexError!Digits {
    var value: u64 = 0;
    var offset = start;

    var next = l.scan(offset) orelse '\x00';
    while ((comptime switch (base) {
        .decimal => handleDecimalDigit,
        .binary => handleBinaryDigit,
        .octal => handleOctalDigit,
        .hex => handleHexDigit,
    })(value, next)) |v| {
        value = v;
        offset += 1;
        next = l.scan(offset) orelse '\x00';
    }

    if (offset == start) {
        return error.LexFailed;
    }

    return .{
        .len = offset - start,
        .value = value,
    };
}

fn lexInt(l: Lexer) LexError!Token {
    var offset: usize = 0;
    var int_lit = IntLit{
        .base = .decimal,
    };

    var is_zero = false;

    if (l.current() == '0') {
        const next = l.scan(1) orelse '\x00';
        int_lit.base = switch (next) {
            'b' => .binary,
            'o' => .octal,
            'x' => .hex,
            else => .decimal,
        };
        is_zero = int_lit.base == .decimal;
        if (!is_zero) {
            offset += 2;
        }
    }

    var digits = Digits.zero();
    if (!is_zero) {
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

    var tok = Token{ .type = .identifier, .span = l.code.text[l.cursor..][0..runes.consumed - 1] };
    // Allow trailing ' in identifier
    const after = tok.after(l.code);
    if (after < l.code.text.len and l.code.text[after] == '\'') {
        tok.span.len += 1;
    }

    return tok;
}
