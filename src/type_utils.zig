const Lexer = @import("Lexer.zig");
const Token = Lexer.Token;
const TokenType = Lexer.TokenType;
const TypeRef = @import("type_ref.zig").TypeRef;

pub fn isBuiltinTokenType(tt: TokenType) bool {
    return switch (tt) {
        .s8,
        .u8,
        .s16,
        .u16,
        .s32,
        .u32,
        .s64,
        .u64,
        .f32,
        .f64,
        .str,
        .unit,
        .bool,
        .type,
        => true,
        else => false,
    };
}

pub fn toBuiltinType(tt: TokenType) TypeRef {
    switch (tt) {
        inline else => |t| {
            if (@hasField(TypeRef, @tagName(t))) {
                return @field(TypeRef, @tagName(t));
            }
            unreachable;
        },
    }
}
