const unicode = @import("unicode_types");
const unicode_data = @import("unicode_data");

pub fn lookupGeneralCategory(code_point: u32) ?unicode.GeneralCategory {
    var l: usize = 0;
    var h: usize = unicode_data.data.len;
    while (l <= h) {
        const m = l + (h - l) / 2;
        if (code_point < unicode_data.data[m].start) {
            h = m - 1;
        } else if (code_point > unicode_data.data[m].end) {
            l = m + 1;
        } else {
            return unicode_data.data[m].category;
        }
    }
    return null;
}

pub fn isIdentStart(ch: u32) bool {
    const gc = lookupGeneralCategory(ch) orelse return false;
    return switch (gc) {
        .Lu, .Ll, .Lt, .Lm, .Lo, .Nl => true,
        else => ch == '_',
    };
}

pub fn isIdentContinue(ch: u32) bool {
    const gc = lookupGeneralCategory(ch) orelse return false;
    return switch (gc) {
        .Lu, .Ll, .Lt, .Lm, .Lo, .Nl, .Mn, .Mc, .Nd, .Pc => true,
        else => ch == '_',
    };
}
