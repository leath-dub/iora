pub const GeneralCategory = enum {
    Lu,
    Ll,
    Lt,
    Lm,
    Lo,
    Mn,
    Mc,
    Me,
    Nd,
    Nl,
    No,
    Pc,
    Pd,
    Ps,
    Pe,
    Pi,
    Pf,
    Po,
    Sm,
    Sc,
    Sk,
    So,
    Zs,
    Zl,
    Zp,
    Cc,
    Cf,
    Cs,
    Co,
    Cn,
};

pub const CodepointGroup = struct {
    start: u32,
    end: u32,
    category: GeneralCategory,

    pub fn lessThan(_: void, left: CodepointGroup, right: CodepointGroup) bool {
        return left.end < right.end;
    }
};
