use self::Token::*;

#[derive(Debug)]
pub enum Token {
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    Identifier(String),
    True,
    False,
    Plus,
    Minus,
    Asterisk,
    ForwardSlash,
    Caret,
    Percent,
    Equals,
    DoubleEquals,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LogicalNot,
    LogicalAnd,
    LogicalOr,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    LeftMapLiteral,
    RightMapLiteral,
    LeftSetLiteral,
    RightSetLiteral,
    Period,
    Comma,
    Colon,
    Arrow,
    Pipe,
    Scope,
    Type,
    Let,
    Const,
    Def,
    Fun,
    If,
    Else,
    While,
    Do,
    For,
    ForEach,
    In,
    Match,
    When,
    Return,
    Break,
    Continue,
    UnitType,
    BoolType,
    StringType,
    ByteType,
    IntType,
    FloatType,
    DoubleType,
    VecType,
    SetType,
    MapType,
    Package,
    Import,
    As,
    Implement,
    Extends,
    Trait,
    Sig,
    Static,
    Mut,
    Builtin,
}

impl ToString for Token {
    fn to_string(&self) -> String {
        match self {
            IntLiteral(num) => num.to_string(),
            FloatLiteral(num) => num.to_string(),
            StringLiteral(string) => string.clone(),
            Identifier(string) => string.clone(),
            True => String::from("true"),
            False => String::from("false"),
            Plus => String::from("+"),
            Minus => String::from("-"),
            Asterisk => String::from("*"),
            ForwardSlash => String::from("/"),
            Caret => String::from("^"),
            Percent => String::from("%"),
            Equals => String::from("="),
            DoubleEquals => String::from("=="),
            NotEqual => String::from("!="),
            LessThan => String::from("<"),
            LessThanOrEqual => String::from("<="),
            GreaterThan => String::from(">"),
            GreaterThanOrEqual => String::from(">="),
            LogicalNot => String::from("!"),
            LogicalAnd => String::from("&&"),
            LogicalOr => String::from("||"),
            LeftParen => String::from("("),
            RightParen => String::from(")"),
            LeftBrace => String::from("{"),
            RightBrace => String::from("}"),
            LeftBracket => String::from("["),
            RightBracket => String::from("]"),
            LeftMapLiteral => String::from("[|"),
            RightMapLiteral => String::from("|]"),
            LeftSetLiteral => String::from("{|"),
            RightSetLiteral => String::from("|}"),
            Period => String::from("."),
            Comma => String::from(","),
            Colon => String::from(":"),
            Arrow => String::from("->"),
            Pipe => String::from("|"),
            Scope => String::from("::"),
            Type => String::from("type"),
            Let => String::from("let"),
            Const => String::from("const"),
            Def => String::from("def"),
            Fun => String::from("fun"),
            If => String::from("if"),
            Else => String::from("else"),
            While => String::from("while"),
            Do => String::from("do"),
            For => String::from("for"),
            ForEach => String::from("forEach"),
            In => String::from("in"),
            Match => String::from("match"),
            When => String::from("when"),
            Return => String::from("return"),
            Break => String::from("break"),
            Continue => String::from("continue"),
            UnitType => String::from("unit"),
            BoolType => String::from("bool"),
            StringType => String::from("string"),
            ByteType => String::from("byte"),
            IntType => String::from("int"),
            FloatType => String::from("float"),
            DoubleType => String::from("double"),
            VecType => String::from("vec"),
            SetType => String::from("set"),
            MapType => String::from("map"),
            Package => String::from("package"),
            Import => String::from("import"),
            As => String::from("as"),
            Implement => String::from("impl"),
            Extends => String::from("extends"),
            Trait => String::from("trait"),
            Sig => String::from("sig"),
            Static => String::from("static"),
            Mut => String::from("mut"),
            Builtin => String::from("__builtin"),
        }
    }
}
