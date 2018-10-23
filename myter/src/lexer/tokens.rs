use self::Token::*;
use common::span::Span;

#[derive(Clone, Debug)]
pub enum Token {
    IntLiteral(i64, Span),
    FloatLiteral(f64, Span),
    StringLiteral(String, Span),
    Identifier(String, Span),
    True(Span),
    False(Span),
    Plus(Span),
    Minus(Span),
    Asterisk(Span),
    ForwardSlash(Span),
    Caret(Span),
    Percent(Span),
    Equals(Span),
    DoubleEquals(Span),
    NotEqual(Span),
    LessThan(Span),
    LessThanOrEqual(Span),
    GreaterThan(Span),
    GreaterThanOrEqual(Span),
    LogicalNot(Span),
    LogicalAnd(Span),
    LogicalOr(Span),
    LeftParen(Span),
    RightParen(Span),
    LeftBrace(Span),
    RightBrace(Span),
    LeftBracket(Span),
    RightBracket(Span),
    LeftMapLiteral(Span),
    RightMapLiteral(Span),
    LeftSetLiteral(Span),
    RightSetLiteral(Span),
    Period(Span),
    Comma(Span),
    Colon(Span),
    Arrow(Span),
    Pipe(Span),
    Scope(Span),
    Type(Span),
    Let(Span),
    Const(Span),
    Def(Span),
    Fun(Span),
    If(Span),
    Else(Span),
    While(Span),
    Do(Span),
    For(Span),
    ForEach(Span),
    In(Span),
    Match(Span),
    When(Span),
    Return(Span),
    Break(Span),
    Continue(Span),
    UnitType(Span),
    BoolType(Span),
    StringType(Span),
    ByteType(Span),
    IntType(Span),
    FloatType(Span),
    DoubleType(Span),
    VecType(Span),
    SetType(Span),
    MapType(Span),
    Package(Span),
    Import(Span),
    As(Span),
    Implement(Span),
    Extends(Span),
    Trait(Span),
    Sig(Span),
    Static(Span),
    Mut(Span),
    Builtin(Span),
}

impl Token {
    pub fn span(&self) -> &Span {
        return match self {
            IntLiteral(_, span) => span,
            FloatLiteral(_, span) => span,
            StringLiteral(_, span) => span,
            Identifier(_, span) => span,
            True(span) => span,
            False(span) => span,
            Plus(span) => span,
            Minus(span) => span,
            Asterisk(span) => span,
            ForwardSlash(span) => span,
            Caret(span) => span,
            Percent(span) => span,
            Equals(span) => span,
            DoubleEquals(span) => span,
            NotEqual(span) => span,
            LessThan(span) => span,
            LessThanOrEqual(span) => span,
            GreaterThan(span) => span,
            GreaterThanOrEqual(span) => span,
            LogicalNot(span) => span,
            LogicalAnd(span) => span,
            LogicalOr(span) => span,
            LeftParen(span) => span,
            RightParen(span) => span,
            LeftBrace(span) => span,
            RightBrace(span) => span,
            LeftBracket(span) => span,
            RightBracket(span) => span,
            LeftMapLiteral(span) => span,
            RightMapLiteral(span) => span,
            LeftSetLiteral(span) => span,
            RightSetLiteral(span) => span,
            Period(span) => span,
            Comma(span) => span,
            Colon(span) => span,
            Arrow(span) => span,
            Pipe(span) => span,
            Scope(span) => span,
            Type(span) => span,
            Let(span) => span,
            Const(span) => span,
            Def(span) => span,
            Fun(span) => span,
            If(span) => span,
            Else(span) => span,
            While(span) => span,
            Do(span) => span,
            For(span) => span,
            ForEach(span) => span,
            In(span) => span,
            Match(span) => span,
            When(span) => span,
            Return(span) => span,
            Break(span) => span,
            Continue(span) => span,
            UnitType(span) => span,
            BoolType(span) => span,
            StringType(span) => span,
            ByteType(span) => span,
            IntType(span) => span,
            FloatType(span) => span,
            DoubleType(span) => span,
            VecType(span) => span,
            SetType(span) => span,
            MapType(span) => span,
            Package(span) => span,
            Import(span) => span,
            As(span) => span,
            Implement(span) => span,
            Extends(span) => span,
            Trait(span) => span,
            Sig(span) => span,
            Static(span) => span,
            Mut(span) => span,
            Builtin(span) => span,
        };
    }

    pub fn type_to_string(&self) -> String {
        match self {
            IntLiteral(..) => "int literal".to_string(),
            FloatLiteral(..) => "float literal".to_string(),
            StringLiteral(..) => "string literal".to_string(),
            Identifier(..) => "identifier".to_string(),
            _ => self.to_string(),
        }
    }
}

impl ToString for Token {
    fn to_string(&self) -> String {
        match self {
            IntLiteral(num, _) => num.to_string(),
            FloatLiteral(num, _) => num.to_string(),
            StringLiteral(string, _) => string.clone(),
            Identifier(string, _) => string.clone(),
            True(_) => String::from("true"),
            False(_) => String::from("false"),
            Plus(_) => String::from("+"),
            Minus(_) => String::from("-"),
            Asterisk(_) => String::from("*"),
            ForwardSlash(_) => String::from("/"),
            Caret(_) => String::from("^"),
            Percent(_) => String::from("%"),
            Equals(_) => String::from("="),
            DoubleEquals(_) => String::from("=="),
            NotEqual(_) => String::from("!="),
            LessThan(_) => String::from("<"),
            LessThanOrEqual(_) => String::from("<="),
            GreaterThan(_) => String::from(">"),
            GreaterThanOrEqual(_) => String::from(">="),
            LogicalNot(_) => String::from("!"),
            LogicalAnd(_) => String::from("&&"),
            LogicalOr(_) => String::from("||"),
            LeftParen(_) => String::from("("),
            RightParen(_) => String::from(")"),
            LeftBrace(_) => String::from("{"),
            RightBrace(_) => String::from("}"),
            LeftBracket(_) => String::from("["),
            RightBracket(_) => String::from("]"),
            LeftMapLiteral(_) => String::from("[|"),
            RightMapLiteral(_) => String::from("|]"),
            LeftSetLiteral(_) => String::from("{|"),
            RightSetLiteral(_) => String::from("|}"),
            Period(_) => String::from("."),
            Comma(_) => String::from(","),
            Colon(_) => String::from(":"),
            Arrow(_) => String::from("->"),
            Pipe(_) => String::from("|"),
            Scope(_) => String::from("::"),
            Type(_) => String::from("type"),
            Let(_) => String::from("let"),
            Const(_) => String::from("const"),
            Def(_) => String::from("def"),
            Fun(_) => String::from("fun"),
            If(_) => String::from("if"),
            Else(_) => String::from("else"),
            While(_) => String::from("while"),
            Do(_) => String::from("do"),
            For(_) => String::from("for"),
            ForEach(_) => String::from("forEach"),
            In(_) => String::from("in"),
            Match(_) => String::from("match"),
            When(_) => String::from("when"),
            Return(_) => String::from("return"),
            Break(_) => String::from("break"),
            Continue(_) => String::from("continue"),
            UnitType(_) => String::from("unit"),
            BoolType(_) => String::from("bool"),
            StringType(_) => String::from("string"),
            ByteType(_) => String::from("byte"),
            IntType(_) => String::from("int"),
            FloatType(_) => String::from("float"),
            DoubleType(_) => String::from("double"),
            VecType(_) => String::from("vec"),
            SetType(_) => String::from("set"),
            MapType(_) => String::from("map"),
            Package(_) => String::from("package"),
            Import(_) => String::from("import"),
            As(_) => String::from("as"),
            Implement(_) => String::from("impl"),
            Extends(_) => String::from("extends"),
            Trait(_) => String::from("trait"),
            Sig(_) => String::from("sig"),
            Static(_) => String::from("static"),
            Mut(_) => String::from("mut"),
            Builtin(_) => String::from("__builtin"),
        }
    }
}
