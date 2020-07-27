macro_rules! token_type {
    (Identifier) => {
        "identifier"
    };
    (LeftParen) => {
        "("
    };
    (RightParen) => {
        ")"
    };
    (Equals) => {
        "="
    };
    (Else) => {
        "else"
    };
    (Colon) => {
        ":"
    };
}

macro_rules! incorrect_token {
    ($expected:ident, $actual:ident) => {
        MyteError::new(
            format!(
                "Expected {} but found {}",
                token_type!($expected),
                $actual.type_to_string(),
            ),
            &$actual.loc,
            MyteErrorType::Parser,
        )
    };
}

macro_rules! assert_current {
    ($self:ident, $expected:ident) => {{
        match $self.tokenizer.current()? {
            Token {
                ty: TokenType::$expected,
                ..
            } => {}
            other => return Err(incorrect_token!($expected, other)),
        }
    }};
}

macro_rules! is_current {
    ($self:ident, $expected:ident) => {{
        match $self.tokenizer.current()? {
            Token {
                ty: TokenType::$expected,
                ..
            } => true,
            _ => false,
        }
    }};
}
