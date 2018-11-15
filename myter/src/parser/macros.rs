macro_rules! token_type {
    (Identifier) => {
        "identifier"
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
}

macro_rules! incorrect_token {
    ($expected:ident, $actual:ident) => {
        MyteError::new(
            format!(
                "{} expected, but {} found",
                token_type!($expected),
                $actual.type_to_string(),
            ),
            $actual.span(),
            MyteErrorType::Parser,
        )
    };
}

macro_rules! assert_current {
    ($self:ident, $expected:ident) => {{
        match $self.tokenizer.current()? {
            Token::$expected(..) => {}
            other => return Err(incorrect_token!($expected, other)),
        }
    }};
}

macro_rules! is_current {
    ($self:ident, $expected:ident) => {{
        match $self.tokenizer.current()? {
            Token::$expected(..) => true,
            _ => false,
        }
    }};
}
