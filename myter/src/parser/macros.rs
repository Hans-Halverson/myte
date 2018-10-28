macro_rules! token_type {
    (RightParen) => {
        ")"
    };
}

macro_rules! assert_current {
    ($self:ident, $expected:ident) => {{
        match $self.tokenizer.current()? {
            Token::$expected(..) => {}
            other => {
                return Err(MyteError::new(
                    format!(
                        "{} expected, but {} found",
                        token_type!($expected),
                        other.type_to_string(),
                    ),
                    other.span(),
                    MyteErrorType::Parser,
                ))
            }
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
