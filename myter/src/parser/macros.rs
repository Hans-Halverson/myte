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
                        other.to_string()
                    ),
                    other.span(),
                ))
            }
        }
    }};
}
