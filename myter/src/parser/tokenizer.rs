use common::error::{mkerr, MyteErrorType, MyteResult};
use common::span::Span;
use lexer::tokens::Token;

pub struct Tokenizer<'a> {
    tokens: &'a [Token],
    current: usize,
}

impl<'a> Tokenizer<'a> {
    pub fn new(tokens: &'a [Token]) -> Tokenizer<'a> {
        Tokenizer { tokens, current: 0 }
    }

    pub fn current(&self) -> MyteResult<Token> {
        if self.current < self.tokens.len() {
            Ok(self.tokens[self.current].clone())
        } else {
            mkerr(
                "Unexpected end of file".to_string(),
                &Span::end_point_span(&self.tokens[self.tokens.len() - 1].span),
                MyteErrorType::UnexpectedEOF,
            )
        }
    }

    pub fn next(&mut self) -> MyteResult<Token> {
        if self.reached_end() {
            mkerr(
                "Unexpected end of file".to_string(),
                &Span::end_point_span(&self.tokens[self.tokens.len() - 1].span),
                MyteErrorType::UnexpectedEOF,
            )
        } else {
            let token = self.tokens[self.current].clone();
            self.current += 1;

            return Ok(token);
        }
    }

    pub fn reached_end(&self) -> bool {
        self.current == self.tokens.len()
    }
}
