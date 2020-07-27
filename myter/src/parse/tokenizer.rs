use common::error::{mkerr, MyteErrorType, MyteResult};
use common::loc::Loc;
use lex::tokens::Token;

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
                &Loc::end_point_loc(&self.tokens[self.tokens.len() - 1].loc),
                MyteErrorType::UnexpectedEOF,
            )
        }
    }

    pub fn next(&mut self) -> MyteResult<Token> {
        if self.reached_end() {
            mkerr(
                "Unexpected end of file".to_string(),
                &Loc::end_point_loc(&self.tokens[self.tokens.len() - 1].loc),
                MyteErrorType::UnexpectedEOF,
            )
        } else {
            let token = self.tokens[self.current].clone();
            self.current += 1;

            Ok(token)
        }
    }

    pub fn reached_end(&self) -> bool {
        self.current == self.tokens.len()
    }
}
