use common::error::{MyteError, MyteErrorType, MyteResult};
use common::span::Span;
use lexer::tokens::Token;
use parser::ast::Ast;
use parser::tokenizer::Tokenizer;

pub struct Parser<'a> {
    tokenizer: Tokenizer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Parser<'a> {
        Parser {
            tokenizer: Tokenizer::new(tokens),
        }
    }

    pub fn parse(&mut self) -> MyteResult<Ast> {
        self.parse_expr()
    }

    fn parse_expr(&mut self) -> MyteResult<Ast> {
        let first_token = self.tokenizer.next()?;
        self.parse_expr_precedence(first_token, EXPR_PRECEDENCE_NONE)
    }

    fn parse_expr_precedence(&mut self, first_token: Token, precedence: u32) -> MyteResult<Ast> {
        let mut expr = match first_token {
            Token::True(span) => Ast::BoolLiteral { bool: true, span },
            Token::False(span) => Ast::BoolLiteral { bool: false, span },
            Token::StringLiteral(string, span) => Ast::StringLiteral { string, span },
            Token::IntLiteral(num, span) => Ast::IntLiteral { num, span },
            Token::FloatLiteral(num, span) => Ast::FloatLiteral { num, span },
            Token::Plus(..) => self.parse_unary_plus(first_token)?,
            Token::Minus(..) => self.parse_unary_minus(first_token)?,
            Token::LeftParen(..) => self.parse_parenthesized_expr(first_token)?,
            Token::Bang(..) => self.parse_logical_not(first_token)?,
            _ => return Err(unexpected_token(&first_token)),
        };

        if self.tokenizer.reached_end() {
            return Ok(expr);
        }

        while !self.tokenizer.reached_end()
            && precedence < expr_precedence(&self.tokenizer.current()?)
        {
            let current_token = self.tokenizer.next()?;
            expr = match current_token {
                Token::Plus(..) => self.parse_add(expr)?,
                Token::Minus(..) => self.parse_subtract(expr)?,
                Token::Asterisk(..) => self.parse_multiply(expr)?,
                Token::ForwardSlash(..) => self.parse_divide(expr)?,
                Token::Caret(..) => self.parse_exponentiate(expr)?,
                Token::Percent(..) => self.parse_remainder(expr)?,
                Token::DoubleAmpersand(..) => self.parse_logical_and(expr)?,
                Token::DoublePipe(..) => self.parse_logical_or(expr)?,
                _ => return Err(unexpected_token(&current_token)),
            }
        }

        Ok(expr)
    }

    fn parse_unary_plus(&mut self, plus: Token) -> MyteResult<Ast> {
        let next_token = self.tokenizer.next()?;
        let node = self.parse_expr_precedence(next_token, EXPR_PRECEDENCE_NUMERIC_PREFIX)?;
        Ok(Ast::UnaryPlus {
            span: Span::concat(plus.span(), node.span()),
            node: Box::new(node),
        })
    }

    fn parse_unary_minus(&mut self, plus: Token) -> MyteResult<Ast> {
        let next_token = self.tokenizer.next()?;
        let node = self.parse_expr_precedence(next_token, EXPR_PRECEDENCE_NUMERIC_PREFIX)?;
        Ok(Ast::UnaryMinus {
            span: Span::concat(plus.span(), node.span()),
            node: Box::new(node),
        })
    }

    fn parse_add(&mut self, left_expr: Ast) -> MyteResult<Ast> {
        let next_token = self.tokenizer.next()?;
        let right_expr = self.parse_expr_precedence(next_token, EXPR_PRECEDENCE_ADD)?;
        Ok(Ast::Add {
            span: Span::concat(left_expr.span(), right_expr.span()),
            left: Box::new(left_expr),
            right: Box::new(right_expr),
        })
    }

    fn parse_subtract(&mut self, left_expr: Ast) -> MyteResult<Ast> {
        let next_token = self.tokenizer.next()?;
        let right_expr = self.parse_expr_precedence(next_token, EXPR_PRECEDENCE_ADD)?;
        Ok(Ast::Subtract {
            span: Span::concat(left_expr.span(), right_expr.span()),
            left: Box::new(left_expr),
            right: Box::new(right_expr),
        })
    }

    fn parse_multiply(&mut self, left_expr: Ast) -> MyteResult<Ast> {
        let next_token = self.tokenizer.next()?;
        let right_expr = self.parse_expr_precedence(next_token, EXPR_PRECEDENCE_MULTIPLY)?;
        Ok(Ast::Multiply {
            span: Span::concat(left_expr.span(), right_expr.span()),
            left: Box::new(left_expr),
            right: Box::new(right_expr),
        })
    }

    fn parse_divide(&mut self, left_expr: Ast) -> MyteResult<Ast> {
        let next_token = self.tokenizer.next()?;
        let right_expr = self.parse_expr_precedence(next_token, EXPR_PRECEDENCE_MULTIPLY)?;
        Ok(Ast::Divide {
            span: Span::concat(left_expr.span(), right_expr.span()),
            left: Box::new(left_expr),
            right: Box::new(right_expr),
        })
    }

    fn parse_exponentiate(&mut self, left_expr: Ast) -> MyteResult<Ast> {
        let next_token = self.tokenizer.next()?;
        let right_expr = self
            .parse_expr_precedence(next_token, right_associative(EXPR_PRECEDENCE_EXPONENTIATE))?;
        Ok(Ast::Exponentiate {
            span: Span::concat(left_expr.span(), right_expr.span()),
            left: Box::new(left_expr),
            right: Box::new(right_expr),
        })
    }

    fn parse_remainder(&mut self, left_expr: Ast) -> MyteResult<Ast> {
        let next_token = self.tokenizer.next()?;
        let right_expr =
            self.parse_expr_precedence(next_token, right_associative(EXPR_PRECEDENCE_MULTIPLY))?;
        Ok(Ast::Remainder {
            span: Span::concat(left_expr.span(), right_expr.span()),
            left: Box::new(left_expr),
            right: Box::new(right_expr),
        })
    }

    fn parse_parenthesized_expr(&mut self, left_paren: Token) -> MyteResult<Ast> {
        if let Token::RightParen(..) = self.tokenizer.current()? {
            let right_paren = self.tokenizer.next()?;
            return Ok(Ast::UnitLiteral {
                span: Span::concat(left_paren.span(), right_paren.span()),
            });
        }

        let node = self.parse_expr()?;

        assert_current!(self, RightParen);
        let right_paren = self.tokenizer.next()?;

        Ok(Ast::ParenthesizedGroup {
            span: Span::concat(left_paren.span(), right_paren.span()),
            node: Box::new(node),
        })
    }

    fn parse_logical_not(&mut self, bang: Token) -> MyteResult<Ast> {
        let next_token = self.tokenizer.next()?;
        let node = self.parse_expr_precedence(next_token, EXPR_PRECEDENCE_LOGICAL_NOT)?;
        Ok(Ast::LogicalNot {
            span: Span::concat(bang.span(), node.span()),
            node: Box::new(node),
        })
    }

    fn parse_logical_and(&mut self, left_expr: Ast) -> MyteResult<Ast> {
        let next_token = self.tokenizer.next()?;
        let right_expr = self.parse_expr_precedence(next_token, EXPR_PRECEDENCE_LOGICAL_AND)?;
        Ok(Ast::LogicalAnd {
            span: Span::concat(left_expr.span(), right_expr.span()),
            left: Box::new(left_expr),
            right: Box::new(right_expr),
        })
    }

    fn parse_logical_or(&mut self, left_expr: Ast) -> MyteResult<Ast> {
        let next_token = self.tokenizer.next()?;
        let right_expr = self.parse_expr_precedence(next_token, EXPR_PRECEDENCE_LOGICAL_OR)?;
        Ok(Ast::LogicalOr {
            span: Span::concat(left_expr.span(), right_expr.span()),
            left: Box::new(left_expr),
            right: Box::new(right_expr),
        })
    }
}

///////////////////////////////////////////////////////////////////////////////
///
/// Errors
///
///////////////////////////////////////////////////////////////////////////////

fn unexpected_token(token: &Token) -> MyteError {
    MyteError::new(
        format!("Unexpected {} encountered", token.type_to_string()),
        token.span(),
        MyteErrorType::Parser,
    )
}

///////////////////////////////////////////////////////////////////////////////
///
/// Precedence
///
///////////////////////////////////////////////////////////////////////////////

const EXPR_PRECEDENCE_NONE: u32 = 0;
const EXPR_PRECEDENCE_LOGICAL_OR: u32 = 1;
const EXPR_PRECEDENCE_LOGICAL_AND: u32 = 2;
const EXPR_PRECEDENCE_LOGICAL_NOT: u32 = 3;
const EXPR_PRECEDENCE_ADD: u32 = 4;
const EXPR_PRECEDENCE_MULTIPLY: u32 = 5;
const EXPR_PRECEDENCE_EXPONENTIATE: u32 = 6;
const EXPR_PRECEDENCE_NUMERIC_PREFIX: u32 = 7;

fn expr_precedence(token: &Token) -> u32 {
    match token {
        Token::DoublePipe(..) => EXPR_PRECEDENCE_LOGICAL_OR,
        Token::DoubleAmpersand(..) => EXPR_PRECEDENCE_LOGICAL_AND,
        Token::Plus(..) | Token::Minus(..) => EXPR_PRECEDENCE_ADD,
        Token::Asterisk(..) | Token::ForwardSlash(..) => EXPR_PRECEDENCE_MULTIPLY,
        Token::Caret(..) => EXPR_PRECEDENCE_EXPONENTIATE,
        _ => EXPR_PRECEDENCE_NONE,
    }
}

fn right_associative(precedence: u32) -> u32 {
    precedence - 1
}
