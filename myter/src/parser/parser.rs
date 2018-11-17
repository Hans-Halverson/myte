use common::error::{mkerr, ErrorContext, MyteError, MyteErrorType, MyteResult};
use common::ident::SymbolTable;
use common::span::Span;
use lexer::tokens::Token;
use parser::ast::{AstExpr, AstPat, AstStmt, BinaryOp, UnaryOp};
use parser::tokenizer::Tokenizer;

pub struct Parser<'t, 's, 'e> {
    tokenizer: Tokenizer<'t>,
    symbol_table: &'s mut SymbolTable,
    error_context: &'e mut ErrorContext,
}

impl<'t, 's, 'e> Parser<'t, 's, 'e> {
    pub fn new(
        tokens: &'t [Token],
        symbol_table: &'s mut SymbolTable,
        error_context: &'e mut ErrorContext,
    ) -> Parser<'t, 's, 'e> {
        Parser {
            tokenizer: Tokenizer::new(tokens),
            symbol_table,
            error_context,
        }
    }

    pub fn parse(&mut self) -> Option<AstStmt> {
        if self.tokenizer.reached_end() {
            return None;
        }

        let stmt = self.parse_stmt();
        if !self.tokenizer.reached_end() {
            if let Ok(token) = self.tokenizer.current() {
                self.error_context.add_error(unexpected_token(&token))
            }

            return None;
        }

        match stmt {
            Ok(ast) => Some(ast),
            Err(err) => {
                self.error_context.add_error(err);
                None
            }
        }
    }

    fn parse_stmt(&mut self) -> MyteResult<AstStmt> {
        let token = self.tokenizer.next()?;
        match token {
            Token::Let(..) => self.parse_variable_definition(token),
            Token::Def(..) => self.parse_function_definition(token),
            Token::If(..) => self.parse_if_stmt(token),
            _ => Ok(AstStmt::Expr {
                expr: Box::new(self.parse_expr_precedence(token, EXPR_PRECEDENCE_NONE)?),
            }),
        }
    }

    fn parse_expr(&mut self) -> MyteResult<AstExpr> {
        let first_token = self.tokenizer.next()?;
        self.parse_expr_precedence(first_token, EXPR_PRECEDENCE_NONE)
    }

    fn parse_expr_precedence(
        &mut self,
        first_token: Token,
        precedence: u32,
    ) -> MyteResult<AstExpr> {
        let mut expr = match first_token {
            Token::True(span) => AstExpr::BoolLiteral { bool: true, span },
            Token::False(span) => AstExpr::BoolLiteral { bool: false, span },
            Token::StringLiteral(string, span) => AstExpr::StringLiteral { string, span },
            Token::IntLiteral(num, span) => AstExpr::IntLiteral { num, span },
            Token::FloatLiteral(num, span) => AstExpr::FloatLiteral { num, span },
            Token::Identifier(name, span) => self.parse_variable(name, span)?,
            Token::Plus(..) => self.parse_unary_op(first_token, UnaryOp::Plus)?,
            Token::Minus(..) => self.parse_unary_op(first_token, UnaryOp::Minus)?,
            Token::Bang(..) => self.parse_unary_op(first_token, UnaryOp::LogicalNot)?,
            Token::LeftParen(..) => self.parse_parenthesized_expr(first_token)?,
            Token::LeftBrace(..) => self.parse_block(first_token)?,
            Token::If(..) => self.parse_if_expr(first_token)?,
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
                Token::Plus(..) => self.parse_binary_op(expr, BinaryOp::Add)?,
                Token::Minus(..) => self.parse_binary_op(expr, BinaryOp::Subtract)?,
                Token::Asterisk(..) => self.parse_binary_op(expr, BinaryOp::Multiply)?,
                Token::ForwardSlash(..) => self.parse_binary_op(expr, BinaryOp::Divide)?,
                Token::Caret(..) => self.parse_binary_op(expr, BinaryOp::Exponentiate)?,
                Token::Percent(..) => self.parse_binary_op(expr, BinaryOp::Remainder)?,
                Token::DoubleAmpersand(..) => self.parse_binary_op(expr, BinaryOp::LogicalAnd)?,
                Token::DoublePipe(..) => self.parse_binary_op(expr, BinaryOp::LogicalOr)?,
                Token::DoubleEquals(..) => self.parse_binary_op(expr, BinaryOp::Equals)?,
                Token::NotEqual(..) => self.parse_binary_op(expr, BinaryOp::NotEqual)?,
                Token::LessThan(..) => self.parse_binary_op(expr, BinaryOp::LessThan)?,
                Token::LessThanOrEqual(..) => {
                    self.parse_binary_op(expr, BinaryOp::LessThanOrEqual)?
                }
                Token::GreaterThan(..) => self.parse_binary_op(expr, BinaryOp::GreaterThan)?,
                Token::GreaterThanOrEqual(..) => {
                    self.parse_binary_op(expr, BinaryOp::GreaterThanOrEqual)?
                }
                Token::LeftParen(..) => self.parse_application(expr)?,
                _ => return Err(unexpected_token(&current_token)),
            }
        }

        Ok(expr)
    }

    fn parse_variable(&mut self, name: String, span: Span) -> MyteResult<AstExpr> {
        Ok(AstExpr::Variable {
            var: self.symbol_table.unresolved_variable(&name),
            span,
        })
    }

    fn parse_unary_op(&mut self, op_token: Token, op: UnaryOp) -> MyteResult<AstExpr> {
        let next_token = self.tokenizer.next()?;
        let precedence = match op {
            UnaryOp::Plus | UnaryOp::Minus => EXPR_PRECEDENCE_NUMERIC_PREFIX,
            UnaryOp::LogicalNot => EXPR_PRECEDENCE_LOGICAL_NOT,
        };

        let node = self.parse_expr_precedence(next_token, precedence)?;
        Ok(AstExpr::UnaryOp {
            op,
            span: Span::concat(op_token.span(), node.span()),
            node: Box::new(node),
        })
    }

    fn parse_binary_op(&mut self, left_expr: AstExpr, op: BinaryOp) -> MyteResult<AstExpr> {
        let next_token = self.tokenizer.next()?;
        let precedence = match op {
            BinaryOp::Add | BinaryOp::Subtract => EXPR_PRECEDENCE_ADD,
            BinaryOp::Multiply | BinaryOp::Divide => EXPR_PRECEDENCE_MULTIPLY,
            BinaryOp::Exponentiate => right_associative(EXPR_PRECEDENCE_EXPONENTIATE),
            BinaryOp::Remainder => EXPR_PRECEDENCE_MULTIPLY,
            BinaryOp::LogicalAnd => EXPR_PRECEDENCE_LOGICAL_AND,
            BinaryOp::LogicalOr => EXPR_PRECEDENCE_LOGICAL_OR,
            BinaryOp::Equals
            | BinaryOp::NotEqual
            | BinaryOp::LessThan
            | BinaryOp::LessThanOrEqual
            | BinaryOp::GreaterThan
            | BinaryOp::GreaterThanOrEqual => EXPR_PRECEDENCE_COMPARISON,
        };

        let right_expr = self.parse_expr_precedence(next_token, precedence)?;
        Ok(AstExpr::BinaryOp {
            op,
            span: Span::concat(left_expr.span(), right_expr.span()),
            left: Box::new(left_expr),
            right: Box::new(right_expr),
        })
    }

    fn parse_application(&mut self, left_expr: AstExpr) -> MyteResult<AstExpr> {
        let mut args = Vec::new();

        while !is_current!(self, RightParen) {
            args.push(self.parse_expr()?);

            match self.tokenizer.current()? {
                Token::RightParen(..) => break,
                Token::Comma(..) => {
                    self.tokenizer.next()?;
                    continue;
                }
                other_token => return Err(unexpected_token(&other_token)),
            }
        }

        let span = Span::concat(left_expr.span(), self.tokenizer.current()?.span());
        self.tokenizer.next()?;

        Ok(AstExpr::Application {
            func: Box::new(left_expr),
            args,
            span,
        })
    }

    fn parse_parenthesized_expr(&mut self, left_paren: Token) -> MyteResult<AstExpr> {
        if let Token::RightParen(..) = self.tokenizer.current()? {
            let right_paren = self.tokenizer.next()?;
            return Ok(AstExpr::UnitLiteral {
                span: Span::concat(left_paren.span(), right_paren.span()),
            });
        }

        let node = self.parse_expr()?;

        assert_current!(self, RightParen);
        let right_paren = self.tokenizer.next()?;

        Ok(AstExpr::ParenthesizedGroup {
            span: Span::concat(left_paren.span(), right_paren.span()),
            node: Box::new(node),
        })
    }

    fn parse_block(&mut self, left_brace: Token) -> MyteResult<AstExpr> {
        let mut nodes = Vec::new();

        self.symbol_table.enter_scope();

        while !is_current!(self, RightBrace) {
            match self.parse_stmt() {
                Ok(node) => nodes.push(node),
                Err(err) => self.error_context.add_error(err),
            }
        }

        self.symbol_table.exit_scope();

        let right_brace = self.tokenizer.next()?;
        Ok(AstExpr::Block {
            nodes,
            span: Span::concat(left_brace.span(), right_brace.span()),
        })
    }

    fn parse_if_expr(&mut self, if_token: Token) -> MyteResult<AstExpr> {
        let cond = self.parse_expr()?;
        let conseq = self.parse_expr()?;

        assert_current!(self, Else);
        self.tokenizer.next()?;

        let altern = self.parse_expr()?;

        Ok(AstExpr::If {
            span: Span::concat(if_token.span(), altern.span()),
            cond: Box::new(cond),
            conseq: Box::new(conseq),
            altern: Box::new(altern),
        })
    }

    fn parse_variable_definition(&mut self, start_token: Token) -> MyteResult<AstStmt> {
        let lvalue = self.parse_pattern()?;

        assert_current!(self, Equals);
        self.tokenizer.next()?;

        let rvalue = self.parse_expr()?;

        Ok(AstStmt::VariableDefinition {
            span: Span::concat(start_token.span(), rvalue.span()),
            lvalue: Box::new(lvalue),
            rvalue: Box::new(rvalue),
        })
    }

    fn parse_function_definition(&mut self, def_token: Token) -> MyteResult<AstStmt> {
        let ident_token = self.tokenizer.next()?;
        let name_id = if let Token::Identifier(name, ..) = ident_token {
            self.symbol_table.add_variable(&name)
        } else {
            return Err(incorrect_token!(Identifier, ident_token));
        };

        let mut param_ids = Vec::new();
        self.symbol_table.enter_scope();

        assert_current!(self, LeftParen);
        self.tokenizer.next()?;

        while !is_current!(self, RightParen) {
            let param_token = self.tokenizer.next()?;
            let param_id = if let Token::Identifier(name, ..) = param_token {
                self.symbol_table.add_variable(&name)
            } else {
                return Err(incorrect_token!(Identifier, param_token));
            };

            param_ids.push(param_id);

            match self.tokenizer.current()? {
                Token::RightParen(..) => break,
                Token::Comma(..) => {
                    self.tokenizer.next()?;
                    continue;
                }
                other_token => return Err(unexpected_token(&other_token)),
            }
        }

        self.tokenizer.next()?;

        let current = self.tokenizer.next()?;
        let body = match current {
            Token::Equals(..) => self.parse_expr()?,
            Token::LeftBrace(..) => self.parse_block(current)?,
            other_token => return Err(unexpected_token(&other_token)),
        };

        Ok(AstStmt::FunctionDefinition {
            span: Span::concat(def_token.span(), body.span()),
            name: name_id,
            params: param_ids,
            body: Box::new(body),
        })
    }

    fn parse_if_stmt(&mut self, if_token: Token) -> MyteResult<AstStmt> {
        let cond = self.parse_expr()?;
        let conseq = self.parse_expr()?;

        match self.tokenizer.current() {
            Ok(Token::Else(..)) => {
                self.tokenizer.next()?;

                let altern = self.parse_expr()?;

                Ok(AstStmt::Expr {
                    expr: Box::new(AstExpr::If {
                        span: Span::concat(if_token.span(), altern.span()),
                        cond: Box::new(cond),
                        conseq: Box::new(conseq),
                        altern: Box::new(altern),
                    }),
                })
            }
            Ok(_)
            | Err(MyteError {
                ty: MyteErrorType::UnexpectedEOF,
                ..
            }) => Ok(AstStmt::If {
                span: Span::concat(if_token.span(), conseq.span()),
                cond: Box::new(cond),
                conseq: Box::new(conseq),
            }),
            Err(err) => Err(err),
        }
    }

    fn parse_pattern(&mut self) -> MyteResult<AstPat> {
        match self.tokenizer.next()? {
            Token::Identifier(name, span) => Ok(AstPat::Variable {
                var: self.symbol_table.add_variable(&name),
                span,
            }),
            token => mkerr(
                format!("Expected pattern, found {}", token.type_to_string()),
                token.span(),
                MyteErrorType::Parser,
            ),
        }
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
const EXPR_PRECEDENCE_COMPARISON: u32 = 4;
const EXPR_PRECEDENCE_ADD: u32 = 5;
const EXPR_PRECEDENCE_MULTIPLY: u32 = 6;
const EXPR_PRECEDENCE_EXPONENTIATE: u32 = 7;
const EXPR_PRECEDENCE_NUMERIC_PREFIX: u32 = 8;
const EXPR_APPLICATION: u32 = 9;

fn expr_precedence(token: &Token) -> u32 {
    match token {
        Token::DoublePipe(..) => EXPR_PRECEDENCE_LOGICAL_OR,
        Token::DoubleAmpersand(..) => EXPR_PRECEDENCE_LOGICAL_AND,
        Token::Plus(..) | Token::Minus(..) => EXPR_PRECEDENCE_ADD,
        Token::Asterisk(..) | Token::ForwardSlash(..) => EXPR_PRECEDENCE_MULTIPLY,
        Token::Caret(..) => EXPR_PRECEDENCE_EXPONENTIATE,
        Token::LeftParen(..) => EXPR_APPLICATION,
        Token::DoubleEquals(..)
        | Token::NotEqual(..)
        | Token::LessThan(..)
        | Token::LessThanOrEqual(..)
        | Token::GreaterThan(..)
        | Token::GreaterThanOrEqual(..) => EXPR_PRECEDENCE_COMPARISON,
        _ => EXPR_PRECEDENCE_NONE,
    }
}

fn right_associative(precedence: u32) -> u32 {
    precedence - 1
}
