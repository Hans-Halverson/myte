use common::context::Context;
use common::error::{mkerr, MyteError, MyteErrorType, MyteResult};
use common::ident::ScopeType;
use common::span::Span;
use lex::tokens::{Token, TokenType};
use parse::ast::{AstExpr, AstExprType, AstPat, AstStmt, AstType, AstTypeType, BinaryOp, UnaryOp};
use parse::tokenizer::Tokenizer;

pub struct Parser<'tok, 'ctx> {
    tokenizer: Tokenizer<'tok>,
    ctx: &'ctx mut Context,
}

impl<'tok, 'ctx> Parser<'tok, 'ctx> {
    pub fn new(tokens: &'tok [Token], ctx: &'ctx mut Context) -> Parser<'tok, 'ctx> {
        Parser {
            tokenizer: Tokenizer::new(tokens),
            ctx,
        }
    }

    pub fn parse_file(&mut self) -> Vec<AstStmt> {
        let mut definitions = Vec::new();
        while !self.tokenizer.reached_end() {
            match self.parse_top_level() {
                Ok(definition) => definitions.push(definition),
                Err(err) => self.ctx.error_ctx.add_error(err),
            }
        }

        definitions
    }

    pub fn parse_repl_line(&mut self) -> Option<AstStmt> {
        if self.tokenizer.reached_end() {
            return None;
        }

        let ast = match self.parse_stmt() {
            Ok(ast) => Some(ast),
            Err(err) => {
                self.ctx.error_ctx.add_error(err);
                return None;
            }
        };

        if !self.tokenizer.reached_end() {
            if let Ok(token) = self.tokenizer.current() {
                self.ctx.error_ctx.add_error(unexpected_token(&token))
            }

            return None;
        }

        ast
    }

    fn parse_top_level(&mut self) -> MyteResult<AstStmt> {
        let token = self.tokenizer.next()?;
        match token.ty {
            TokenType::Let => self.parse_variable_definition(&token),
            TokenType::Def => self.parse_function_definition(&token),
            _ => mkerr(
                "Only variable and function definitions are allowed on top level".to_string(),
                &token.span,
                MyteErrorType::Parser,
            ),
        }
    }

    fn parse_stmt(&mut self) -> MyteResult<AstStmt> {
        let token = self.tokenizer.next()?;
        match token.ty {
            TokenType::Let => self.parse_variable_definition(&token),
            TokenType::Def => self.parse_function_definition(&token),
            TokenType::If => self.parse_if_stmt(&token),
            TokenType::While => self.parse_while_stmt(&token),
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
        let mut expr = match first_token.ty {
            TokenType::True => AstExpr {
                span: first_token.span,
                node: AstExprType::BoolLiteral(true),
            },
            TokenType::False => AstExpr {
                span: first_token.span,
                node: AstExprType::BoolLiteral(false),
            },
            TokenType::StringLiteral(string) => AstExpr {
                span: first_token.span,
                node: AstExprType::StringLiteral(string),
            },
            TokenType::IntLiteral(num) => AstExpr {
                span: first_token.span,
                node: AstExprType::IntLiteral(num),
            },
            TokenType::FloatLiteral(num) => AstExpr {
                span: first_token.span,
                node: AstExprType::FloatLiteral(num),
            },
            TokenType::Identifier(name) => self.parse_variable(&name, first_token.span)?,
            TokenType::Plus => self.parse_unary_op(&first_token, UnaryOp::Plus)?,
            TokenType::Minus => self.parse_unary_op(&first_token, UnaryOp::Minus)?,
            TokenType::Bang => self.parse_unary_op(&first_token, UnaryOp::LogicalNot)?,
            TokenType::LeftParen => self.parse_parenthesized_expr(&first_token)?,
            TokenType::LeftBrace => self.parse_block(&first_token)?,
            TokenType::If => self.parse_if_expr(&first_token)?,
            TokenType::Return => self.parse_return(&first_token)?,
            TokenType::Break => self.parse_break(&first_token)?,
            TokenType::Continue => self.parse_continue(&first_token)?,
            _ => return Err(unexpected_token(&first_token)),
        };

        if self.tokenizer.reached_end() {
            return Ok(expr);
        }

        while !self.tokenizer.reached_end()
            && precedence < expr_precedence(&self.tokenizer.current()?)
        {
            let current_token = self.tokenizer.next()?;
            expr = match current_token.ty {
                TokenType::Plus => self.parse_binary_op(expr, BinaryOp::Add)?,
                TokenType::Minus => self.parse_binary_op(expr, BinaryOp::Subtract)?,
                TokenType::Asterisk => self.parse_binary_op(expr, BinaryOp::Multiply)?,
                TokenType::ForwardSlash => self.parse_binary_op(expr, BinaryOp::Divide)?,
                TokenType::Caret => self.parse_binary_op(expr, BinaryOp::Exponentiate)?,
                TokenType::Percent => self.parse_binary_op(expr, BinaryOp::Remainder)?,
                TokenType::DoubleAmpersand => self.parse_binary_op(expr, BinaryOp::LogicalAnd)?,
                TokenType::DoublePipe => self.parse_binary_op(expr, BinaryOp::LogicalOr)?,
                TokenType::DoubleEquals => self.parse_binary_op(expr, BinaryOp::Equals)?,
                TokenType::NotEqual => self.parse_binary_op(expr, BinaryOp::NotEqual)?,
                TokenType::LessThan => self.parse_binary_op(expr, BinaryOp::LessThan)?,
                TokenType::LessThanOrEqual => {
                    self.parse_binary_op(expr, BinaryOp::LessThanOrEqual)?
                }
                TokenType::GreaterThan => self.parse_binary_op(expr, BinaryOp::GreaterThan)?,
                TokenType::GreaterThanOrEqual => {
                    self.parse_binary_op(expr, BinaryOp::GreaterThanOrEqual)?
                }
                TokenType::LeftParen => self.parse_application(expr)?,
                TokenType::Equals => self.parse_assignment(expr)?,
                _ => return Err(unexpected_token(&current_token)),
            }
        }

        Ok(expr)
    }

    fn parse_variable(&mut self, name: &str, span: Span) -> MyteResult<AstExpr> {
        Ok(AstExpr {
            span,
            node: AstExprType::Variable(self.ctx.symbol_table.unresolved_variable(name)),
        })
    }

    fn parse_unary_op(&mut self, op_token: &Token, op: UnaryOp) -> MyteResult<AstExpr> {
        let next_token = self.tokenizer.next()?;
        let precedence = match op {
            UnaryOp::Plus | UnaryOp::Minus => EXPR_PRECEDENCE_NUMERIC_PREFIX,
            UnaryOp::LogicalNot => EXPR_PRECEDENCE_LOGICAL_NOT,
        };

        let node = self.parse_expr_precedence(next_token, precedence)?;
        Ok(AstExpr {
            span: Span::concat(&op_token.span, &node.span),
            node: AstExprType::UnaryOp {
                op,
                node: Box::new(node),
            },
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
        Ok(AstExpr {
            span: Span::concat(&left_expr.span, &right_expr.span),
            node: AstExprType::BinaryOp {
                op,
                left: Box::new(left_expr),
                right: Box::new(right_expr),
            },
        })
    }

    fn parse_application(&mut self, left_expr: AstExpr) -> MyteResult<AstExpr> {
        let mut args = Vec::new();

        while !is_current!(self, RightParen) {
            args.push(self.parse_expr()?);

            match self.tokenizer.current()? {
                Token {
                    ty: TokenType::RightParen,
                    ..
                } => break,
                Token {
                    ty: TokenType::Comma,
                    ..
                } => {
                    self.tokenizer.next()?;
                    continue;
                }
                other_token => return Err(unexpected_token(&other_token)),
            }
        }

        let span = Span::concat(&left_expr.span, &self.tokenizer.current()?.span);
        self.tokenizer.next()?;

        Ok(AstExpr {
            span,
            node: AstExprType::Application {
                func: Box::new(left_expr),
                args,
            },
        })
    }

    fn parse_assignment(&mut self, lvalue: AstExpr) -> MyteResult<AstExpr> {
        let (var, var_span) = match lvalue {
            AstExpr {
                span,
                node: AstExprType::Variable(var),
            } => (var, span),
            _ => {
                return mkerr(
                    "Left side of assignment must be variable".to_string(),
                    &lvalue.span,
                    MyteErrorType::Parser,
                )
            }
        };

        let next_token = self.tokenizer.next()?;
        let expr = self.parse_expr_precedence(next_token, EXPR_PRECEDENCE_ASSIGNMENT)?;

        Ok(AstExpr {
            span: Span::concat(&var_span, &expr.span),
            node: AstExprType::Assignment {
                var,
                expr: Box::new(expr),
            },
        })
    }

    fn parse_parenthesized_expr(&mut self, left_paren: &Token) -> MyteResult<AstExpr> {
        if let TokenType::RightParen = self.tokenizer.current()?.ty {
            let right_paren = self.tokenizer.next()?;
            return Ok(AstExpr {
                span: Span::concat(&left_paren.span, &right_paren.span),
                node: AstExprType::UnitLiteral,
            });
        }

        let mut elements = vec![self.parse_expr()?];
        while is_current!(self, Comma) {
            self.tokenizer.next()?;
            elements.push(self.parse_expr()?);
        }

        assert_current!(self, RightParen);
        let right_paren = self.tokenizer.next()?;

        if elements.len() == 1 {
            Ok(AstExpr {
                span: Span::concat(&left_paren.span, &right_paren.span),
                node: AstExprType::ParenthesizedGroup(Box::new(
                    elements.into_iter().nth(0).unwrap(),
                )),
            })
        } else {
            Ok(AstExpr {
                span: Span::concat(&left_paren.span, &right_paren.span),
                node: AstExprType::TupleLiteral(elements),
            })
        }
    }

    fn parse_block(&mut self, left_brace: &Token) -> MyteResult<AstExpr> {
        let mut nodes = Vec::new();

        self.ctx.symbol_table.enter_scope(ScopeType::Block);

        while !is_current!(self, RightBrace) {
            match self.parse_stmt() {
                Ok(node) => nodes.push(node),
                Err(err) => self.ctx.error_ctx.add_error(err),
            }
        }

        self.ctx.symbol_table.exit_scope();

        let right_brace = self.tokenizer.next()?;
        Ok(AstExpr {
            span: Span::concat(&left_brace.span, &right_brace.span),
            node: AstExprType::Block(nodes),
        })
    }

    fn parse_if_expr(&mut self, if_token: &Token) -> MyteResult<AstExpr> {
        let cond = self.parse_expr()?;
        let conseq = self.parse_expr()?;

        assert_current!(self, Else);
        self.tokenizer.next()?;

        let altern = self.parse_expr()?;

        Ok(AstExpr {
            span: Span::concat(&if_token.span, &altern.span),
            node: AstExprType::If {
                cond: Box::new(cond),
                conseq: Box::new(conseq),
                altern: Box::new(altern),
            },
        })
    }

    fn parse_return(&mut self, return_token: &Token) -> MyteResult<AstExpr> {
        let expr = self.parse_expr()?;
        Ok(AstExpr {
            span: Span::concat(&return_token.span, &expr.span),
            node: AstExprType::Return(Box::new(expr)),
        })
    }

    fn parse_break(&mut self, break_token: &Token) -> MyteResult<AstExpr> {
        Ok(AstExpr {
            span: break_token.span,
            node: AstExprType::Break,
        })
    }

    fn parse_continue(&mut self, continue_token: &Token) -> MyteResult<AstExpr> {
        Ok(AstExpr {
            span: continue_token.span,
            node: AstExprType::Continue,
        })
    }

    fn parse_variable_definition(&mut self, start_token: &Token) -> MyteResult<AstStmt> {
        let lvalue = self.parse_unresolved_pat()?;

        let annot = if is_current!(self, Colon) {
            self.tokenizer.next()?;
            Some(Box::new(self.parse_type(false)?))
        } else {
            None
        };

        assert_current!(self, Equals);
        self.tokenizer.next()?;

        let rvalue = self.parse_expr()?;

        Ok(AstStmt::VariableDefinition {
            span: Span::concat(&start_token.span, &rvalue.span),
            lvalue: Box::new(self.resolve_def_pat(lvalue)),
            rvalue: Box::new(rvalue),
            annot,
        })
    }

    fn parse_function_definition(&mut self, def_token: &Token) -> MyteResult<AstStmt> {
        let ident_token = self.tokenizer.next()?;
        let name_id = if let Token {
            ty: TokenType::Identifier(name),
            span,
        } = ident_token
        {
            self.ctx
                .symbol_table
                .add_function(&name, &span, &mut self.ctx.error_ctx)
        } else {
            return Err(incorrect_token!(Identifier, ident_token));
        };

        let mut param_ids = Vec::new();
        self.ctx.symbol_table.enter_scope(ScopeType::FunctionBody);

        assert_current!(self, LeftParen);
        self.tokenizer.next()?;

        while !is_current!(self, RightParen) {
            let param_token = self.tokenizer.next()?;
            let param_id = if let Token {
                span,
                ty: TokenType::Identifier(name),
            } = param_token
            {
                self.ctx.symbol_table.add_variable(&name, &span)
            } else {
                return Err(incorrect_token!(Identifier, param_token));
            };

            assert_current!(self, Colon);
            self.tokenizer.next()?;

            let ty = self.parse_type(true)?;

            param_ids.push((param_id, Box::new(ty)));

            match self.tokenizer.current()? {
                Token {
                    ty: TokenType::RightParen,
                    ..
                } => break,
                Token {
                    ty: TokenType::Comma,
                    ..
                } => {
                    self.tokenizer.next()?;
                    continue;
                }
                other_token => return Err(unexpected_token(&other_token)),
            }
        }

        self.tokenizer.next()?;

        let return_annot = if is_current!(self, Colon) {
            self.tokenizer.next()?;
            Some(Box::new(self.parse_type(false)?))
        } else {
            None
        };

        let current = self.tokenizer.next()?;
        let body = match current {
            Token {
                ty: TokenType::Equals,
                ..
            } => self.parse_expr()?,
            Token {
                ty: TokenType::LeftBrace,
                ..
            } => self.parse_block(&current)?,
            other_token => return Err(unexpected_token(&other_token)),
        };

        self.ctx.symbol_table.exit_scope();

        Ok(AstStmt::FunctionDefinition {
            span: Span::concat(&def_token.span, &body.span),
            name: name_id,
            params: param_ids,
            body: Box::new(AstExpr {
                span: body.span,
                node: AstExprType::Return(Box::new(body)),
            }),
            return_annot,
        })
    }

    fn parse_if_stmt(&mut self, if_token: &Token) -> MyteResult<AstStmt> {
        let cond = self.parse_expr()?;
        let conseq = self.parse_expr()?;

        match self.tokenizer.current() {
            Ok(Token {
                ty: TokenType::Else,
                ..
            }) => {
                self.tokenizer.next()?;

                let altern = self.parse_expr()?;

                Ok(AstStmt::Expr {
                    expr: Box::new(AstExpr {
                        span: Span::concat(&if_token.span, &altern.span),
                        node: AstExprType::If {
                            cond: Box::new(cond),
                            conseq: Box::new(conseq),
                            altern: Box::new(altern),
                        },
                    }),
                })
            }
            Ok(_)
            | Err(MyteError {
                ty: MyteErrorType::UnexpectedEOF,
                ..
            }) => Ok(AstStmt::If {
                span: Span::concat(&if_token.span, &conseq.span),
                cond: Box::new(cond),
                conseq: Box::new(conseq),
            }),
            Err(err) => Err(err),
        }
    }

    fn parse_while_stmt(&mut self, while_token: &Token) -> MyteResult<AstStmt> {
        let cond = self.parse_expr()?;
        let body = self.parse_expr()?;
        Ok(AstStmt::While {
            span: Span::concat(&while_token.span, &body.span),
            cond: Box::new(cond),
            body: Box::new(body),
        })
    }

    fn parse_unresolved_pat(&mut self) -> MyteResult<UnresolvedPat> {
        match self.tokenizer.next()? {
            Token {
                ty: TokenType::Identifier(name),
                span,
            } => Ok(UnresolvedPat::Variable { var: name, span }),
            Token {
                ty: TokenType::LeftParen,
                span,
            } => {
                let mut elements = vec![self.parse_unresolved_pat()?];
                while is_current!(self, Comma) {
                    self.tokenizer.next()?;
                    elements.push(self.parse_unresolved_pat()?);
                }

                assert_current!(self, RightParen);
                let right_paren = self.tokenizer.next()?;

                if elements.len() == 1 {
                    Ok(elements.into_iter().nth(0).unwrap())
                } else {
                    Ok(UnresolvedPat::Tuple {
                        elements,
                        span: Span::concat(&span, &right_paren.span),
                    })
                }
            }
            token => mkerr(
                format!("Expected pattern, found {}", token.type_to_string()),
                &token.span,
                MyteErrorType::Parser,
            ),
        }
    }

    fn resolve_def_pat(&mut self, pat: UnresolvedPat) -> AstPat {
        match pat {
            UnresolvedPat::Variable { var, span } => AstPat::Variable {
                var: self.ctx.symbol_table.add_variable(&var, &span),
                span,
            },
            UnresolvedPat::Tuple { elements, span } => AstPat::Tuple {
                elements: elements
                    .into_iter()
                    .map(|element| self.resolve_def_pat(element))
                    .collect(),
                span,
            },
        }
    }

    fn parse_type(&mut self, in_def: bool) -> MyteResult<AstType> {
        self.parse_type_precedence(in_def, TYPE_PRECEDENCE_NONE)
    }

    fn parse_type_precedence(&mut self, in_def: bool, precedence: u32) -> MyteResult<AstType> {
        let first_token = self.tokenizer.next()?;
        let mut ty = match first_token.ty {
            TokenType::UnitType => AstType {
                span: first_token.span,
                ty: AstTypeType::Unit,
            },
            TokenType::BoolType => AstType {
                span: first_token.span,
                ty: AstTypeType::Bool,
            },
            TokenType::IntType => AstType {
                span: first_token.span,
                ty: AstTypeType::Int,
            },
            TokenType::FloatType => AstType {
                span: first_token.span,
                ty: AstTypeType::Float,
            },
            TokenType::StringType => AstType {
                span: first_token.span,
                ty: AstTypeType::String,
            },
            TokenType::Identifier(name) => {
                self.parse_variable_type(&name, first_token.span, in_def)?
            }
            TokenType::LeftParen => self.parse_parenthesized_type(&first_token, in_def)?,
            _ => return Err(unexpected_token(&first_token)),
        };

        if self.tokenizer.reached_end() {
            return Ok(ty);
        }

        while !self.tokenizer.reached_end()
            && precedence < type_precedence(&self.tokenizer.current()?)
        {
            let current_token = self.tokenizer.next()?;
            ty = match current_token.ty {
                TokenType::Arrow => self.parse_function_type(ty, in_def)?,
                _ => return Err(unexpected_token(&current_token)),
            }
        }

        Ok(ty)
    }

    fn parse_variable_type(&mut self, name: &str, span: Span, in_def: bool) -> MyteResult<AstType> {
        Ok(AstType {
            span,
            ty: AstTypeType::Variable(self.ctx.symbol_table.unresolved_type(&name, &span, in_def)),
        })
    }

    fn parse_parenthesized_type(
        &mut self,
        left_paren: &Token,
        in_def: bool,
    ) -> MyteResult<AstType> {
        let mut tys = vec![self.parse_type(in_def)?];

        while is_current!(self, Comma) {
            self.tokenizer.next()?;
            tys.push(self.parse_type(in_def)?);
        }

        assert_current!(self, RightParen);
        let right_paren = self.tokenizer.next()?;

        if tys.len() == 1 {
            Ok(tys.into_iter().nth(0).unwrap())
        } else {
            Ok(AstType {
                span: Span::concat(&left_paren.span, &right_paren.span),
                ty: AstTypeType::Tuple(tys),
            })
        }
    }

    fn parse_function_type(&mut self, left_ty: AstType, in_def: bool) -> MyteResult<AstType> {
        let right_ty = self.parse_type(in_def)?;
        let span = Span::concat(&left_ty.span, &right_ty.span);

        let arg_tys = match left_ty.ty {
            AstTypeType::Function(mut arg_tys, ret_ty) => {
                arg_tys.push(*ret_ty);
                arg_tys
            }
            _ => vec![left_ty],
        };

        Ok(AstType {
            ty: AstTypeType::Function(arg_tys, Box::new(right_ty)),
            span,
        })
    }
}

enum UnresolvedPat {
    Variable {
        var: String,
        span: Span,
    },
    Tuple {
        elements: Vec<UnresolvedPat>,
        span: Span,
    },
}

///////////////////////////////////////////////////////////////////////////////
///
/// Errors
///
///////////////////////////////////////////////////////////////////////////////

fn unexpected_token(token: &Token) -> MyteError {
    MyteError::new(
        format!("Unexpected {} encountered", token.type_to_string()),
        &token.span,
        MyteErrorType::Parser,
    )
}

///////////////////////////////////////////////////////////////////////////////
///
/// Precedence
///
///////////////////////////////////////////////////////////////////////////////

const EXPR_PRECEDENCE_NONE: u32 = 0;
const EXPR_PRECEDENCE_ASSIGNMENT: u32 = 1;
const EXPR_PRECEDENCE_LOGICAL_OR: u32 = 2;
const EXPR_PRECEDENCE_LOGICAL_AND: u32 = 3;
const EXPR_PRECEDENCE_LOGICAL_NOT: u32 = 4;
const EXPR_PRECEDENCE_COMPARISON: u32 = 5;
const EXPR_PRECEDENCE_ADD: u32 = 6;
const EXPR_PRECEDENCE_MULTIPLY: u32 = 7;
const EXPR_PRECEDENCE_EXPONENTIATE: u32 = 8;
const EXPR_PRECEDENCE_NUMERIC_PREFIX: u32 = 9;
const EXPR_PRECEDENCE_APPLICATION: u32 = 10;

const TYPE_PRECEDENCE_NONE: u32 = 0;
const TYPE_PRECEDENCE_FUNCTION: u32 = 1;

fn expr_precedence(token: &Token) -> u32 {
    match token.ty {
        TokenType::DoublePipe => EXPR_PRECEDENCE_LOGICAL_OR,
        TokenType::DoubleAmpersand => EXPR_PRECEDENCE_LOGICAL_AND,
        TokenType::Plus | TokenType::Minus => EXPR_PRECEDENCE_ADD,
        TokenType::Asterisk | TokenType::ForwardSlash => EXPR_PRECEDENCE_MULTIPLY,
        TokenType::Caret => EXPR_PRECEDENCE_EXPONENTIATE,
        TokenType::LeftParen => EXPR_PRECEDENCE_APPLICATION,
        TokenType::Equals => EXPR_PRECEDENCE_ASSIGNMENT,
        TokenType::DoubleEquals
        | TokenType::NotEqual
        | TokenType::LessThan
        | TokenType::LessThanOrEqual
        | TokenType::GreaterThan
        | TokenType::GreaterThanOrEqual => EXPR_PRECEDENCE_COMPARISON,
        _ => EXPR_PRECEDENCE_NONE,
    }
}

fn type_precedence(token: &Token) -> u32 {
    match token.ty {
        TokenType::Arrow => TYPE_PRECEDENCE_FUNCTION,
        _ => TYPE_PRECEDENCE_NONE,
    }
}

fn right_associative(precedence: u32) -> u32 {
    precedence - 1
}