use common::context::Context;
use common::error::{mkerr, MyteError, MyteErrorType, MyteResult};
use common::ident::ScopeType;
use common::loc::Loc;
use lex::tokens::{Token, TokenType};
use parse::ast::{
    BinaryOp, Expr, ExprKind, FuncDecl, Module, Pat, PatKind, Stmt, StmtKind, TopLevel,
    TopLevelKind, Type, TypeKind, UnaryOp, VarDecl,
};
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

    pub fn parse_file(&mut self) -> Module {
        let mut top_levels = Vec::new();
        while !self.tokenizer.reached_end() {
            match self.parse_top_level() {
                Ok(definition) => top_levels.push(definition),
                Err(err) => self.ctx.error_ctx.add_error(err),
            }
        }

        Module { top_levels }
    }

    pub fn parse_repl_line(&mut self) -> Option<Stmt> {
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

    fn parse_top_level(&mut self) -> MyteResult<TopLevel> {
        let token = self.tokenizer.next()?;
        match token.ty {
            TokenType::Let => {
                let (loc, decl) = self.parse_variable_declaration(&token)?;
                Ok(TopLevel {
                    loc,
                    kind: TopLevelKind::VarDecl(decl),
                })
            }
            TokenType::Fun => {
                let (loc, decl) = self.parse_function_declaration(&token)?;
                Ok(TopLevel {
                    loc,
                    kind: TopLevelKind::FuncDecl(decl),
                })
            }
            _ => mkerr(
                "Only variable and function definitions are allowed on top level".to_string(),
                &token.loc,
                MyteErrorType::Parser,
            ),
        }
    }

    fn parse_stmt(&mut self) -> MyteResult<Stmt> {
        let token = self.tokenizer.next()?;
        match token.ty {
            TokenType::Let => {
                let (loc, decl) = self.parse_variable_declaration(&token)?;
                Ok(Stmt {
                    loc,
                    kind: StmtKind::VarDecl(decl),
                })
            }
            TokenType::Fun => {
                let (loc, decl) = self.parse_function_declaration(&token)?;
                Ok(Stmt {
                    loc,
                    kind: StmtKind::FuncDecl(decl),
                })
            }
            TokenType::LeftBrace => self.parse_block(&token),
            TokenType::If => self.parse_if_stmt(&token),
            TokenType::While => self.parse_while_stmt(&token),
            TokenType::Return => self.parse_return(&token),
            TokenType::Break => self.parse_break(&token),
            TokenType::Continue => self.parse_continue(&token),
            _ => Ok(Stmt {
                // TODO: Remove placeholder loc once expr stmts are terminated by semicolons
                loc: token.loc,
                kind: StmtKind::Expr(Box::new(
                    self.parse_expr_precedence(token, EXPR_PRECEDENCE_NONE)?,
                )),
            }),
        }
    }

    fn parse_expr(&mut self) -> MyteResult<Expr> {
        let first_token = self.tokenizer.next()?;
        self.parse_expr_precedence(first_token, EXPR_PRECEDENCE_NONE)
    }

    fn parse_expr_precedence(&mut self, first_token: Token, precedence: u32) -> MyteResult<Expr> {
        let mut expr = match first_token.ty {
            TokenType::True => Expr {
                loc: first_token.loc,
                kind: ExprKind::BoolLiteral(true),
            },
            TokenType::False => Expr {
                loc: first_token.loc,
                kind: ExprKind::BoolLiteral(false),
            },
            TokenType::StringLiteral(string) => Expr {
                loc: first_token.loc,
                kind: ExprKind::StringLiteral(string),
            },
            TokenType::IntLiteral(num) => Expr {
                loc: first_token.loc,
                kind: ExprKind::IntLiteral(num),
            },
            TokenType::FloatLiteral(num) => Expr {
                loc: first_token.loc,
                kind: ExprKind::FloatLiteral(num),
            },
            TokenType::Identifier(name) => self.parse_variable(&name, first_token.loc)?,
            TokenType::Plus => self.parse_unary_op(&first_token, UnaryOp::Plus)?,
            TokenType::Minus => self.parse_unary_op(&first_token, UnaryOp::Minus)?,
            TokenType::Bang => self.parse_unary_op(&first_token, UnaryOp::LogicalNot)?,
            TokenType::LeftParen => self.parse_parenthesized_expr(&first_token)?,
            TokenType::If => self.parse_if_expr(&first_token)?,
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

    fn parse_variable(&mut self, name: &str, loc: Loc) -> MyteResult<Expr> {
        Ok(Expr {
            loc,
            kind: ExprKind::Variable(self.ctx.symbol_table.unresolved_variable(name)),
        })
    }

    fn parse_unary_op(&mut self, op_token: &Token, op: UnaryOp) -> MyteResult<Expr> {
        let next_token = self.tokenizer.next()?;
        let precedence = match op {
            UnaryOp::Plus | UnaryOp::Minus => EXPR_PRECEDENCE_NUMERIC_PREFIX,
            UnaryOp::LogicalNot => EXPR_PRECEDENCE_LOGICAL_NOT,
        };

        let node = self.parse_expr_precedence(next_token, precedence)?;
        Ok(Expr {
            loc: Loc::between(&op_token.loc, &node.loc),
            kind: ExprKind::UnaryOp {
                op,
                node: Box::new(node),
            },
        })
    }

    fn parse_binary_op(&mut self, left_expr: Expr, op: BinaryOp) -> MyteResult<Expr> {
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
        Ok(Expr {
            loc: Loc::between(&left_expr.loc, &right_expr.loc),
            kind: ExprKind::BinaryOp {
                op,
                left: Box::new(left_expr),
                right: Box::new(right_expr),
            },
        })
    }

    fn parse_application(&mut self, left_expr: Expr) -> MyteResult<Expr> {
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

        let loc = Loc::between(&left_expr.loc, &self.tokenizer.current()?.loc);
        self.tokenizer.next()?;

        Ok(Expr {
            loc,
            kind: ExprKind::Application {
                func: Box::new(left_expr),
                args,
            },
        })
    }

    fn parse_assignment(&mut self, lvalue: Expr) -> MyteResult<Expr> {
        let (var, var_loc) = match lvalue {
            Expr {
                loc,
                kind: ExprKind::Variable(var),
            } => (var, loc),
            _ => {
                return mkerr(
                    "Left side of assignment must be variable".to_string(),
                    &lvalue.loc,
                    MyteErrorType::Parser,
                )
            }
        };

        let next_token = self.tokenizer.next()?;
        let expr = self.parse_expr_precedence(next_token, EXPR_PRECEDENCE_ASSIGNMENT)?;

        Ok(Expr {
            loc: Loc::between(&var_loc, &expr.loc),
            kind: ExprKind::Assignment {
                var,
                expr: Box::new(expr),
            },
        })
    }

    fn parse_parenthesized_expr(&mut self, left_paren: &Token) -> MyteResult<Expr> {
        if let TokenType::RightParen = self.tokenizer.current()?.ty {
            let right_paren = self.tokenizer.next()?;
            return Ok(Expr {
                loc: Loc::between(&left_paren.loc, &right_paren.loc),
                kind: ExprKind::UnitLiteral,
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
            Ok(Expr {
                loc: Loc::between(&left_paren.loc, &right_paren.loc),
                kind: ExprKind::ParenthesizedGroup(Box::new(elements.into_iter().nth(0).unwrap())),
            })
        } else {
            Ok(Expr {
                loc: Loc::between(&left_paren.loc, &right_paren.loc),
                kind: ExprKind::TupleLiteral(elements),
            })
        }
    }

    fn parse_block(&mut self, left_brace: &Token) -> MyteResult<Stmt> {
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
        Ok(Stmt {
            loc: Loc::between(&left_brace.loc, &right_brace.loc),
            kind: StmtKind::Block(nodes),
        })
    }

    fn parse_if_expr(&mut self, if_token: &Token) -> MyteResult<Expr> {
        let cond = self.parse_expr()?;
        let conseq = self.parse_expr()?;

        assert_current!(self, Else);
        self.tokenizer.next()?;

        let altern = self.parse_expr()?;

        Ok(Expr {
            loc: Loc::between(&if_token.loc, &altern.loc),
            kind: ExprKind::If {
                cond: Box::new(cond),
                conseq: Box::new(conseq),
                altern: Box::new(altern),
            },
        })
    }

    fn parse_return(&mut self, return_token: &Token) -> MyteResult<Stmt> {
        let expr = self.parse_expr()?;
        Ok(Stmt {
            loc: Loc::between(&return_token.loc, &expr.loc),
            kind: StmtKind::Return(Box::new(expr)),
        })
    }

    fn parse_break(&mut self, break_token: &Token) -> MyteResult<Stmt> {
        Ok(Stmt {
            loc: break_token.loc,
            kind: StmtKind::Break,
        })
    }

    fn parse_continue(&mut self, continue_token: &Token) -> MyteResult<Stmt> {
        Ok(Stmt {
            loc: continue_token.loc,
            kind: StmtKind::Continue,
        })
    }

    fn parse_variable_declaration(&mut self, start_token: &Token) -> MyteResult<(Loc, VarDecl)> {
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

        let loc = Loc::between(&start_token.loc, &rvalue.loc);
        let decl = VarDecl {
            lvalue: Box::new(self.resolve_def_pat(lvalue)),
            rvalue: Box::new(rvalue),
            annot,
        };

        Ok((loc, decl))
    }

    fn parse_function_declaration(&mut self, def_token: &Token) -> MyteResult<(Loc, FuncDecl)> {
        let ident_token = self.tokenizer.next()?;
        let name_id = if let Token {
            ty: TokenType::Identifier(name),
            loc,
        } = ident_token
        {
            self.ctx
                .symbol_table
                .add_function(&name, &loc, &mut self.ctx.error_ctx)
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
                loc,
                ty: TokenType::Identifier(name),
            } = param_token
            {
                self.ctx.symbol_table.add_variable(&name, &loc)
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
            } => {
                let expr = self.parse_expr()?;
                Stmt {
                    loc: expr.loc,
                    kind: StmtKind::Return(Box::new(expr)),
                }
            }
            Token {
                ty: TokenType::LeftBrace,
                ..
            } => self.parse_block(&current)?,
            other_token => return Err(unexpected_token(&other_token)),
        };

        self.ctx.symbol_table.exit_scope();

        let loc = Loc::between(&def_token.loc, &body.loc);
        let decl = FuncDecl {
            name: name_id,
            params: param_ids,
            body: Box::new(body),
            return_annot,
        };

        Ok((loc, decl))
    }

    fn parse_if_stmt(&mut self, if_token: &Token) -> MyteResult<Stmt> {
        let cond = self.parse_expr()?;
        let conseq = self.parse_expr()?;

        match self.tokenizer.current() {
            Ok(Token {
                ty: TokenType::Else,
                ..
            }) => {
                self.tokenizer.next()?;

                let altern = self.parse_expr()?;
                let loc = Loc::between(&if_token.loc, &altern.loc);

                Ok(Stmt {
                    loc,
                    kind: StmtKind::Expr(Box::new(Expr {
                        loc,
                        kind: ExprKind::If {
                            cond: Box::new(cond),
                            conseq: Box::new(conseq),
                            altern: Box::new(altern),
                        },
                    })),
                })
            }
            Ok(_)
            | Err(MyteError {
                ty: MyteErrorType::UnexpectedEOF,
                ..
            }) => Ok(Stmt {
                loc: Loc::between(&if_token.loc, &conseq.loc),
                kind: StmtKind::If {
                    cond: Box::new(cond),
                    conseq: Box::new(conseq),
                },
            }),
            Err(err) => Err(err),
        }
    }

    fn parse_while_stmt(&mut self, while_token: &Token) -> MyteResult<Stmt> {
        let cond = self.parse_expr()?;
        let body = self.parse_expr()?;
        Ok(Stmt {
            loc: Loc::between(&while_token.loc, &body.loc),
            kind: StmtKind::While {
                cond: Box::new(cond),
                body: Box::new(body),
            },
        })
    }

    fn parse_unresolved_pat(&mut self) -> MyteResult<UnresolvedPat> {
        match self.tokenizer.next()? {
            Token {
                ty: TokenType::Identifier(name),
                loc,
            } => Ok(UnresolvedPat::Variable { var: name, loc }),
            Token {
                ty: TokenType::LeftParen,
                loc,
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
                        loc: Loc::between(&loc, &right_paren.loc),
                    })
                }
            }
            token => mkerr(
                format!("Expected pattern, found {}", token.type_to_string()),
                &token.loc,
                MyteErrorType::Parser,
            ),
        }
    }

    fn resolve_def_pat(&mut self, pat: UnresolvedPat) -> Pat {
        match pat {
            UnresolvedPat::Variable { var, loc } => Pat {
                loc,
                kind: PatKind::Variable(self.ctx.symbol_table.add_variable(&var, &loc)),
            },
            UnresolvedPat::Tuple { elements, loc } => Pat {
                loc,
                kind: PatKind::Tuple(
                    elements
                        .into_iter()
                        .map(|element| self.resolve_def_pat(element))
                        .collect(),
                ),
            },
        }
    }

    fn parse_type(&mut self, in_def: bool) -> MyteResult<Type> {
        self.parse_type_precedence(in_def, TYPE_PRECEDENCE_NONE)
    }

    fn parse_type_precedence(&mut self, in_def: bool, precedence: u32) -> MyteResult<Type> {
        let first_token = self.tokenizer.next()?;
        let mut ty = match first_token.ty {
            TokenType::UnitType => Type {
                loc: first_token.loc,
                kind: TypeKind::Unit,
            },
            TokenType::BoolType => Type {
                loc: first_token.loc,
                kind: TypeKind::Bool,
            },
            TokenType::IntType => Type {
                loc: first_token.loc,
                kind: TypeKind::Int,
            },
            TokenType::FloatType => Type {
                loc: first_token.loc,
                kind: TypeKind::Float,
            },
            TokenType::StringType => Type {
                loc: first_token.loc,
                kind: TypeKind::String,
            },
            TokenType::Identifier(name) => {
                self.parse_variable_type(&name, first_token.loc, in_def)?
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

    fn parse_variable_type(&mut self, name: &str, loc: Loc, in_def: bool) -> MyteResult<Type> {
        Ok(Type {
            loc,
            kind: TypeKind::Variable(self.ctx.symbol_table.unresolved_type(&name, &loc, in_def)),
        })
    }

    fn parse_parenthesized_type(&mut self, left_paren: &Token, in_def: bool) -> MyteResult<Type> {
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
            Ok(Type {
                loc: Loc::between(&left_paren.loc, &right_paren.loc),
                kind: TypeKind::Tuple(tys),
            })
        }
    }

    fn parse_function_type(&mut self, left_ty: Type, in_def: bool) -> MyteResult<Type> {
        let right_ty = self.parse_type(in_def)?;
        let loc = Loc::between(&left_ty.loc, &right_ty.loc);

        let arg_tys = match left_ty.kind {
            TypeKind::Function(mut arg_tys, ret_ty) => {
                arg_tys.push(*ret_ty);
                arg_tys
            }
            _ => vec![left_ty],
        };

        Ok(Type {
            kind: TypeKind::Function(arg_tys, Box::new(right_ty)),
            loc,
        })
    }
}

enum UnresolvedPat {
    Variable {
        var: String,
        loc: Loc,
    },
    Tuple {
        elements: Vec<UnresolvedPat>,
        loc: Loc,
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
        &token.loc,
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
