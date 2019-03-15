use std::io::{self, Write};

use common::context::Context;
use common::error::{self, MyteError, MyteErrorType};
use common::source;
use interpreter::env::Environment;
use interpreter::evaluate;
use ir::nodes::{IrStmt, IrStmtType};
use ir::resolution;
use lex::tokenizer;
use parse::parser::Parser;
use types::check;
use types::infer::InferType;

pub fn repl() {
    let mut ctx = Context::new_for_repl();
    let mut env = Environment::new();

    let stdin = io::stdin();
    let mut stdout = io::stdout();

    let mut current_input = String::new();
    let mut current_line = 0;

    loop {
        if current_line == 0 {
            print!(">> ");
        } else {
            print!("   ");
        }

        if let Err(err) = stdout.flush() {
            println!("{}", err);
            return;
        }

        if let Err(err) = stdin.read_line(&mut current_input) {
            println!("{}", err);
            return;
        }

        // Exit REPL if EOF is hit
        if current_input.is_empty() {
            return;
        }

        ctx.file_table.set_repl_contents(current_input.clone());

        ctx.symbol_table.reset();
        env.reset();

        let old_ctx = ctx.clone();

        let tokens =
            match tokenizer::tokenize(current_input.as_bytes(), source::REPL_FILE_DESCRIPTOR) {
                Ok(tokens) => tokens,
                Err(MyteError {
                    ty: MyteErrorType::UnexpectedEOF,
                    ..
                }) => {
                    current_line += 1;
                    ctx = old_ctx;
                    continue;
                }
                Err(err) => {
                    if let Err(err) = error::print_err(&err, &ctx) {
                        println!("{}", err);
                    }

                    current_input.clear();
                    current_line = 0;
                    ctx = old_ctx;
                    continue;
                }
            };

        let ast = {
            let ast_opt = {
                let mut parser = Parser::new(&tokens, &mut ctx);
                parser.parse_repl_line()
            };

            if ctx.error_ctx.is_unexpected_eof() {
                ctx = old_ctx;
                current_line += 1;
                continue;
            } else if !ctx.error_ctx.is_empty() {
                if let Err(err) = ctx.error_ctx.print_errors(&ctx) {
                    println!("{}", err);
                }

                current_input.clear();
                current_line = 0;
                ctx = old_ctx;
                continue;
            }

            match ast_opt {
                Some(ast) => ast,
                None => {
                    current_input.clear();
                    current_line = 0;
                    ctx = old_ctx;
                    continue;
                }
            }
        };

        let ir = {
            let ir_opt = resolution::resolve_repl_line(ast, &mut ctx);
            if !ctx.error_ctx.is_empty() {
                if let Err(err) = ctx.error_ctx.print_errors(&ctx) {
                    println!("{}", err);
                }

                current_input.clear();
                current_line = 0;
                ctx = old_ctx;
                continue;
            }

            ir_opt.unwrap()
        };

        let is_expr = match ir {
            IrStmt {
                node: IrStmtType::Expr(_),
                ..
            } => true,
            _ => false,
        };

        check::type_check_repl_line(&ir, &mut ctx);
        if !ctx.error_ctx.is_empty() {
            if let Err(err) = ctx.error_ctx.print_errors(&ctx) {
                println!("{}", err);
            }

            current_input.clear();
            current_line = 0;
            ctx = old_ctx;
            continue;
        }

        let value = match evaluate::evaluate_repl_line(&ir, &mut env, &mut ctx) {
            Ok(value) => value,
            Err(err) => {
                if let Err(err) = error::print_err(&err, &ctx) {
                    println!("{}", err);
                }

                current_input.clear();
                current_line = 0;
                ctx = old_ctx;
                continue;
            }
        };

        if is_expr {
            println!(
                "{} : {}",
                value.to_string(),
                InferType::format_types(&[ctx.infer_ctx.graph.rep(&value.ty())])[0]
            )
        }

        current_input.clear();
        current_line = 0;
    }
}
