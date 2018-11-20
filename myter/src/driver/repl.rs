use std::io::{self, Write};

use common::error::{self, ErrorContext, MyteError, MyteErrorType};
use common::ident::SymbolTable;
use common::source::{FileTable, REPL_FILE_DESCRIPTOR};
use interpreter::env::Environment;
use interpreter::evaluate;
use ir::ir::{IrStmt, IrStmtType};
use ir::resolution;
use lexer::tokenizer;
use parser::parser::Parser;

pub fn repl() {
    let mut file_table = FileTable::new();
    let mut symbol_table = SymbolTable::new();
    let mut error_context = ErrorContext::new();
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
        if current_input.len() == 0 {
            return;
        }

        file_table.set_repl_contents(current_input.clone());

        symbol_table.reset();
        env.reset();
        let old_symbol_table = symbol_table.clone();

        let tokens = match tokenizer::tokenize(current_input.as_bytes(), REPL_FILE_DESCRIPTOR) {
            Ok(tokens) => tokens,
            Err(MyteError {
                ty: MyteErrorType::UnexpectedEOF,
                ..
            }) => {
                current_line += 1;
                error_context = ErrorContext::new();
                continue;
            }
            Err(err) => {
                if let Err(err) = error::print_err(&err, &file_table) {
                    println!("{}", err);
                }

                current_input.clear();
                current_line = 0;
                error_context = ErrorContext::new();
                symbol_table = old_symbol_table;
                continue;
            }
        };

        let ast = {
            let ast_opt = {
                let mut parser = Parser::new(&tokens, &mut symbol_table, &mut error_context);
                parser.parse_repl_line()
            };

            if error_context.is_unexpected_eof() {
                error_context = ErrorContext::new();
                current_line += 1;
                continue;
            } else if !error_context.is_empty() {
                if let Err(err) = error_context.print_errors(&file_table) {
                    println!("{}", err);
                }

                current_input.clear();
                current_line = 0;
                error_context = ErrorContext::new();
                symbol_table = old_symbol_table;
                continue;
            }

            match ast_opt {
                Some(ast) => ast,
                None => {
                    current_input.clear();
                    current_line = 0;
                    error_context = ErrorContext::new();
                    symbol_table = old_symbol_table;
                    continue;
                }
            }
        };

        let ir = {
            let ir_opt = resolution::resolve_repl_line(ast, &mut symbol_table, &mut error_context);
            if !error_context.is_empty() {
                if let Err(err) = error_context.print_errors(&file_table) {
                    println!("{}", err);
                }

                current_input.clear();
                current_line = 0;
                error_context = ErrorContext::new();
                symbol_table = old_symbol_table;
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

        let value = match evaluate::evaluate_repl_line(ir, &mut env) {
            Ok(value) => value,
            Err(err) => {
                if let Err(err) = error::print_err(&err, &file_table) {
                    println!("{}", err);
                }

                current_input.clear();
                current_line = 0;
                error_context = ErrorContext::new();
                symbol_table = old_symbol_table;
                continue;
            }
        };

        if is_expr {
            println!("{}", value.to_string())
        }

        current_input.clear();
        current_line = 0;
        error_context = ErrorContext::new();
        symbol_table.reset();
    }
}
