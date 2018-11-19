use std::fs::File;
use std::io::{self, Read};
use std::path::Path;

use common::error::{self, ErrorContext};
use common::ident::SymbolTable;
use common::source::FileTable;
use interpreter::env::Environment;
use interpreter::evaluate;
use ir::resolution;
use lexer::tokenizer;
use parser::ast::AstStmt;
use parser::parser::Parser;

pub fn interpret(file_names: &[String]) {
    let mut file_table = FileTable::new();
    let mut symbol_table = SymbolTable::new();
    let mut error_context = ErrorContext::new();
    let mut env = Environment::new();

    let file_asts = match parse_files(
        file_names,
        &mut file_table,
        &mut symbol_table,
        &mut error_context,
    ) {
        Ok(asts) => asts,
        Err(err) => {
            println!("{}", err.to_string());
            return;
        }
    };

    if !error_context.is_empty() {
        if let Err(err) = error_context.print_errors(&file_table) {
            println!("{}", err);
        }

        return;
    }

    let ir = file_asts
        .into_iter()
        .flat_map(|ast| resolution::resolve_file(ast, &mut symbol_table, &mut error_context))
        .collect();

    if !error_context.is_empty() {
        if let Err(err) = error_context.print_errors(&file_table) {
            println!("{}", err);
        }

        return;
    }

    if let Err(err) = evaluate::evaluate_files(ir, &mut env) {
        if let Err(err) = error::print_err(&err, &file_table) {
            println!("{}", err);
        }

        return;
    };

    let main_id = match symbol_table.get_main_id() {
        Some(id) => id,
        None => {
            error::print_err_string("No main function defined");
            return;
        }
    };

    let return_value = match evaluate::apply_main(main_id, &mut env, &symbol_table) {
        Ok(value) => value,
        Err(err) => {
            if let Err(err) = error::print_err(&err, &file_table) {
                println!("{}", err);
            }

            return;
        }
    };

    println!("{}", return_value.to_string())
}

fn parse_files(
    file_names: &[String],
    file_table: &mut FileTable,
    symbol_table: &mut SymbolTable,
    error_context: &mut ErrorContext,
) -> io::Result<Vec<Vec<AstStmt>>> {
    let mut file_asts = Vec::new();

    for file_name in file_names {
        let file_descriptor = file_table.add_file(file_name);

        let mut file = File::open(Path::new(file_name))?;
        let mut file_bytes = Vec::new();

        file.read_to_end(&mut file_bytes)?;

        let tokens = match tokenizer::tokenize(&file_bytes, file_descriptor) {
            Ok(tokens) => tokens,
            Err(err) => {
                error_context.add_error(err);
                continue;
            }
        };

        let ast = {
            let mut parser = Parser::new(&tokens, symbol_table, error_context);
            parser.parse_file()
        };

        file_asts.push(ast);
    }

    Ok(file_asts)
}
