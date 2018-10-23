use std::env;
use std::fs::File;
use std::io::{self, Read, Write};
use std::path::Path;

use common::error::{self, MyteError, MyteErrorType};
use common::source::{FileTable, REPL_FILE_DESCRIPTOR};
use interpreter::evaluate;
use ir::resolution;
use lexer::tokenizer;
use parser::parser::Parser;

mod common;
mod interpreter;
mod ir;
mod lexer;
mod parser;

fn evaluate_file(file_name: &str) {
    let mut file_table = FileTable::new();
    let file_descriptor = file_table.add_file(file_name);

    let path = Path::new(file_name);
    let mut file = match File::open(&path) {
        Ok(file) => file,
        Err(err) => {
            println!("Could not open {}: {}", path.display(), err.to_string());
            return;
        }
    };

    let mut file_bytes = Vec::new();
    if let Err(err) = file.read_to_end(&mut file_bytes) {
        println!("{}", err);
        return;
    }

    let tokens = match tokenizer::tokenize(&file_bytes, file_descriptor) {
        Ok(tokens) => tokens,
        Err(err) => {
            if let Err(err) = error::print_err(err, file_table) {
                println!("{}", err);
            }

            return;
        }
    };

    let mut parser = Parser::new(&tokens);
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(err) => {
            if let Err(err) = error::print_err(err, file_table) {
                println!("{}", err);
            }

            return;
        }
    };

    let ir = match resolution::resolve_ast(ast) {
        Ok(ir) => ir,
        Err(err) => {
            if let Err(err) = error::print_err(err, file_table) {
                println!("{}", err);
            }

            return;
        }
    };

    let value = match evaluate::evaluate(ir) {
        Ok(value) => value,
        Err(err) => {
            if let Err(err) = error::print_err(err, file_table) {
                println!("{}", err);
            }

            return;
        }
    };

    println!("{}", value.to_string());
}

fn repl() {
    let mut file_table = FileTable::new();

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

        let tokens = match tokenizer::tokenize(current_input.as_bytes(), REPL_FILE_DESCRIPTOR) {
            Ok(tokens) => tokens,
            Err(MyteError {
                ty: MyteErrorType::UnexpectedEOF,
                ..
            }) => {
                current_line += 1;
                continue;
            }
            Err(err) => {
                if let Err(err) = error::print_err(err, file_table) {
                    println!("{}", err);
                }

                return;
            }
        };

        let mut parser = Parser::new(&tokens);
        let ast = match parser.parse() {
            Ok(ast) => ast,
            Err(MyteError {
                ty: MyteErrorType::UnexpectedEOF,
                ..
            }) => {
                current_line += 1;
                continue;
            }
            Err(err) => {
                if let Err(err) = error::print_err(err, file_table) {
                    println!("{}", err);
                }

                return;
            }
        };

        let ir = match resolution::resolve_ast(ast) {
            Ok(ir) => ir,
            Err(err) => {
                if let Err(err) = error::print_err(err, file_table) {
                    println!("{}", err);
                }

                return;
            }
        };

        let value = match evaluate::evaluate(ir) {
            Ok(value) => value,
            Err(err) => {
                if let Err(err) = error::print_err(err, file_table) {
                    println!("{}", err);
                }

                return;
            }
        };

        println!("{}", value.to_string());

        current_input.clear();
        current_line = 0;
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        repl();
    } else {
        evaluate_file(&args[1]);
    }
}
