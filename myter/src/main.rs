use std::env;
use std::fs::File;
use std::io::Read;
use std::path::Path;

use common::error;
use common::file_table::FileTable;
use interpreter::evaluate;
use ir::resolution;
use lexer::tokenizer;
use parser::parser::Parser;

mod common;
mod interpreter;
mod ir;
mod lexer;
mod parser;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        println!("myter repl not yet implemented. Exiting...");
        return;
    }

    let mut file_table = FileTable::new();
    let file_descriptor = file_table.add(&args[1]);

    let path = Path::new(&args[1]);
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

    let tokens = match tokenizer::tokenize(file_bytes, file_descriptor) {
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
