use std::env;

mod common;
mod driver;
mod interpreter;
mod ir;
mod lexer;
mod parser;
mod types;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        driver::repl::repl();
    } else {
        driver::interpret::interpret(&args[1..]);
    }
}
