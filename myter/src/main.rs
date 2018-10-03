use std::env;
use std::path::Path;
use std::fs::File;

use lexer::tokenizer;

mod common;
mod lexer;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        println!("myter repl not yet implemented. Exiting...");
        return
    }

    let path = Path::new(&args[1]);
    let mut file = match File::open(&path) {
        Ok(file) => file,
        Err(err) => {
            println!("Could not open {}: {}", path.display(), err.to_string());
            return
        }
    };

    let tokens = match tokenizer::tokenize(&mut file) {
        Ok(tokens) => tokens,
        Err(err) => {
            println!("{}", err);
            return
        }
    };

    println!("{:?}", tokens);
}
