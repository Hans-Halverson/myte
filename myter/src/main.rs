#![allow(clippy::map_entry)]

use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        myte::driver::repl::repl();
    } else {
        myte::driver::interpret::interpret(&args[1..]);
    }
}
