use std::env;

extern crate myte;
use myte::common::cli_args::{parse, Config, OptionConfig, OptionType};

mod harness;

fn cli_config() -> Config {
    Config {
        name: "run_tests".to_owned(),
        usage: "run_tests <options>".to_owned(),
        options: vec![
            OptionConfig {
                name: "bin".to_owned(),
                desc: "The Myte binary to test".to_owned(),
                ty: OptionType::String("path".to_owned()),
            },
            OptionConfig {
                name: "filter".to_owned(),
                desc: "Run only tests which match a pattern".to_owned(),
                ty: OptionType::String("pattern".to_owned()),
            },
            OptionConfig {
                name: "record".to_owned(),
                desc: "Re-record snapshot tests".to_owned(),
                ty: OptionType::Exists,
            },
            OptionConfig {
                name: "tree".to_owned(),
                desc: "Include tree of all test failures in output.".to_owned(),
                ty: OptionType::Exists,
            },
            OptionConfig {
                name: "tree-full".to_owned(),
                desc: "Include tree of all test passes and failures in output".to_owned(),
                ty: OptionType::Exists,
            },
        ],
    }
}

fn main() {
    let config = cli_config();
    match parse(&config, env::args().collect()) {
        Ok(args) => println!("bin: {}", args.get_string("bin")),
        Err(err) => println!("{}", err),
    }
}
