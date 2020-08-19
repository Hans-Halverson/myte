use std::collections::HashMap;

pub struct Config {
    pub name: String,
    pub usage: String,
    pub options: Vec<OptionConfig>,
}

pub struct OptionConfig {
    pub name: String,
    pub desc: String,
    pub ty: OptionType,
}

#[derive(Clone)]
pub enum OptionType {
    Exists,
    String(String),
}

struct OptionParseData {
    name: String,
    ty: OptionType,
    matched: bool,
}

fn usage_message(config: &Config) -> String {
    let mut lines = vec![];

    lines.push(format!("usage: {}", config.usage));

    lines.join("\n")
}

pub fn parse(config: &Config, args: Vec<String>) -> Result<Args, String> {
    // Create map of parse data
    let mut options_data = HashMap::new();
    for OptionConfig { name, ty, .. } in &config.options {
        options_data.insert(
            name.to_string(),
            OptionParseData {
                name: name.to_string(),
                ty: ty.clone(),
                matched: false,
            },
        );
    }

    let mut options = HashMap::new();
    let mut positionals = vec![];
    let mut i = 0;
    while i < args.len() {
        if args[i] == "-h" || args[i] == "--help" {
            return Err(format!("Usage: "));
        }

        match options_data.get_mut(&args[i]) {
            Some(option_data) => {
                if option_data.matched {
                    return Err(format!("Error: option {} is matched twice", args[i]));
                }

                let value = match option_data.ty {
                    OptionType::Exists => OptionValue::Exists(true),
                    OptionType::String(_) => {
                        if i + 1 >= args.len() {
                            return Err(format!("Error: option {} requires argument", args[i]));
                        } else {
                            i += 1;
                            OptionValue::String(args[i].to_string())
                        }
                    }
                };

                options.insert(option_data.name.to_string(), value);
            }
            None => positionals.push(args[i].to_string()),
        }

        i += 1;
    }

    // Add results for unmatched exists options
    for option in options_data.values() {
        match option {
            OptionParseData {
                ty: OptionType::Exists,
                matched: false,
                ..
            } => {
                options.insert(option.name.to_string(), OptionValue::Exists(false));
            }
            _ => (),
        }
    }

    Ok(Args {
        options,
        positionals,
    })
}

enum OptionValue {
    Exists(bool),
    String(String),
}

pub struct Args {
    options: HashMap<String, OptionValue>,
    positionals: Vec<String>,
}

impl Args {
    pub fn get_exists(&self, name: &str) -> bool {
        match self.options.get(name) {
            Some(&OptionValue::Exists(b)) => b,
            _ => panic!("No exists option found with name {}", name),
        }
    }

    pub fn get_string(&self, name: &str) -> &str {
        match self.options.get(name) {
            Some(OptionValue::String(s)) => s,
            _ => panic!("No string option found with name {}", name),
        }
    }

    pub fn get_positionals(&self) -> &[String] {
        return &self.positionals;
    }
}
