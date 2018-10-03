use std::io::Read;

use lexer::tokens::Token;

struct Tokenizer {
    bytes: Vec<u8>,
    bytes_idx: usize,
    current_byte: u32,
    current_line: u32,
}

impl Tokenizer {
    fn new(bytes: Vec<u8>) -> Tokenizer {
        Tokenizer {
            bytes,
            bytes_idx: 0,
            current_byte: 0,
            current_line: 0,
        }
    }

    fn advance(&mut self) {
        if self.bytes_idx < self.bytes.len() {
            self.bytes_idx += 1;

            match self.current() {
                Some(b'\n') => {
                    self.current_byte = 0;
                    self.current_line += 1;
                }
                Some(_) => self.current_byte += 1,
                None => {}
            }
        }
    }

    fn current(& self) -> Option<u8> {
        if self.bytes_idx >= self.bytes.len() {
            None
        } else {
            Some(self.bytes[self.bytes_idx])
        }
    }

    fn next(& self) -> Option<u8> {
        if self.bytes_idx + 1 >= self.bytes.len() {
            None
        } else {
            Some(self.bytes[self.bytes_idx + 1])
        }
    }
}

fn is_whitespace_byte(byte: u8) -> bool {
    return byte == b' ' || byte == b'\t' || byte == b'\n'
}

fn is_number_byte(byte: u8) -> bool {
    return byte >= b'0' && byte <= b'9';
}

fn is_identifier_byte(byte: u8) -> bool {
    return (byte >= b'a' && byte <= b'z') ||
        (byte >= b'A' && byte <= b'Z') ||
        (byte >= b'0' && byte < b'9') ||
        byte == b'_';
}

fn advance_and_push(token: Token, tokens: &mut Vec<Token>, tokenizer: &mut Tokenizer) {
    tokenizer.advance();
    tokens.push(token);
}

fn read_line_comment(tokenizer: &mut Tokenizer) {
    tokenizer.advance();
    tokenizer.advance();

    while tokenizer.current() != Some(b'\n') {
        tokenizer.advance();
    }
}

fn read_block_comment(tokenizer: &mut Tokenizer) -> Result<(), String> {
    tokenizer.advance();
    tokenizer.advance();

    loop {
        match tokenizer.current() {
            Some(b'*') => match tokenizer.next() {
                Some(b'/') => {
                    tokenizer.advance();
                    break;
                }
                Some(_) => {}
                None => return Err("Unclosed block comment".to_owned()),
            }
            Some(_) => {}
            None => return Err("Unclosed block comment".to_owned()),
        }

        tokenizer.advance();
    }

    Ok(())
}

fn read_identifier(tokens: &mut Vec<Token>, tokenizer: &mut Tokenizer) {
    let mut identifier_bytes = Vec::new();
    identifier_bytes.push(tokenizer.current().unwrap());

    loop {
        match tokenizer.next() {
            Some(byte) if is_identifier_byte(byte) => identifier_bytes.push(byte),
            _ => break,
        }

        tokenizer.advance();
    }

    unsafe {
        let identifier = String::from_utf8_unchecked(identifier_bytes);
        tokens.push(token_from_identifier(identifier));
    }
}

fn token_from_identifier(identifier: String) -> Token {
    match identifier.as_ref() {
        "true" => Token::True,
        "false" => Token::False,
        "let" => Token::Let,
        "const" => Token::Const,
        "def" => Token::Def,
        "fun" => Token::Fun,
        "if" => Token::If,
        "else" => Token::Else,
        "while" => Token::While,
        "do" => Token::Do,
        "for" => Token::For,
        "forEach" => Token::ForEach,
        "in" => Token::In,
        "match" => Token::Match,
        "when" => Token::When,
        "return" => Token::Return,
        "break" => Token::Break,
        "continue" => Token::Continue,
        "unit" => Token::UnitType,
        "bool" => Token::BoolType,
        "string" => Token::StringType,
        "byte" => Token::ByteType,
        "int" => Token::IntType,
        "float" => Token::FloatType,
        "double" => Token::DoubleType,
        "vec" => Token::VecType,
        "map" => Token::MapType,
        "set" => Token::SetType,
        "package" => Token::Package,
        "import" => Token::Import,
        "as" => Token::As,
        "type" => Token::Type,
        "trait" => Token::Trait,
        "implement" => Token::Implement,
        "extends" => Token::Extends,
        "sig" => Token::Sig,
        "static" => Token::Static,
        "mut" => Token::Mut,
        "__builtin" => Token::Builtin,
        _ => Token::Identifier(identifier),
    }
}

fn read_string_literal(tokens: &mut Vec<Token>, tokenizer: &mut Tokenizer)
        -> Result<(), String> {
    let mut identifier_bytes = Vec::new();
    identifier_bytes.push(tokenizer.current().unwrap());

    loop {
        match tokenizer.next() {
            Some(b'"') => break,
            Some(byte) => identifier_bytes.push(byte),
            None => return Err("Unclosed string literal".to_owned()),
        }

        tokenizer.advance();
    }

    unsafe {
        tokens.push(Token::StringLiteral(String::from_utf8_unchecked(identifier_bytes)));
    }

    Ok(())
}

fn int_from_bytes(bytes: Vec<u8>, tokens: &mut Vec<Token>) -> Result<(), String> {
    unsafe {
        let int_string = String::from_utf8_unchecked(bytes);
        match int_string.parse::<i64>() {
            Ok(parsed_number) => {
                tokens.push(Token::IntLiteral(parsed_number));
                return Ok(());
            }
            _ => return Err(format!("Invalid int literal {}", int_string)),
        }
    }
}

fn float_from_bytes(bytes: Vec<u8>, tokens: &mut Vec<Token>) -> Result<(), String> {
    unsafe {
        let float_string = String::from_utf8_unchecked(bytes);
        match float_string.parse::<f64>() {
            Ok(parsed_number) => {
                tokens.push(Token::FloatLiteral(parsed_number));
                return Ok(());
            }
            _ => return Err(format!("Invalid float literal {}", float_string)),
        }
    }
}

fn read_number_literal(tokens: &mut Vec<Token>, tokenizer: &mut Tokenizer)
        -> Result<(), String> {
    let mut number_bytes = Vec::new();
    number_bytes.push(tokenizer.current().unwrap());

    // Collect bytes before the decimal point
    loop {
        match tokenizer.next() {
            Some(byte) if is_number_byte(byte) => number_bytes.push(byte),
            Some(_) => break,
            None => return int_from_bytes(number_bytes, tokens),
        }

        tokenizer.advance();
    }

    // Collect decimal point
    match tokenizer.next() {
        Some(b'.') => number_bytes.push(b'.'),
        _ => return int_from_bytes(number_bytes, tokens),
    }

    tokenizer.advance();

    // Collect digits after decimal point
    loop {
        match tokenizer.next() {
            Some(byte) if is_number_byte(byte) => number_bytes.push(byte),
            Some(_) => break,
            None => return float_from_bytes(number_bytes, tokens),
        }

        tokenizer.advance();
    }

    // Collect exponent byte ('e' or 'E')
    match tokenizer.next() {
        Some(b'e') => number_bytes.push(b'e'),
        Some(b'E') => number_bytes.push(b'E'),
        _ => return float_from_bytes(number_bytes, tokens),
    }

    tokenizer.advance();

    // Collect optional exponent sign ('+' or '-')
    match tokenizer.next() {
        Some(byte) if byte == b'+' || byte == b'-' || is_number_byte(byte) =>
            number_bytes.push(byte),
        _ => return float_from_bytes(number_bytes, tokens),
    }

    tokenizer.advance();

    // Collect exponent digits
    loop {
        match tokenizer.next() {
            Some(byte) if is_number_byte(byte) => number_bytes.push(byte),
            _ => return float_from_bytes(number_bytes, tokens),
        }

        tokenizer.advance();
    }
}

pub fn tokenize(reader: &mut impl Read) -> Result<Vec<Token>, String> {
    let mut input_bytes = Vec::new();

    if let Err(err) = reader.read_to_end(&mut input_bytes) {
        return Err(err.to_string());
    }

    let mut tokenizer = Tokenizer::new(input_bytes);
    let mut tokens = Vec::new();

    loop {
        match tokenizer.current() {
            Some(b'*') => tokens.push(Token::Asterisk),
            Some(b'^') => tokens.push(Token::Caret),
            Some(b'%') => tokens.push(Token::Percent),
            Some(b'(') => tokens.push(Token::LeftParen),
            Some(b')') => tokens.push(Token::RightParen),
            Some(b'}') => tokens.push(Token::RightBrace),
            Some(b']') => tokens.push(Token::RightBracket),
            Some(b'.') => tokens.push(Token::Period),
            Some(b',') => tokens.push(Token::Comma),
            Some(b'+') => match tokenizer.next() {
                Some(byte) if is_number_byte(byte) =>
                    read_number_literal(&mut tokens, &mut tokenizer)?,
                _ => tokens.push(Token::Plus),
            }
            Some(b'-') => match tokenizer.next() {
                Some(b'>') => advance_and_push(Token::Arrow, &mut tokens, &mut tokenizer),
                Some(byte) if is_number_byte(byte) =>
                    read_number_literal(&mut tokens, &mut tokenizer)?,
                _ => tokens.push(Token::Minus),
            }
            Some(b'=') => match tokenizer.next() {
                Some(b'=') => advance_and_push(Token::DoubleEquals, &mut tokens, &mut tokenizer),
                _ => tokens.push(Token::Equals),
            }
            Some(b'<') => match tokenizer.next() {
                Some(b'=') => advance_and_push(Token::LessThanOrEqual, &mut tokens, &mut tokenizer),
                _ => tokens.push(Token::LessThan),
            }
            Some(b'>') => match tokenizer.next() {
                Some(b'=') =>
                    advance_and_push(Token::GreaterThanOrEqual, &mut tokens, &mut tokenizer),
                _ => tokens.push(Token::GreaterThan),
            }
            Some(b'!') => match tokenizer.next() {
                Some(b'=') => advance_and_push(Token::NotEqual, &mut tokens, &mut tokenizer),
                _ => tokens.push(Token::LogicalNot),
            }
            Some(b'&') => match tokenizer.next() {
                Some(b'&') => advance_and_push(Token::LogicalAnd, &mut tokens, &mut tokenizer),
                Some(byte) => return Err(format!("Unknown symbol &{}", byte as char)),
                None => return Err("Unkown symbol &".to_owned()),
            }
            Some(b'|') => match tokenizer.next() {
                Some(b'|') => advance_and_push(Token::LogicalOr, &mut tokens, &mut tokenizer),
                Some(b']') => advance_and_push(Token::RightMapLiteral, &mut tokens, &mut tokenizer),
                Some(b'}') => advance_and_push(Token::RightSetLiteral, &mut tokens, &mut tokenizer),
                _ => tokens.push(Token::Pipe),
            }
            Some(b'{') => match tokenizer.next() {
                Some(b'|') => advance_and_push(Token::LeftSetLiteral, &mut tokens, &mut tokenizer),
                _ => tokens.push(Token::LeftBrace),
            }
            Some(b'[') => match tokenizer.next() {
                Some(b'|') => advance_and_push(Token::LeftMapLiteral, &mut tokens, &mut tokenizer),
                _ => tokens.push(Token::LeftBracket),
            }
            Some(b':') => match tokenizer.next() {
                Some(b':') => advance_and_push(Token::Scope, &mut tokens, &mut tokenizer),
                _ => tokens.push(Token::Colon),
            }
            Some(b'/') => match tokenizer.next() {
                Some(b'/') => read_line_comment(&mut tokenizer),
                Some(b'*') => read_block_comment(&mut tokenizer)?,
                _ => tokens.push(Token::ForwardSlash),
            }
            Some(b'"') => read_string_literal(&mut tokens, &mut tokenizer)?,
            Some(byte) if is_number_byte(byte) => read_number_literal(&mut tokens, &mut tokenizer)?,
            Some(byte) if is_identifier_byte(byte) => read_identifier(&mut tokens, &mut tokenizer),
            Some(byte) if is_whitespace_byte(byte) => {}
            Some(byte) => return Err(String::from(format!("Unknown symbol {}", byte as char))),
            None => break,
        }

        tokenizer.advance();
    }

    Ok(tokens)
}
