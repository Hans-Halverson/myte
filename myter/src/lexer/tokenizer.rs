use common::error::{mkerr, MyteResult};
use common::span::Span;
use lexer::tokens::Token;

struct Tokenizer {
    bytes: Vec<u8>,
    bytes_idx: usize,
    current_byte: u32,
    current_line: u32,
    marked_byte: u32,
    marked_line: u32,
    file_descriptor: u32,
}

impl Tokenizer {
    fn new(bytes: Vec<u8>, file_descriptor: u32) -> Tokenizer {
        Tokenizer {
            bytes,
            bytes_idx: 0,
            current_byte: 0,
            current_line: 0,
            marked_byte: 0,
            marked_line: 0,
            file_descriptor,
        }
    }

    fn advance(&mut self) {
        if self.bytes_idx < self.bytes.len() {
            match self.current() {
                Some(b'\n') => {
                    self.current_byte = 0;
                    self.current_line += 1;
                }
                Some(_) => self.current_byte += 1,
                None => {}
            }

            self.bytes_idx += 1;
        }
    }

    fn current(&self) -> Option<u8> {
        if self.bytes_idx >= self.bytes.len() {
            None
        } else {
            Some(self.bytes[self.bytes_idx])
        }
    }

    fn next(&self) -> Option<u8> {
        if self.bytes_idx + 1 >= self.bytes.len() {
            None
        } else {
            Some(self.bytes[self.bytes_idx + 1])
        }
    }

    fn start_span(&mut self) {
        self.marked_byte = self.current_byte;
        self.marked_line = self.current_line;
    }

    fn mark_span(&self) -> Span {
        return Span::new(
            self.marked_byte,
            self.current_byte,
            self.marked_line,
            self.current_line,
            self.file_descriptor,
        );
    }

    fn mark_span_next(&self) -> Span {
        return Span::new(
            self.marked_byte,
            self.current_byte + 1,
            self.marked_line,
            self.current_line,
            self.file_descriptor,
        );
    }
}

fn is_whitespace_byte(byte: u8) -> bool {
    return byte == b' ' || byte == b'\t' || byte == b'\n';
}

fn is_number_byte(byte: u8) -> bool {
    return byte >= b'0' && byte <= b'9';
}

fn is_identifier_byte(byte: u8) -> bool {
    return (byte >= b'a' && byte <= b'z')
        || (byte >= b'A' && byte <= b'Z')
        || (byte >= b'0' && byte < b'9')
        || byte == b'_';
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

fn read_block_comment(tokenizer: &mut Tokenizer) -> MyteResult<()> {
    tokenizer.advance();

    let comment_open_span = tokenizer.mark_span();

    tokenizer.advance();

    loop {
        match tokenizer.current() {
            Some(b'*') => match tokenizer.next() {
                Some(b'/') => {
                    tokenizer.advance();
                    break;
                }
                Some(_) => {}
                None => return mkerr("Unclosed block comment".to_owned(), &comment_open_span),
            },
            Some(_) => {}
            None => return mkerr("Unclosed block comment".to_owned(), &comment_open_span),
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
        tokens.push(token_from_identifier(identifier, tokenizer.mark_span()));
    }
}

fn token_from_identifier(identifier: String, span: Span) -> Token {
    match identifier.as_ref() {
        "true" => Token::True(span),
        "false" => Token::False(span),
        "let" => Token::Let(span),
        "const" => Token::Const(span),
        "def" => Token::Def(span),
        "fun" => Token::Fun(span),
        "if" => Token::If(span),
        "else" => Token::Else(span),
        "while" => Token::While(span),
        "do" => Token::Do(span),
        "for" => Token::For(span),
        "forEach" => Token::ForEach(span),
        "in" => Token::In(span),
        "match" => Token::Match(span),
        "when" => Token::When(span),
        "return" => Token::Return(span),
        "break" => Token::Break(span),
        "continue" => Token::Continue(span),
        "unit" => Token::UnitType(span),
        "bool" => Token::BoolType(span),
        "string" => Token::StringType(span),
        "byte" => Token::ByteType(span),
        "int" => Token::IntType(span),
        "float" => Token::FloatType(span),
        "double" => Token::DoubleType(span),
        "vec" => Token::VecType(span),
        "map" => Token::MapType(span),
        "set" => Token::SetType(span),
        "package" => Token::Package(span),
        "import" => Token::Import(span),
        "as" => Token::As(span),
        "type" => Token::Type(span),
        "trait" => Token::Trait(span),
        "implement" => Token::Implement(span),
        "extends" => Token::Extends(span),
        "sig" => Token::Sig(span),
        "static" => Token::Static(span),
        "mut" => Token::Mut(span),
        "__builtin" => Token::Builtin(span),
        _ => Token::Identifier(identifier, span),
    }
}

fn read_string_literal(tokens: &mut Vec<Token>, tokenizer: &mut Tokenizer) -> MyteResult<()> {
    let mut identifier_bytes = Vec::new();
    identifier_bytes.push(tokenizer.current().unwrap());

    let open_quote_span = tokenizer.mark_span();

    loop {
        match tokenizer.next() {
            Some(b'"') => break,
            Some(byte) => identifier_bytes.push(byte),
            None => return mkerr("Unclosed string literal".to_owned(), &open_quote_span),
        }

        tokenizer.advance();
    }

    tokenizer.advance();

    unsafe {
        tokens.push(Token::StringLiteral(
            String::from_utf8_unchecked(identifier_bytes),
            tokenizer.mark_span(),
        ));
    }

    Ok(())
}

fn int_from_bytes(bytes: Vec<u8>, tokens: &mut Vec<Token>, span: Span) -> MyteResult<()> {
    unsafe {
        let int_string = String::from_utf8_unchecked(bytes);
        match int_string.parse::<i64>() {
            Ok(parsed_number) => {
                tokens.push(Token::IntLiteral(parsed_number, span));
                return Ok(());
            }
            _ => return mkerr(format!("Invalid int literal {}", int_string), &span),
        }
    }
}

fn float_from_bytes(bytes: Vec<u8>, tokens: &mut Vec<Token>, span: Span) -> MyteResult<()> {
    unsafe {
        let float_string = String::from_utf8_unchecked(bytes);
        match float_string.parse::<f64>() {
            Ok(parsed_number) => {
                tokens.push(Token::FloatLiteral(parsed_number, span));
                return Ok(());
            }
            _ => return mkerr(format!("Invalid float literal {}", float_string), &span),
        }
    }
}

fn read_number_literal(tokens: &mut Vec<Token>, tokenizer: &mut Tokenizer) -> MyteResult<()> {
    let mut number_bytes = Vec::new();
    number_bytes.push(tokenizer.current().unwrap());

    // Collect bytes before the decimal point
    loop {
        match tokenizer.next() {
            Some(byte) if is_number_byte(byte) => number_bytes.push(byte),
            Some(_) => break,
            None => return int_from_bytes(number_bytes, tokens, tokenizer.mark_span()),
        }

        tokenizer.advance();
    }

    // Collect decimal point
    match tokenizer.next() {
        Some(b'.') => number_bytes.push(b'.'),
        _ => return int_from_bytes(number_bytes, tokens, tokenizer.mark_span()),
    }

    tokenizer.advance();

    // Collect digits after decimal point
    loop {
        match tokenizer.next() {
            Some(byte) if is_number_byte(byte) => number_bytes.push(byte),
            Some(_) => break,
            None => return float_from_bytes(number_bytes, tokens, tokenizer.mark_span()),
        }

        tokenizer.advance();
    }

    // Collect exponent byte ('e' or 'E')
    match tokenizer.next() {
        Some(b'e') => number_bytes.push(b'e'),
        Some(b'E') => number_bytes.push(b'E'),
        _ => return float_from_bytes(number_bytes, tokens, tokenizer.mark_span()),
    }

    tokenizer.advance();

    // Collect optional exponent sign ('+' or '-')
    match tokenizer.next() {
        Some(byte) if byte == b'+' || byte == b'-' || is_number_byte(byte) => {
            number_bytes.push(byte)
        }
        _ => return float_from_bytes(number_bytes, tokens, tokenizer.mark_span()),
    }

    tokenizer.advance();

    // Collect exponent digits
    loop {
        match tokenizer.next() {
            Some(byte) if is_number_byte(byte) => number_bytes.push(byte),
            _ => return float_from_bytes(number_bytes, tokens, tokenizer.mark_span()),
        }

        tokenizer.advance();
    }
}

pub fn tokenize(input_bytes: Vec<u8>, file_descriptor: u32) -> MyteResult<Vec<Token>> {
    let mut tokenizer = Tokenizer::new(input_bytes, file_descriptor);
    let mut tokens = Vec::new();

    loop {
        tokenizer.start_span();
        match tokenizer.current() {
            Some(b'*') => tokens.push(Token::Asterisk(tokenizer.mark_span())),
            Some(b'^') => tokens.push(Token::Caret(tokenizer.mark_span())),
            Some(b'%') => tokens.push(Token::Percent(tokenizer.mark_span())),
            Some(b'(') => tokens.push(Token::LeftParen(tokenizer.mark_span())),
            Some(b')') => tokens.push(Token::RightParen(tokenizer.mark_span())),
            Some(b'}') => tokens.push(Token::RightBrace(tokenizer.mark_span())),
            Some(b']') => tokens.push(Token::RightBracket(tokenizer.mark_span())),
            Some(b'.') => tokens.push(Token::Period(tokenizer.mark_span())),
            Some(b',') => tokens.push(Token::Comma(tokenizer.mark_span())),
            Some(b'+') => match tokenizer.next() {
                Some(byte) if is_number_byte(byte) => {
                    read_number_literal(&mut tokens, &mut tokenizer)?
                }
                _ => tokens.push(Token::Plus(tokenizer.mark_span())),
            },
            Some(b'-') => match tokenizer.next() {
                Some(b'>') => advance_and_push(
                    Token::Arrow(tokenizer.mark_span_next()),
                    &mut tokens,
                    &mut tokenizer,
                ),
                Some(byte) if is_number_byte(byte) => {
                    read_number_literal(&mut tokens, &mut tokenizer)?
                }
                _ => tokens.push(Token::Minus(tokenizer.mark_span())),
            },
            Some(b'=') => match tokenizer.next() {
                Some(b'=') => advance_and_push(
                    Token::DoubleEquals(tokenizer.mark_span_next()),
                    &mut tokens,
                    &mut tokenizer,
                ),
                _ => tokens.push(Token::Equals(tokenizer.mark_span())),
            },
            Some(b'<') => match tokenizer.next() {
                Some(b'=') => advance_and_push(
                    Token::LessThanOrEqual(tokenizer.mark_span_next()),
                    &mut tokens,
                    &mut tokenizer,
                ),
                _ => tokens.push(Token::LessThan(tokenizer.mark_span())),
            },
            Some(b'>') => match tokenizer.next() {
                Some(b'=') => advance_and_push(
                    Token::GreaterThanOrEqual(tokenizer.mark_span_next()),
                    &mut tokens,
                    &mut tokenizer,
                ),
                _ => tokens.push(Token::GreaterThan(tokenizer.mark_span())),
            },
            Some(b'!') => match tokenizer.next() {
                Some(b'=') => advance_and_push(
                    Token::NotEqual(tokenizer.mark_span_next()),
                    &mut tokens,
                    &mut tokenizer,
                ),
                _ => tokens.push(Token::LogicalNot(tokenizer.mark_span())),
            },
            Some(b'&') => match tokenizer.next() {
                Some(b'&') => advance_and_push(
                    Token::LogicalAnd(tokenizer.mark_span_next()),
                    &mut tokens,
                    &mut tokenizer,
                ),
                Some(byte) => {
                    return mkerr(
                        format!("Unknown symbol &{}", byte as char),
                        &tokenizer.mark_span_next(),
                    )
                }
                None => return mkerr("Unkown symbol &".to_owned(), &tokenizer.mark_span()),
            },
            Some(b'|') => match tokenizer.next() {
                Some(b'|') => advance_and_push(
                    Token::LogicalOr(tokenizer.mark_span_next()),
                    &mut tokens,
                    &mut tokenizer,
                ),
                Some(b']') => advance_and_push(
                    Token::RightMapLiteral(tokenizer.mark_span_next()),
                    &mut tokens,
                    &mut tokenizer,
                ),
                Some(b'}') => advance_and_push(
                    Token::RightSetLiteral(tokenizer.mark_span_next()),
                    &mut tokens,
                    &mut tokenizer,
                ),
                _ => tokens.push(Token::Pipe(tokenizer.mark_span())),
            },
            Some(b'{') => match tokenizer.next() {
                Some(b'|') => advance_and_push(
                    Token::LeftSetLiteral(tokenizer.mark_span_next()),
                    &mut tokens,
                    &mut tokenizer,
                ),
                _ => tokens.push(Token::LeftBrace(tokenizer.mark_span())),
            },
            Some(b'[') => match tokenizer.next() {
                Some(b'|') => advance_and_push(
                    Token::LeftMapLiteral(tokenizer.mark_span_next()),
                    &mut tokens,
                    &mut tokenizer,
                ),
                _ => tokens.push(Token::LeftBracket(tokenizer.mark_span())),
            },
            Some(b':') => match tokenizer.next() {
                Some(b':') => advance_and_push(
                    Token::Scope(tokenizer.mark_span_next()),
                    &mut tokens,
                    &mut tokenizer,
                ),
                _ => tokens.push(Token::Colon(tokenizer.mark_span())),
            },
            Some(b'/') => match tokenizer.next() {
                Some(b'/') => read_line_comment(&mut tokenizer),
                Some(b'*') => read_block_comment(&mut tokenizer)?,
                _ => tokens.push(Token::ForwardSlash(tokenizer.mark_span())),
            },
            Some(b'"') => read_string_literal(&mut tokens, &mut tokenizer)?,
            Some(byte) if is_number_byte(byte) => read_number_literal(&mut tokens, &mut tokenizer)?,
            Some(byte) if is_identifier_byte(byte) => read_identifier(&mut tokens, &mut tokenizer),
            Some(byte) if is_whitespace_byte(byte) => {}
            Some(byte) => {
                return mkerr(
                    String::from(format!("Unknown symbol {}", byte as char)),
                    &tokenizer.mark_span(),
                )
            }
            None => break,
        }

        tokenizer.advance();
    }

    Ok(tokens)
}
