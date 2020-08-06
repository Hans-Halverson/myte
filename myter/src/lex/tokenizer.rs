use common::error::{mkerr, MyteErrorType, MyteResult, ERROR_TAB_WIDTH};
use common::loc::Loc;
use lex::tokens::{Token, TokenType};

struct Tokenizer<'a> {
    bytes: &'a [u8],
    bytes_idx: usize,
    current_byte: u32,
    current_line: u32,
    marked_byte: u32,
    marked_line: u32,
    file_descriptor: u32,
}

impl<'a> Tokenizer<'a> {
    fn new(bytes: &[u8], file_descriptor: u32) -> Tokenizer {
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
                Some(b'\t') => {
                    self.current_byte += ERROR_TAB_WIDTH;
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

    fn start_loc(&mut self) {
        self.marked_byte = self.current_byte;
        self.marked_line = self.current_line;
    }

    fn mark_loc(&self) -> Loc {
        Loc::new(
            self.marked_byte,
            self.current_byte,
            self.marked_line,
            self.current_line,
            self.file_descriptor,
        )
    }

    fn mark_loc_next(&self) -> Loc {
        Loc::new(
            self.marked_byte,
            self.current_byte + 1,
            self.marked_line,
            self.current_line,
            self.file_descriptor,
        )
    }
}

fn is_whitespace_byte(byte: u8) -> bool {
    byte == b' ' || byte == b'\t' || byte == b'\n'
}

fn is_number_byte(byte: u8) -> bool {
    byte >= b'0' && byte <= b'9'
}

fn is_identifier_byte(byte: u8) -> bool {
    (byte >= b'a' && byte <= b'z')
        || (byte >= b'A' && byte <= b'Z')
        || (byte >= b'0' && byte < b'9')
        || byte == b'_'
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

    let comment_open_loc = tokenizer.mark_loc();

    tokenizer.advance();

    loop {
        match tokenizer.current() {
            Some(b'*') => match tokenizer.next() {
                Some(b'/') => {
                    tokenizer.advance();
                    break;
                }
                Some(_) => {}
                None => return mk_lex_err("Unclosed block comment".to_owned(), &comment_open_loc),
            },
            Some(_) => {}
            None => return mk_lex_err("Unclosed block comment".to_owned(), &comment_open_loc),
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
        tokens.push(token_from_identifier(identifier, tokenizer.mark_loc()));
    }
}

fn token_from_identifier(identifier: String, loc: Loc) -> Token {
    let ty = match identifier.as_ref() {
        "true" => TokenType::True,
        "false" => TokenType::False,
        "let" => TokenType::Let,
        "const" => TokenType::Const,
        "fun" => TokenType::Fun,
        "if" => TokenType::If,
        "else" => TokenType::Else,
        "while" => TokenType::While,
        "do" => TokenType::Do,
        "for" => TokenType::For,
        "forEach" => TokenType::ForEach,
        "in" => TokenType::In,
        "match" => TokenType::Match,
        "when" => TokenType::When,
        "return" => TokenType::Return,
        "break" => TokenType::Break,
        "continue" => TokenType::Continue,
        "unit" => TokenType::UnitType,
        "bool" => TokenType::BoolType,
        "string" => TokenType::StringType,
        "byte" => TokenType::ByteType,
        "int" => TokenType::IntType,
        "float" => TokenType::FloatType,
        "double" => TokenType::DoubleType,
        "vec" => TokenType::VecType,
        "map" => TokenType::MapType,
        "set" => TokenType::SetType,
        "package" => TokenType::Package,
        "import" => TokenType::Import,
        "as" => TokenType::As,
        "type" => TokenType::Type,
        "trait" => TokenType::Trait,
        "implement" => TokenType::Implement,
        "extends" => TokenType::Extends,
        "sig" => TokenType::Sig,
        "static" => TokenType::Static,
        "mut" => TokenType::Mut,
        "__builtin" => TokenType::Builtin,
        _ => TokenType::Identifier(identifier),
    };

    Token { loc, ty }
}

fn read_string_literal(tokens: &mut Vec<Token>, tokenizer: &mut Tokenizer) -> MyteResult<()> {
    let mut identifier_bytes = Vec::new();

    let open_quote_loc = tokenizer.mark_loc();

    loop {
        match tokenizer.next() {
            Some(b'"') => break,
            Some(byte) => identifier_bytes.push(byte),
            None => {
                return mkerr(
                    "Unclosed string literal".to_owned(),
                    &open_quote_loc,
                    MyteErrorType::UnexpectedEOF,
                )
            }
        }

        tokenizer.advance();
    }

    tokenizer.advance();

    unsafe {
        tokens.push(Token {
            ty: TokenType::StringLiteral(String::from_utf8_unchecked(identifier_bytes)),
            loc: tokenizer.mark_loc(),
        });
    }

    Ok(())
}

fn int_from_bytes(bytes: Vec<u8>, tokens: &mut Vec<Token>, loc: Loc) -> MyteResult<()> {
    unsafe {
        let int_string = String::from_utf8_unchecked(bytes);
        match int_string.parse::<i64>() {
            Ok(parsed_number) => {
                tokens.push(Token {
                    ty: TokenType::IntLiteral(parsed_number),
                    loc,
                });
                Ok(())
            }
            _ => mk_lex_err(format!("Invalid int literal {}", int_string), &loc),
        }
    }
}

fn float_from_bytes(bytes: Vec<u8>, tokens: &mut Vec<Token>, loc: Loc) -> MyteResult<()> {
    unsafe {
        let float_string = String::from_utf8_unchecked(bytes);
        match float_string.parse::<f64>() {
            Ok(parsed_number) => {
                tokens.push(Token {
                    ty: TokenType::FloatLiteral(parsed_number),
                    loc,
                });
                Ok(())
            }
            _ => mk_lex_err(format!("Invalid float literal {}", float_string), &loc),
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
            None => return int_from_bytes(number_bytes, tokens, tokenizer.mark_loc()),
        }

        tokenizer.advance();
    }

    // Collect decimal point
    match tokenizer.next() {
        Some(b'.') => number_bytes.push(b'.'),
        _ => return int_from_bytes(number_bytes, tokens, tokenizer.mark_loc()),
    }

    tokenizer.advance();

    // Collect digits after decimal point
    loop {
        match tokenizer.next() {
            Some(byte) if is_number_byte(byte) => number_bytes.push(byte),
            Some(_) => break,
            None => return float_from_bytes(number_bytes, tokens, tokenizer.mark_loc()),
        }

        tokenizer.advance();
    }

    // Collect exponent byte ('e' or 'E')
    match tokenizer.next() {
        Some(b'e') => number_bytes.push(b'e'),
        Some(b'E') => number_bytes.push(b'E'),
        _ => return float_from_bytes(number_bytes, tokens, tokenizer.mark_loc()),
    }

    tokenizer.advance();

    // Collect optional exponent sign ('+' or '-')
    match tokenizer.next() {
        Some(byte) if byte == b'+' || byte == b'-' || is_number_byte(byte) => {
            number_bytes.push(byte)
        }
        _ => return float_from_bytes(number_bytes, tokens, tokenizer.mark_loc()),
    }

    tokenizer.advance();

    // Collect exponent digits
    loop {
        match tokenizer.next() {
            Some(byte) if is_number_byte(byte) => number_bytes.push(byte),
            _ => return float_from_bytes(number_bytes, tokens, tokenizer.mark_loc()),
        }

        tokenizer.advance();
    }
}

fn mark_token(ty: TokenType, tokenizer: &Tokenizer) -> Token {
    Token {
        ty,
        loc: tokenizer.mark_loc(),
    }
}

fn mark_next_token(ty: TokenType, tokenizer: &Tokenizer) -> Token {
    Token {
        ty,
        loc: tokenizer.mark_loc_next(),
    }
}

pub fn tokenize(input_bytes: &[u8], file_descriptor: u32) -> MyteResult<Vec<Token>> {
    let mut tokenizer = Tokenizer::new(input_bytes, file_descriptor);
    let mut tokens = Vec::new();

    loop {
        tokenizer.start_loc();
        match tokenizer.current() {
            Some(b'*') => tokens.push(mark_token(TokenType::Asterisk, &tokenizer)),
            Some(b'^') => tokens.push(mark_token(TokenType::Caret, &tokenizer)),
            Some(b'%') => tokens.push(mark_token(TokenType::Percent, &tokenizer)),
            Some(b'(') => tokens.push(mark_token(TokenType::LeftParen, &tokenizer)),
            Some(b')') => tokens.push(mark_token(TokenType::RightParen, &tokenizer)),
            Some(b'}') => tokens.push(mark_token(TokenType::RightBrace, &tokenizer)),
            Some(b']') => tokens.push(mark_token(TokenType::RightBracket, &tokenizer)),
            Some(b'.') => tokens.push(mark_token(TokenType::Period, &tokenizer)),
            Some(b',') => tokens.push(mark_token(TokenType::Comma, &tokenizer)),
            Some(b'+') => match tokenizer.next() {
                Some(byte) if is_number_byte(byte) => {
                    read_number_literal(&mut tokens, &mut tokenizer)?
                }
                _ => tokens.push(mark_token(TokenType::Plus, &tokenizer)),
            },
            Some(b'-') => match tokenizer.next() {
                Some(b'>') => advance_and_push(
                    mark_next_token(TokenType::Arrow, &tokenizer),
                    &mut tokens,
                    &mut tokenizer,
                ),
                Some(byte) if is_number_byte(byte) => {
                    read_number_literal(&mut tokens, &mut tokenizer)?
                }
                _ => tokens.push(mark_token(TokenType::Minus, &tokenizer)),
            },
            Some(b'=') => match tokenizer.next() {
                Some(b'=') => advance_and_push(
                    mark_next_token(TokenType::DoubleEquals, &tokenizer),
                    &mut tokens,
                    &mut tokenizer,
                ),
                _ => tokens.push(mark_token(TokenType::Equals, &tokenizer)),
            },
            Some(b'<') => match tokenizer.next() {
                Some(b'=') => advance_and_push(
                    mark_next_token(TokenType::LessThanOrEqual, &tokenizer),
                    &mut tokens,
                    &mut tokenizer,
                ),
                _ => tokens.push(mark_token(TokenType::LessThan, &tokenizer)),
            },
            Some(b'>') => match tokenizer.next() {
                Some(b'=') => advance_and_push(
                    mark_next_token(TokenType::GreaterThanOrEqual, &tokenizer),
                    &mut tokens,
                    &mut tokenizer,
                ),
                _ => tokens.push(mark_token(TokenType::GreaterThan, &tokenizer)),
            },
            Some(b'!') => match tokenizer.next() {
                Some(b'=') => advance_and_push(
                    mark_next_token(TokenType::NotEqual, &tokenizer),
                    &mut tokens,
                    &mut tokenizer,
                ),
                _ => tokens.push(mark_token(TokenType::Bang, &tokenizer)),
            },
            Some(b'&') => match tokenizer.next() {
                Some(b'&') => advance_and_push(
                    mark_next_token(TokenType::DoubleAmpersand, &tokenizer),
                    &mut tokens,
                    &mut tokenizer,
                ),
                Some(byte) => {
                    return mk_lex_err(
                        format!("Unknown symbol &{}", byte as char),
                        &tokenizer.mark_loc_next(),
                    )
                }
                None => return mk_lex_err("Unkown symbol &".to_owned(), &tokenizer.mark_loc()),
            },
            Some(b'|') => match tokenizer.next() {
                Some(b'|') => advance_and_push(
                    mark_next_token(TokenType::DoublePipe, &tokenizer),
                    &mut tokens,
                    &mut tokenizer,
                ),
                Some(b']') => advance_and_push(
                    mark_next_token(TokenType::RightMapLiteral, &tokenizer),
                    &mut tokens,
                    &mut tokenizer,
                ),
                Some(b'}') => advance_and_push(
                    mark_next_token(TokenType::RightSetLiteral, &tokenizer),
                    &mut tokens,
                    &mut tokenizer,
                ),
                _ => tokens.push(mark_token(TokenType::Pipe, &tokenizer)),
            },
            Some(b'{') => match tokenizer.next() {
                Some(b'|') => advance_and_push(
                    mark_next_token(TokenType::LeftSetLiteral, &tokenizer),
                    &mut tokens,
                    &mut tokenizer,
                ),
                _ => tokens.push(mark_token(TokenType::LeftBrace, &tokenizer)),
            },
            Some(b'[') => match tokenizer.next() {
                Some(b'|') => advance_and_push(
                    mark_next_token(TokenType::LeftMapLiteral, &tokenizer),
                    &mut tokens,
                    &mut tokenizer,
                ),
                _ => tokens.push(mark_token(TokenType::LeftBracket, &tokenizer)),
            },
            Some(b':') => match tokenizer.next() {
                Some(b':') => advance_and_push(
                    mark_next_token(TokenType::Scope, &tokenizer),
                    &mut tokens,
                    &mut tokenizer,
                ),
                _ => tokens.push(mark_token(TokenType::Colon, &tokenizer)),
            },
            Some(b'/') => match tokenizer.next() {
                Some(b'/') => read_line_comment(&mut tokenizer),
                Some(b'*') => read_block_comment(&mut tokenizer)?,
                _ => tokens.push(mark_token(TokenType::ForwardSlash, &tokenizer)),
            },
            Some(b'"') => read_string_literal(&mut tokens, &mut tokenizer)?,
            Some(byte) if is_number_byte(byte) => read_number_literal(&mut tokens, &mut tokenizer)?,
            Some(byte) if is_identifier_byte(byte) => read_identifier(&mut tokens, &mut tokenizer),
            Some(byte) if is_whitespace_byte(byte) => {}
            Some(byte) => {
                return mk_lex_err(
                    format!("Unknown symbol {}", byte as char),
                    &tokenizer.mark_loc(),
                )
            }
            None => break,
        }

        tokenizer.advance();
    }

    Ok(tokens)
}

fn mk_lex_err<T>(error: String, loc: &Loc) -> MyteResult<T> {
    mkerr(error, loc, MyteErrorType::Lexer)
}
