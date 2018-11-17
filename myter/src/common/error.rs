use common::source::{self, FileTable};
use common::span::Span;

use std::cmp::Ordering;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Read};
use std::path::Path;

#[derive(PartialEq)]
pub enum MyteErrorType {
    UnexpectedEOF,
    Lexer,
    Parser,
    Resolve,
    Evaluate,
}

pub struct MyteError {
    pub error: String,
    pub span: Span,
    pub ty: MyteErrorType,
}

pub struct ErrorContext {
    errors: Vec<MyteError>,
    unexpected_eof: Option<MyteError>,
}

pub type MyteResult<T> = Result<T, MyteError>;

impl MyteError {
    pub fn new(error: String, span: &Span, ty: MyteErrorType) -> MyteError {
        MyteError {
            error,
            span: span.clone(),
            ty,
        }
    }
}

pub fn mkerr<T>(error: String, span: &Span, ty: MyteErrorType) -> MyteResult<T> {
    Err(MyteError::new(error, span, ty))
}

impl ErrorContext {
    pub fn new() -> ErrorContext {
        ErrorContext {
            errors: Vec::new(),
            unexpected_eof: None,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.errors.is_empty() && self.unexpected_eof.is_none()
    }

    pub fn add_error(&mut self, error: MyteError) {
        if error.ty == MyteErrorType::UnexpectedEOF {
            self.unexpected_eof = Some(error);
        } else {
            self.errors.push(error);
        }
    }

    pub fn print_errors(&self, file_table: &FileTable) -> io::Result<()> {
        let mut all_errors = Vec::new();
        if let Some(err) = &self.unexpected_eof {
            all_errors.push(err);
        }

        for err in &self.errors {
            all_errors.push(err);
        }

        all_errors.sort_unstable_by(|err1, err2| {
            let file1 = file_table.get_file_name(err1.span.file_descriptor);
            let file2 = file_table.get_file_name(err2.span.file_descriptor);

            let file_ord = file1.cmp(&file2);
            if file_ord != Ordering::Equal {
                return file_ord;
            }

            let line_ord = err1.span.start_line.cmp(&err2.span.start_line);
            if line_ord != Ordering::Equal {
                return line_ord;
            }

            err1.span.start_byte.cmp(&err2.span.start_byte)
        });

        for error in all_errors {
            print_err(error, file_table)?;
            println!("");
        }

        Ok(())
    }

    pub fn is_unexpected_eof(&self) -> bool {
        self.errors.len() == 0 && self.unexpected_eof.is_some()
    }
}

///////////////////////////////////////////////////////////////////////////////
///
/// Error Formatting
///
///////////////////////////////////////////////////////////////////////////////

pub const ERROR_TAB_WIDTH: u32 = 4;

const RESET_ATTRIBUTES: &str = "\u{001B}[0m";
const BOLD_ATTRIBUTE: &str = "\u{001B}[1m";
const RED_COLOR: &str = "\u{001B}[31m";
const RESET_COLOR: &str = "\u{001B}[39m";

trait ErrorRead {
    fn read_line(&mut self, line_num: u32) -> io::Result<String>;
}

struct ErrorReader<T> {
    reader: BufReader<T>,
    current_line_num: u32,
    current_line: String,
}

impl<T: Read> ErrorReader<T> {
    fn from_file(file_name: String) -> io::Result<ErrorReader<File>> {
        let reader = BufReader::new(File::open(&Path::new(&file_name))?);
        Ok(ErrorReader {
            reader,
            current_line_num: 0,
            current_line: String::new(),
        })
    }

    fn from_repl<'a>(repl_contents: &'a str) -> ErrorReader<&'a [u8]> {
        ErrorReader {
            reader: BufReader::new(repl_contents.as_bytes()),
            current_line_num: 0,
            current_line: String::new(),
        }
    }
}

impl<T: Read> ErrorRead for ErrorReader<T> {
    fn read_line(&mut self, line_num: u32) -> io::Result<String> {
        while self.current_line_num <= line_num {
            self.current_line_num += 1;
            self.current_line.clear();
            self.reader.read_line(&mut self.current_line)?;
            self.current_line = self.current_line.trim_right().to_string();
        }

        return Ok(self
            .current_line
            .replace("\t", &" ".repeat(ERROR_TAB_WIDTH as usize)));
    }
}

/// Return the number of digits in the base-10 representation of a number.
fn digit_count(n: u32) -> u32 {
    return f64::log10(n as f64) as u32 + 1;
}

/// Pad a line number with whitespace for displaying in error messages.
fn pad_number(n: u32, max_num_digits: u32) -> String {
    let num_digits = digit_count(n);
    format!(
        " {}{} ",
        n,
        " ".repeat((max_num_digits - num_digits) as usize)
    )
}

fn print_summary_line(err: &MyteError, file_name: Option<&str>) {
    let MyteError { error, span, .. } = err;
    let file_name_prefix = match file_name {
        Some(file_name) => format!("{}:", file_name),
        None => String::new(),
    };

    println!(
        "{}{}:{}:{}{} error: {}{}{}",
        file_name_prefix,
        span.start_line + 1,
        span.start_byte + 1,
        BOLD_ATTRIBUTE,
        RED_COLOR,
        RESET_COLOR,
        error,
        RESET_ATTRIBUTES
    );
}

fn print_single_line(line: &str, span: &Span) {
    let max_num_digits = digit_count(span.start_line + 1);
    let padded_line_num = pad_number(span.start_line + 1, max_num_digits);
    let padded_carets = format!(
        "{}{}",
        " ".repeat(span.start_byte as usize),
        "^".repeat((span.end_byte - span.start_byte + 1) as usize)
    );

    println!(
        "{}{}|{} {}",
        BOLD_ATTRIBUTE, padded_line_num, RESET_ATTRIBUTES, line
    );
    println!(
        "{}{}|{} {}{}",
        BOLD_ATTRIBUTE,
        " ".repeat((max_num_digits + 2) as usize),
        RED_COLOR,
        padded_carets,
        RESET_ATTRIBUTES
    );
}

fn print_first_line(line: &str, span: &Span) {
    let max_num_digits = digit_count(span.end_line + 1);
    let padded_line_num = pad_number(span.start_line + 1, max_num_digits);
    let padded_carets = format!(
        "{}{}",
        " ".repeat(span.start_byte as usize),
        "^".repeat(line.len() - (span.start_byte as usize))
    );

    println!(
        "{}{}|{} / {}{}",
        BOLD_ATTRIBUTE, padded_line_num, RED_COLOR, RESET_ATTRIBUTES, line
    );
    println!(
        "{}{}|{} | {}{}",
        BOLD_ATTRIBUTE,
        " ".repeat((max_num_digits + 2) as usize),
        RED_COLOR,
        padded_carets,
        RESET_ATTRIBUTES
    );
}

fn print_last_line(line: &str, span: &Span) {
    let max_num_digits = digit_count(span.end_line + 1);
    let padded_line_num = pad_number(span.end_line + 1, max_num_digits);
    let padded_carets = "^".repeat((span.end_byte + 1) as usize);

    println!(
        "{}{}|{} | {}{}",
        BOLD_ATTRIBUTE, padded_line_num, RED_COLOR, RESET_ATTRIBUTES, line
    );
    println!(
        "{}{}|{} \\ {}{}",
        BOLD_ATTRIBUTE,
        " ".repeat((max_num_digits + 2) as usize),
        RED_COLOR,
        padded_carets,
        RESET_ATTRIBUTES
    );
}

fn print_middle_line(line: &str, span: &Span, line_num: u32) {
    let max_num_digits = digit_count(span.end_line + 1);
    let padded_line_num = pad_number(line_num + 1, max_num_digits);
    let padded_carets = "^".repeat(line.len());

    println!(
        "{}{}|{} | {}{}",
        BOLD_ATTRIBUTE, padded_line_num, RED_COLOR, RESET_ATTRIBUTES, line
    );
    println!(
        "{}{}|{} | {}{}",
        BOLD_ATTRIBUTE,
        " ".repeat((max_num_digits + 2) as usize),
        RED_COLOR,
        padded_carets,
        RESET_ATTRIBUTES
    );
}

fn print_dots_line(span: &Span) {
    let max_num_digits = digit_count(span.end_line + 1);
    let padding = " ".repeat((max_num_digits - 1) as usize);

    println!(
        "{} ...{}{} |{}",
        BOLD_ATTRIBUTE, padding, RED_COLOR, RESET_ATTRIBUTES
    );
}

pub fn print_err(err: &MyteError, file_table: &FileTable) -> io::Result<()> {
    let span = &err.span;
    let repl_contents = file_table.get_repl_contents();
    let mut error_reader: Box<ErrorRead> = if span.file_descriptor == source::REPL_FILE_DESCRIPTOR {
        print_summary_line(err, None);
        Box::new(ErrorReader::<&[u8]>::from_repl(&repl_contents))
    } else {
        let file_name = file_table.get_file_name(span.file_descriptor);
        print_summary_line(err, Some(&file_name));
        Box::new(ErrorReader::<File>::from_file(file_name)?)
    };

    // Print first line, or single line if span is on single line
    let first_line = error_reader.read_line(span.start_line)?;
    if span.start_line == span.end_line {
        print_single_line(&first_line, span);
        return Ok(());
    } else {
        print_first_line(&first_line, span);
    }

    // Print second line, or last line if two line span
    let second_line = error_reader.read_line(span.start_line + 1)?;
    if span.end_line == span.start_line + 1 {
        print_last_line(&second_line, span);
        return Ok(());
    } else {
        print_middle_line(&second_line, span, span.start_line + 1);
    }

    // Print third line, or last line if three line span
    let third_line = error_reader.read_line(span.start_line + 2)?;
    if span.end_line == span.start_line + 2 {
        print_last_line(&third_line, span);
        return Ok(());
    } else {
        print_middle_line(&third_line, span, span.start_line + 2);
    }

    // Print last line if four line span
    let fourth_line = error_reader.read_line(span.start_line + 3)?;
    if span.end_line == span.start_line + 3 {
        print_last_line(&fourth_line, span);
        return Ok(());
    // Print fourth and last lines if five line span
    } else if span.end_line == span.start_line + 4 {
        print_middle_line(&fourth_line, span, span.start_line + 3);

        let fifth_line = error_reader.read_line(span.start_line + 4)?;
        print_last_line(&fifth_line, span);

        return Ok(());
    }

    // If greater than five line span, print dots after third line
    print_dots_line(span);

    // Print second to last and last lines
    let second_to_last_line = error_reader.read_line(span.end_line - 1)?;
    print_middle_line(&second_to_last_line, span, span.end_line - 1);

    let last_line = error_reader.read_line(span.end_line)?;
    print_last_line(&last_line, span);

    Ok(())
}
