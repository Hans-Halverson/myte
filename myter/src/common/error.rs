use common::file_table::FileTable;
use common::span::Span;

use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::path::Path;

pub struct MyteError {
    error: String,
    span: Span,
}

pub type MyteResult<T> = Result<T, MyteError>;

impl MyteError {
    pub fn new(error: String, span: &Span) -> MyteError {
        MyteError {
            error,
            span: span.clone(),
        }
    }
}

pub fn mkerr<T>(error: String, span: &Span) -> MyteResult<T> {
    Err(MyteError::new(error, span))
}

///////////////////////////////////////////////////////////////////////////////
///
/// Error Formatting
///
///////////////////////////////////////////////////////////////////////////////

const RESET_ATTRIBUTES: &str = "\u{001B}[0m";
const BOLD_ATTRIBUTE: &str = "\u{001B}[1m";
const RED_COLOR: &str = "\u{001B}[31m";
const RESET_COLOR: &str = "\u{001B}[39m";

struct ErrorFileReader {
    file_reader: BufReader<File>,
    current_line_num: u32,
    current_line: String,
}

impl ErrorFileReader {
    fn new(file_name: &str) -> io::Result<ErrorFileReader> {
        let file_reader = BufReader::new(File::open(&Path::new(&file_name))?);
        return Ok(ErrorFileReader {
            file_reader,
            current_line_num: 0,
            current_line: String::new(),
        });
    }

    fn read_line(&mut self, line_num: u32) -> io::Result<String> {
        while self.current_line_num <= line_num {
            self.current_line_num += 1;
            self.current_line.clear();
            self.file_reader.read_line(&mut self.current_line)?;
            self.current_line = self.current_line.trim_right().to_string();
        }

        return Ok(self.current_line.clone());
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

fn print_summary_line(err: &MyteError, file_name: &str) {
    let MyteError { error, span } = err;
    println!(
        "{}:{}:{}:{}{} error: {}{}{}",
        file_name,
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
    let padding = " ".repeat((max_num_digits - 2) as usize);

    println!(
        "{} ... {}{} |{}",
        BOLD_ATTRIBUTE, padding, RED_COLOR, RESET_ATTRIBUTES
    );
}

pub fn print_err(err: MyteError, file_table: FileTable) -> io::Result<()> {
    let span = &err.span;
    let file_name = file_table.get(span.file_descriptor);
    let mut file_reader = ErrorFileReader::new(&file_name)?;

    print_summary_line(&err, &file_name);

    // Print first line, or single line if span is on single line
    let first_line = file_reader.read_line(span.start_line)?;
    if span.start_line == span.end_line {
        print_single_line(&first_line, span);
        return Ok(());
    } else {
        print_first_line(&first_line, span);
    }

    // Print second line, or last line if two line span
    let second_line = file_reader.read_line(span.start_line + 1)?;
    if span.end_line == span.start_line + 1 {
        print_last_line(&second_line, span);
        return Ok(());
    } else {
        print_middle_line(&second_line, span, span.start_line + 1);
    }

    // Print third line, or last line if three line span
    let third_line = file_reader.read_line(span.start_line + 2)?;
    if span.end_line == span.start_line + 2 {
        print_last_line(&third_line, span);
        return Ok(());
    } else {
        print_middle_line(&third_line, span, span.start_line + 2);
    }

    // Print last line if four line span
    let fourth_line = file_reader.read_line(span.start_line + 3)?;
    if span.end_line == span.start_line + 3 {
        print_last_line(&fourth_line, span);
        return Ok(());
    // Print fourth and last lines if five line span
    } else if span.end_line == span.start_line + 4 {
        print_middle_line(&fourth_line, span, span.start_line + 3);

        let fifth_line = file_reader.read_line(span.start_line + 4)?;
        print_last_line(&fifth_line, span);

        return Ok(());
    }

    // If greater than five line span, print dots after third line
    print_dots_line(span);

    // Print second to last and last lines
    let second_to_last_line = file_reader.read_line(span.end_line - 1)?;
    print_middle_line(&second_to_last_line, span, span.end_line - 1);

    let last_line = file_reader.read_line(span.end_line)?;
    print_last_line(&last_line, span);

    Ok(())
}
