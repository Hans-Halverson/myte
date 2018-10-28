#[derive(Clone, Copy, Debug)]
pub struct Span {
    pub start_byte: u32,
    pub end_byte: u32,
    pub start_line: u32,
    pub end_line: u32,
    pub file_descriptor: u32,
}

impl Span {
    pub fn new(
        start_byte: u32,
        end_byte: u32,
        start_line: u32,
        end_line: u32,
        file_descriptor: u32,
    ) -> Span {
        Span {
            start_byte,
            end_byte,
            start_line,
            end_line,
            file_descriptor,
        }
    }

    pub fn end_point_span(span: &Span) -> Span {
        Span {
            start_byte: span.end_byte,
            end_byte: span.end_byte,
            start_line: span.end_line,
            end_line: span.end_line,
            file_descriptor: span.file_descriptor,
        }
    }

    pub fn concat(first: &Span, second: &Span) -> Span {
        Span {
            start_byte: first.start_byte,
            end_byte: second.end_byte,
            start_line: first.start_line,
            end_line: second.end_line,
            file_descriptor: first.file_descriptor,
        }
    }
}