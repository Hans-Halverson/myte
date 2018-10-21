#[derive(Debug)]
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
        return Span {
            start_byte,
            end_byte,
            start_line,
            end_line,
            file_descriptor,
        };
    }
}
