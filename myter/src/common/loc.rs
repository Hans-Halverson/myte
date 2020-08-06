#[derive(Clone, Copy, Debug)]
pub struct Loc {
    pub start_byte: u32,
    pub end_byte: u32,
    pub start_line: u32,
    pub end_line: u32,
    pub file_descriptor: u32,
}

impl Loc {
    pub fn new(
        start_byte: u32,
        end_byte: u32,
        start_line: u32,
        end_line: u32,
        file_descriptor: u32,
    ) -> Loc {
        Loc {
            start_byte,
            end_byte,
            start_line,
            end_line,
            file_descriptor,
        }
    }

    pub fn end_point_loc(loc: &Loc) -> Loc {
        Loc {
            start_byte: loc.end_byte,
            end_byte: loc.end_byte,
            start_line: loc.end_line,
            end_line: loc.end_line,
            file_descriptor: loc.file_descriptor,
        }
    }

    pub fn between(first: &Loc, second: &Loc) -> Loc {
        Loc {
            start_byte: first.start_byte,
            end_byte: second.end_byte,
            start_line: first.start_line,
            end_line: second.end_line,
            file_descriptor: first.file_descriptor,
        }
    }
}
