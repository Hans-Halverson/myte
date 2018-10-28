pub const REPL_FILE_DESCRIPTOR: u32 = 0;

pub struct FileTable {
    repl_contents: String,
    files: Vec<String>,
}

impl FileTable {
    pub fn new() -> FileTable {
        FileTable {
            repl_contents: String::new(),
            files: Vec::new(),
        }
    }

    pub fn add_file(&mut self, file: &str) -> u32 {
        self.files.push(file.to_string());
        return self.files.len() as u32;
    }

    pub fn get_file(&self, file_descriptor: u32) -> String {
        self.files[(file_descriptor - 1) as usize].clone()
    }

    pub fn get_repl_contents(&self) -> String {
        self.repl_contents.clone()
    }

    pub fn set_repl_contents(&mut self, repl_contents: String) {
        self.repl_contents = repl_contents;
    }
}