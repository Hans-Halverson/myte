pub struct FileTable {
    files: Vec<String>,
}

impl FileTable {
    pub fn new() -> FileTable {
        FileTable { files: Vec::new() }
    }

    pub fn add(&mut self, file: &str) -> u32 {
        self.files.push(file.to_string());
        return (self.files.len() - 1) as u32;
    }

    pub fn get(&self, file_descriptor: u32) -> String {
        self.files[file_descriptor as usize].to_string()
    }
}
