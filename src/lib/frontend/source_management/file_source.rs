
use crate::includes::{
    frontend::{
        source_management::*,
        lexing::*,
    }
};

use std::{
    fs::File,
    io::{Read, BufReader, BufRead},
    path::PathBuf,
};

pub struct FileSource {
    path: PathBuf,
}

impl FileSource {
    pub fn new(path: PathBuf) -> Self {
        Self { path }
    }
}

impl SourceManagerBody for FileSource {
    fn get_text(&self) -> LexingResult<String> {
        let mut file = File::open(&self.path).map_err(|e| LexingError::IOError(e.to_string()))?;
        let mut contents = String::new();
        file.read_to_string(&mut contents).map_err(|e| LexingError::IOError(e.to_string()))?;
        Ok(contents)
    }
    
    fn get_line(&self, line_number: usize) -> LexingResult<Option<String>> {
        let file = File::open(&self.path).map_err(|e| LexingError::IOError(e.to_string()))?;
        let reader = BufReader::new(file);
        
        for (current_line_number, line_result) in reader.lines().enumerate() {
            if current_line_number + 1 == line_number {
                return match line_result {
                    Ok(line) => Ok(Some(line)),
                    Err(e) => Err(LexingError::IOError(e.to_string())),
                };
            }
        }
        
        Ok(None)
    }
}