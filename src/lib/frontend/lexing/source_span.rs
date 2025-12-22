
use crate::includes::{
    essential::*,
    frontend::source_management::*,
};

#[derive(Clone, PartialEq, Eq)]
pub struct SourcePosition {
    pub line: usize,
    pub column: usize,
}

impl SourcePosition {
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
    
    pub fn line(&self) -> usize {
        self.line
    }
    
    pub fn column(&self) -> usize {
        self.column
    }
}

impl PartialOrd for SourcePosition {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for SourcePosition {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.line.cmp(&other.line) {
            std::cmp::Ordering::Equal => self.column.cmp(&other.column),
            other => other,
        }
    }
}

impl Debug for SourcePosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct SourceSpan {
    pub file: SourceId,
    pub start: SourcePosition,
    pub end: SourcePosition,
}

impl SourceSpan {
    pub fn new(file: SourceId, start: SourcePosition, end: SourcePosition) -> Self {
        Self { file, start, end }
    }
    
    pub fn file(&self) -> SourceId {
        self.file
    }
    
    pub fn start(&self) -> &SourcePosition {
        &self.start
    }
    
    pub fn end(&self) -> &SourcePosition {
        &self.end
    }
    
    pub fn merge(&self, other: &SourceSpan) -> Option<SourceSpan> {
        if self.file != other.file {
            return None;
        }
        
        let start = if self.start < other.start {
            self.start.clone()
        } else {
            other.start.clone()
        };
        
        let end = if self.end > other.end {
            self.end.clone()
        } else {
            other.end.clone()
        };
        
        Some(SourceSpan::new(self.file, start, end))
    }
    
    pub fn contains_pos(&self, position: &SourcePosition) -> bool {
        self.start <= *position && *position <= self.end
    }
    
    pub fn contains_span(&self, other: &SourceSpan) -> bool {
        if self.file != other.file {
            return false;
        }
        
        self.start <= other.start && other.end <= self.end
    }
    
    pub fn overlaps(&self, other: &SourceSpan) -> bool {
        if self.file != other.file {
            return false;
        }
        
        !(self.end < other.start || other.end < self.start)
    }
    
    pub fn get_overlap(&self, other: &SourceSpan) -> Option<SourceSpan> {
        if self.file != other.file {
            return None;
        }
        
        let start = if self.start > other.start {
            self.start.clone()
        } else {
            other.start.clone()
        };
        
        let end = if self.end < other.end {
            self.end.clone()
        } else {
            other.end.clone()
        };
        
        if start <= end {
            Some(SourceSpan::new(self.file, start, end))
        } else {
            None
        }
    }
}

impl Debug for SourceSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}[{:?}-{:?}]", self.file, self.start, self.end)
    }
}