
use crate::includes::{
    essential::*,
    frontend::{
        source_management::*,
        lexing::*,
    }
};

wrapper_enum::wrapper_enum!{
    pub fwd bnd trait SourceManagerBody {
        pub fwd fn get_text(&self) -> LexingResult<String>
        
        pub fwd fn get_line(&self, line_number: usize) -> LexingResult<Option<String>>
    }
    
    pub enum SourceManager {
        FileSource(file_source: FileSource),
    }
}

pub type SourceId = usize;

pub type SourceRef = Arc<RwLock<SourceManager>>;
    
lazy_static::lazy_static! {
    pub static ref SOURCES: RwLock<Vec<SourceRef>> = RwLock::new(Vec::new());
}

pub struct SourceProvider;

impl SourceProvider {
    pub fn add_source(source: SourceManager) -> SourceId {
        SOURCES.write().unwrap().push(Arc::new(RwLock::new(source)));
        SOURCES.read().unwrap().len() - 1
    }
    
    pub fn get_source(source_id: SourceId) -> Option<SourceRef> {
        let sources = SOURCES.read().unwrap();
        sources.get(source_id).cloned()
    }
}