
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

pub type SourceRef = Rc<RefCell<SourceManager>>;

thread_local! {
    static SOURCES: RefCell<Vec<SourceRef>> = const { RefCell::new(Vec::new()) };
}

pub struct SourceProvider;

impl SourceProvider {
    pub fn add_source(source: SourceManager) -> SourceId {
        SOURCES.with(|sources| {
            let mut sources = sources.borrow_mut();
            sources.push(Rc::new(RefCell::new(source)));
            sources.len() - 1
        })
    }
    
    pub fn get_source(source_id: SourceId) -> Option<SourceRef> {
        SOURCES.with(|sources| {
            let sources = sources.borrow();
            sources.get(source_id).cloned()
        })
    }
}