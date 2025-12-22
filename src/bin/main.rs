
use lib::includes::{
    frontend::all::*,
};

fn main() {
    let test_file = std::path::PathBuf::from("test.mlg");
    let source = FileSource::new(test_file);
    let source_id = SourceProvider::add_source(SourceManager::FileSource(source));
    
    let lexer = Lexer::new(source_id)
        .expect("Failed to create lexer");
    
    println!("Lexing tokens...");
    let tokens = lexer.lex()
        .expect("Lexing failed");
    
    println!("Tokens: {:#?}", tokens);
}