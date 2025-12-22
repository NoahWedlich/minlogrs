
use crate::includes::{
    frontend::{
        source_management::*,
        lexing::*,
    }
};

const KEYWORDS: [&str; 9] = [
    "type",
    "algebra",
    "term",
    "program_constant",
    "pred",
    "idp",
    "theorem",
    "axiom",
    "proof",
];


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexingError {
    IOError(String),
    InvalidSource(String),
    UnexpectedCharacter(char, SourceSpan),
    UnterminatedBlockComment(SourceSpan),
}

pub type LexingResult<T> = Result<T, LexingError>;

pub struct Lexer {
    pub source_id: SourceId,
}

impl Lexer {
    pub fn new(source_id: SourceId) -> LexingResult<Self> {
        if SourceProvider::get_source(source_id).is_none() {
            Err(LexingError::InvalidSource(format!("Source ID {} not found", source_id)))
        } else {
            Ok(Self { source_id, })
        }
    }
    
    pub fn lex(&self) -> LexingResult<Vec<Token>> {
        let source = SourceProvider::get_source(self.source_id)
            .ok_or_else(|| LexingError::InvalidSource(format!("Source ID {} not found", self.source_id)))?;
        let source_borrow = source.borrow();
        
        let text = source_borrow.get_text()?;
        let mut tokenizer = Tokenizer::new(self.source_id, &text);
        tokenizer.lex()
    }
}

struct Tokenizer<'src> {
    source_id: SourceId,
    chars: std::str::Chars<'src>,
    position: SourcePosition,
}

impl<'src> Tokenizer<'src> {
    pub fn new(source_id: SourceId, input: &'src str) -> Self {
        Self {
            source_id,
            chars: input.chars(),
            position: SourcePosition { line: 1, column: 1 },
        }
    }
    
    fn lex(&mut self) -> LexingResult<Vec<Token>> {
        let mut tokens = Vec::new();
        
        loop {
            let token = self.next_token()?;
            if token.is_end_of_file() {
                tokens.push(token);
                break;
            } else {
                tokens.push(token);
            }
        }
        
        Ok(tokens)
    }
    
    fn next_token(&mut self) -> LexingResult<Token> {
        self.skip_whitespace();
        
        if let Some(comment) = self.lex_comment()? {
            Ok(comment)
        } else if let Some(keyword) = self.lex_keyword()? {
            Ok(keyword)
        } else if let Some(lexeme) = self.lex_lexeme()? {
            Ok(lexeme)
        } else if self.is_eof() {
            let span = SourceSpan {
                file: self.source_id,
                start: self.position.clone(),
                end: self.position.clone()
            };
            Ok(Token::EndOfFile(span))
        } else {
            let span = SourceSpan {
                file: self.source_id,
                start: self.position.clone(),
                end: self.position.clone()
            };
            
            let ch = self.next_char().unwrap();
            Err(LexingError::UnexpectedCharacter(ch, span))
        }
    }
    
    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek() {
            if ch.is_whitespace() {
                self.next_char();
            } else {
                break;
            }
        }
    }
    
    fn lex_comment(&mut self) -> LexingResult<Option<Token>> {
        if let Some(line_comment) = self.lex_line_comment()? {
            Ok(Some(line_comment))
        } else if let Some(block_comment) = self.lex_block_comment()? {
            Ok(Some(block_comment))
        } else {
            Ok(None)
        }
    }
    
    fn lex_line_comment(&mut self) -> LexingResult<Option<Token>> {
        if self.check_match("//", false) {
            let start_position = self.position.clone();
            self.consume_match("//");
            
            let mut content = String::new();
            while let Some(ch) = self.peek() {
                if ch == '\n' {
                    break;
                } else {
                    content.push(ch);
                    self.next_char();
                }
            }
            
            let end_position = self.position.clone();
            let span = SourceSpan {
                file: self.source_id,
                start: start_position,
                end: end_position,
            };
            
            Ok(Some(Token::Comment(content.trim().to_string(), span)))
        } else {
            Ok(None)
        }
    }
    
    fn lex_block_comment(&mut self) -> LexingResult<Option<Token>> {
        if self.check_match("/*", false) {
            let start_position = self.position.clone();
            self.consume_match("/*");
            
            let mut open_delimiters = 1;
            
            let mut content = String::new();
            while !self.is_eof() {
                if self.check_match("*/", false) {
                    open_delimiters -= 1;
                    if open_delimiters == 0 {
                        break;
                    } else {
                        content.push_str("*/");
                        self.consume_match("*/");
                    }
                } else if self.check_match("/*", false) {
                    open_delimiters += 1;
                    content.push_str("/*");
                    self.consume_match("/*");
                } else {
                    content.push(self.next_char().unwrap());
                }
            }
            
            if self.check_match("*/", false) {
                self.consume_match("*/");
            } else {
                let mut end_position = start_position.clone();
                end_position.column += 2;
                return Err(LexingError::UnterminatedBlockComment(SourceSpan {
                    file: self.source_id,
                    start: start_position,
                    end: end_position,
                }));
            }
            
            let end_position = self.position.clone();
            let span = SourceSpan {
                file: self.source_id,
                start: start_position,
                end: end_position,
            };
            
            Ok(Some(Token::Comment(content.trim().to_string(), span)))
        } else {
            Ok(None)
        }
    }
    
    fn lex_keyword(&mut self) -> LexingResult<Option<Token>> {
        for &keyword in KEYWORDS.iter() {
            if self.check_match(keyword, true) {
                let start_position = self.position.clone();
                self.consume_match(keyword);
                let end_position = self.position.clone();
                
                let span = SourceSpan {
                    file: self.source_id,
                    start: start_position,
                    end: end_position,
                };
                
                return Ok(Some(Token::Keyword(keyword.to_string(), span)));
            }
        }
        Ok(None)
    }
    
    fn lex_lexeme(&mut self) -> LexingResult<Option<Token>> {
        let start_position = self.position.clone();
        let mut lexeme = String::new();
        
        while let Some(ch) = self.peek() && !ch.is_whitespace() {
            if self.check_match("//", false) ||
                self.check_match("/*", false)
            {
                break;
            }
            
            lexeme.push(ch);
            self.next_char();
        }
        
        if !lexeme.is_empty() {
            let end_position = self.position.clone();
            let span = SourceSpan {
                file: self.source_id,
                start: start_position,
                end: end_position,
            };
            Ok(Some(Token::Lexeme(lexeme, span)))
        } else {
            Ok(None)
        }
    }
    
    fn next_char(&mut self) -> Option<char> {
        let ch = self.chars.next();
        if let Some(c) = ch {
            if c == '\n' {
                self.position.line += 1;
                self.position.column = 1;
            } else {
                self.position.column += 1;
            }
        }
        ch
    }
    
    fn peek(&mut self) -> Option<char> {
        let mut iter = self.chars.clone();
        iter.next()
    }
    
    fn check_match(&mut self, expected: &str, enforce_boundary: bool) -> bool {
        let mut iter = self.chars.clone();
        for expected_char in expected.chars() {
            if let Some(actual_char) = iter.next() {
                if actual_char != expected_char {
                    return false;
                }
            } else {
                return false;
            }
        }
        
        if enforce_boundary
            && let Some(next_char) = iter.next()
        {
            if next_char.is_whitespace() {
                return true;
            } else if next_char == '/' {
                if let Some(next_next_char) = iter.next() {
                    return next_next_char == '/' || next_next_char == '*';
                } else {
                    return false;
                }
            } else {
                return !next_char.is_alphanumeric() && next_char != '_';
            }
        }
        
        true
    }
    
    fn consume_match(&mut self, expected: &str) {
        for _ in expected.chars() {
            self.next_char();
        }
    }
    
    fn is_eof(&mut self) -> bool {
        self.peek().is_none()
    }
}