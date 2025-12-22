
use crate::includes::frontend::{
    lexing::*,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Keyword(String, SourceSpan),
    Lexeme(String, SourceSpan),
    Comment(String, SourceSpan),
    EndOfFile(SourceSpan),
}

impl Token {
    pub fn get_span(&self) -> &SourceSpan {
        match self {
            Token::Keyword(_, span) => span,
            Token::Lexeme(_, span) => span,
            Token::Comment(_, span) => span,
            Token::EndOfFile(span) => span,
        }
    }
    
    pub fn into_span(self) -> SourceSpan {
        match self {
            Token::Keyword(_, span) => span,
            Token::Lexeme(_, span) => span,
            Token::Comment(_, span) => span,
            Token::EndOfFile(span) => span,
        }
    }
    
    pub fn is_identifier(&self) -> bool {
        match self {
            Token::Lexeme(content, _) => {
                content.chars().all(|c| c.is_alphanumeric() || c == '_')
            }
            _ => false,
        }
    }
    
    pub fn is_keyword(&self) -> bool {
        matches!(self, Token::Keyword(_, _))
    }
    
    pub fn is_lexeme(&self) -> bool {
        matches!(self, Token::Lexeme(_, _))
    }
    
    pub fn is_comment(&self) -> bool {
        matches!(self, Token::Comment(_, _))
    }
    
    pub fn is_end_of_file(&self) -> bool {
        matches!(self, Token::EndOfFile(_))
    }
    
    pub fn to_identifier(&self) -> Option<&String> {
        if let Token::Lexeme(content, _) = self {
            if content.chars().all(|c| c.is_alphanumeric() || c == '_') {
                Some(content)
            } else {
                None
            }
        } else {
            None
        }
    }
    
    pub fn to_keyword(&self) -> Option<&String> {
        if let Token::Keyword(name, _) = self {
            Some(name)
        } else {
            None
        }
    }
    
    pub fn to_lexeme(&self) -> Option<&String> {
        if let Token::Lexeme(lexeme, _) = self {
            Some(lexeme)
        } else {
            None
        }
    }
    
    pub fn to_comment(&self) -> Option<&String> {
        if let Token::Comment(content, _) = self {
            Some(content)
        } else {
            None
        }
    }
}