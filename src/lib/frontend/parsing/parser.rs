
use crate::includes::{
    frontend::{
        lexing::*,
        parsing::*,
    }
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParserError {
    UnexpectedToken{ expected: String, found: Token },
}

pub type ParserResult<T> = Result<T, ParserError>;