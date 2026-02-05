
use crate::{
    includes::frontend::{
        lexing::*,
        parsing::*,
    }
};

#[derive(Clone, PartialEq, Eq)]
pub enum ParserError {
    UnexpectedToken{ expected: String, found: Token },
    UnexpectedEndOfInput{ reason: String },
    InvalidCharPosition{ reason: String },
}

pub type ParserResult<T> = Result<T, ParserError>;

pub struct Parser {
    pub tokens: Vec<Token>,
    token_pos: usize,
    char_pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, token_pos: 0, char_pos: 0 }
    }
    
    pub fn parse(&mut self) -> ParserResult<Vec<Command>> {
        let mut commands = Vec::new();
        
        while !self.at_end() {
            let command = self.parse_command()?;
            commands.push(command);
        }
        
        Ok(commands)
    }
    
    fn parse_command(&mut self) -> ParserResult<Command> {
        if let Some(command) = self.parse_module_load()? {
            Ok(command)
        } else if let Some(command) = self.parse_coercion_def()? {
            Ok(command)
        } else if let Some(command) = self.parse_type_decl()? {
            Ok(command)
        } else if let Some(command) = self.parse_term_decl()? {
            Ok(command)
        } else if let Some(command) = self.parse_pred_decl()? {
            Ok(command)
        } else if let Some(command) = self.parse_axiom()? {
            Ok(command)
        } else if let Some(command) = self.parse_theorem()? {
            Ok(command)
        } else {
            Err(ParserError::UnexpectedToken{
                expected: "command".to_string(),
                found: self.peek()?.clone(),
            })
        }
    }
    
    fn parse_module_load(&mut self) -> ParserResult<Option<Command>> {
        if !self.match_keyword("load_module")? {
            return Ok(None);
        }
        
        let name = self.expect_identifier()?;
        
        Ok(Some(Command::LoadModule{ module: name }))
    }
    
    fn parse_coercion_def(&mut self) -> ParserResult<Option<Command>> {
        if !self.match_keyword("coercion")? {
            return Ok(None);
        }
        
        let rule_identifier = self.expect_identifier()?;
        
        Ok(Some(Command::CoercionDef { rule_identifier }))
    }
    
    fn parse_type_decl(&mut self) -> ParserResult<Option<Command>> {
        if !self.match_keyword("type")? {
            return Ok(None);
        }
        
        let name = self.expect_identifier_until(&["<", "="])?;
        let type_var_tokens = self.parse_turbofish()?;
        let type_def = self.parse_type_definition()?;
        
        Ok(Some(Command::TypeDecl{
            name,
            type_var_tokens,
            type_def,
        }))
    }
    
    fn parse_type_definition(&mut self) -> ParserResult<TypeDefinition> {
        if !self.match_lexeme("=")? {
            return Ok(TypeDefinition::TypeVariable);
        }
        
        if let Some(alg_def) = self.parse_algebra_definition()? {
            Ok(alg_def)
        } else {
            let term_tokens = self.collect_until(&[&Terminator::AnyKeyword])?;
            Ok(TypeDefinition::TypeTerm{ term_tokens })
        }
    }
    
    fn parse_algebra_definition(&mut self) -> ParserResult<Option<TypeDefinition>> {
        if !self.match_keyword("algebra")? {
            return Ok(None);
        }
        
        self.expect_lexeme("{")?;
        
        let mut constructors = Vec::new();
        
        while !self.match_lexeme("}")? {
            let constructor = self.parse_algebra_constructor()?;
            constructors.push(constructor);
        }
        
        Ok(Some(TypeDefinition::Algebra{ constructors }))
    }
    
    fn parse_algebra_constructor(&mut self) -> ParserResult<AlgebraConstructor> {
        let name = self.expect_identifier()?;
        
        let type_tokens = self.collect_until(&[&Terminator::Lexeme(","), &Terminator::Lexeme("}")])?;
        
        self.match_lexeme(",")?;
        
        Ok(AlgebraConstructor{ name, type_tokens })
    }
    
    fn parse_term_decl(&mut self) -> ParserResult<Option<Command>> {
        if !self.match_keyword("term")? {
            return Ok(None);
        }
        
        let name = self.expect_identifier_until(&["<", ":", "="])?;
        let type_term_var_tokens = self.parse_turbofish()?;
        
        let type_tokens = if self.match_lexeme(":")? {
            self.collect_until(&[&Terminator::Lexeme("="), &Terminator::AnyKeyword])?
        } else {
            Vec::new()
        };
        
        let term_def = self.parse_term_definition()?;
        
        Ok(Some(Command::TermDecl{
            name,
            type_term_var_tokens,
            type_tokens,
            term_def,
        }))
    }
    
    fn parse_term_definition(&mut self) -> ParserResult<TermDefinition> {
        if !self.match_lexeme("=")? {
            return Ok(TermDefinition::Variable);
        }
        
        if let Some(pc_def) = self.parse_program_constant_definition()? {
            Ok(TermDefinition::ProgramConstant{ definition: pc_def })
        } else {
            let term_tokens = self.collect_until(&[&Terminator::AnyKeyword])?;
            Ok(TermDefinition::Term{ term_tokens })
        }
    }
    
    fn parse_program_constant_definition(&mut self) -> ParserResult<Option<ProgramConstantDefinition>> {
        if !self.match_keyword("program_constant")? {
            return Ok(None);
        }
        
        self.expect_lexeme("{")?;
        
        let mut comp_rules = Vec::new();
        
        while !self.match_lexeme("}")? {
            let rule = self.parse_computation_rule()?;
            comp_rules.push(rule);
        }
        
        Ok(Some(ProgramConstantDefinition{ comp_rules }))
    }
    
    fn parse_computation_rule(&mut self) -> ParserResult<(Vec<Token>, Vec<Token>)> {
        let pattern = self.collect_until(&[&Terminator::Lexeme("=>")])?;
        
        self.expect_lexeme("=>")?;
        
        let result = self.collect_until(&[&Terminator::Lexeme(","), &Terminator::Lexeme("}")])?;
        
        self.match_lexeme(",")?;
        
        Ok((pattern, result))
    }
    
    fn parse_pred_decl(&mut self) -> ParserResult<Option<Command>> {
        if !self.match_keyword("pred")? {
            return Ok(None);
        }
        
        let name = self.expect_identifier_until(&["<", ":", "="])?;
        let type_term_pred_var_tokens = self.parse_turbofish()?;
        
        let arity_tokens = if self.match_lexeme(":")? {
            self.collect_until(&[&Terminator::Lexeme("="), &Terminator::AnyKeyword])?
        } else {
            Vec::new()
        };
        
        let pred_def = self.parse_predicate_definition()?;
        
        Ok(Some(Command::PredDecl{
            name,
            type_term_pred_var_tokens,
            arity_tokens,
            pred_def,
        }))
    }
    
    fn parse_predicate_definition(&mut self) -> ParserResult<PredicateDefinition> {
        if !self.match_lexeme("=")? {
            return Ok(PredicateDefinition::Variable);
        }
        
        if let Some(idp_def) = self.parse_idp_definition()? {
            Ok(PredicateDefinition::IDP{ definition: idp_def })
        } else {
            let predicate_tokens = self.collect_until(&[&Terminator::AnyKeyword])?;
            Ok(PredicateDefinition::PredTerm{ predicate_tokens })
        }
    }
    
    fn parse_idp_definition(&mut self) -> ParserResult<Option<IDPDefinition>> {
        if !self.match_keyword("idp")? {
            return Ok(None);
        }
        
        self.expect_lexeme("{")?;
        
        let mut clause_terms = Vec::new();
        
        while !self.match_lexeme("}")? {
            let clause = self.parse_idp_clause()?;
            clause_terms.push(clause);
        }
        
        Ok(Some(IDPDefinition{ clause_terms }))
    }
    
    fn parse_idp_clause(&mut self) -> ParserResult<(Token, Vec<Token>)> {
        let name = self.expect_identifier_until(&[":"])?;
        
        self.expect_lexeme(":")?;
        
        let predicate_tokens = self.collect_until(&[&Terminator::Lexeme(","), &Terminator::Lexeme("}")])?;
        
        self.match_lexeme(",")?;
        
        Ok((name, predicate_tokens))
    }
    
    fn parse_turbofish(&mut self) -> ParserResult<Vec<Vec<Token>>> {
        if !self.match_lexeme("<")? {
            return Ok(Vec::new());
        }
        
        let mut var_tokens_list = Vec::new();
        
        loop {
            let var_tokens = self.collect_until(&[&Terminator::Lexeme(","), &Terminator::Lexeme(">")])?;
            var_tokens_list.push(var_tokens);
            
            if self.match_lexeme(",")? {
                continue;
            } else {
                break;
            }
        }
        
        self.expect_lexeme(">")?;
        
        Ok(var_tokens_list)
    }
    
    fn parse_axiom(&mut self) -> ParserResult<Option<Command>> {
        if !self.match_keyword("axiom")? {
            return Ok(None);
        }
        
        let name = self.expect_identifier_until(&[":", "{"])?;
        let proof_goal = self.parse_proof_goal()?;
        
        Ok(Some(Command::Axiom{
            name,
            proof_goal,
        }))
    }
    
    fn parse_theorem(&mut self) -> ParserResult<Option<Command>> {
        if !self.match_keyword("theorem")? {
            return Ok(None);
        }
        
        let name = self.expect_identifier_until(&[":", "{"])?;
        let proof_goal = self.parse_proof_goal()?;
        
        Ok(Some(Command::Theorem{
            name,
            proof_goal,
        }))
    }
    
    fn parse_proof_goal(&mut self) -> ParserResult<ProofGoal> {
        if let Some(inline_goal) = self.parse_inline_proof_goal()? {
            Ok(inline_goal)
        } else if let Some(full_goal) = self.parse_full_proof_goal()? {
            Ok(full_goal)
        } else {
            Err(ParserError::UnexpectedToken{
                expected: "proof goal".to_string(),
                found: self.peek()?.clone(),
            })
        }
    }
    
    fn parse_inline_proof_goal(&mut self) -> ParserResult<Option<ProofGoal>> {
        if !self.match_lexeme(":")? {
            return Ok(None);
        }
        
        let predicate_tokens = self.collect_until(&[&Terminator::AnyKeyword])?;
        
        Ok(Some(ProofGoal::Inline{ predicate_tokens }))
    }
    
    fn parse_full_proof_goal(&mut self) -> ParserResult<Option<ProofGoal>> {
        if !self.match_lexeme("{")? {
            return Ok(None);
        }
        
        let given = self.parse_given_commands()?;
        let using = self.parse_using_commands()?;
        let goal = self.parse_goal_tokens()?;
        
        self.expect_lexeme("}")?;
        
        Ok(Some(ProofGoal::Full{
            given,
            using,
            goal,
        }))
    }
    
    fn parse_given_commands(&mut self) -> ParserResult<Vec<Command>> {
        self.expect_keyword("given")?;
        self.expect_lexeme(":")?;
        
        let mut commands = Vec::new();
        
        loop {
            let token = self.peek()?;
            if token.matches_keyword("using") || token.matches_keyword("then") {
                break;
            }
            
            let command = self.parse_command()?;
            commands.push(command);
        }
        
        Ok(commands)
    }
    
    fn parse_using_commands(&mut self) -> ParserResult<Vec<Command>> {
        if !self.match_keyword("using")? || !self.match_lexeme(":")? {
            return Ok(Vec::new());
        }
        
        let mut commands = Vec::new();
        
        loop {
            let token = self.peek()?;
            if token.matches_keyword("then") {
                break;
            }
            
            let command = self.parse_command()?;
            commands.push(command);
        }
        
        Ok(commands)
    }
    
    fn parse_goal_tokens(&mut self) -> ParserResult<Vec<Token>> {
        self.expect_keyword("then")?;
        self.expect_lexeme(":")?;
        
        let goal_tokens = self.collect_until(&[&Terminator::Lexeme("}")])?;
        
        Ok(goal_tokens)
    }
    
    fn peek(&mut self) -> ParserResult<Token> {
        if self.at_end() {
            return Err(ParserError::UnexpectedEndOfInput{ reason: "Attempted to peek beyond end of input".to_string() });
        }
        
        let current_token = self.tokens.get(self.token_pos).unwrap();
        
        match current_token {
            Token::Lexeme(text, span) => {
                if self.char_pos >= text.len() {
                    Err(ParserError::InvalidCharPosition{ reason: "Character position exceeds token length".to_string() })
                } else if self.char_pos > 0 {
                    let substr = &text[self.char_pos..];
                    let new_span = SourceSpan {
                        file: span.file,
                        start: SourcePosition {
                            line: span.start.line,
                            column: span.start.column + self.char_pos,
                        },
                        end: span.end.clone(),
                    };
                    Ok(Token::Lexeme(substr.to_string(), new_span))
                } else if text.is_empty() {
                    self.advance_token()?;
                    self.peek()
                } else {
                    Ok(current_token.clone())
                }
            },
            _ if self.char_pos != 0 => {
                Err(ParserError::InvalidCharPosition{ reason: "Character position non-zero for non-lexeme token".to_string() })
            },
            _ => {
                Ok(current_token.clone())
            }
        }
    }
    
    fn advance_token(&mut self) -> ParserResult<Token> {
        if self.at_end() {
            return Err(ParserError::UnexpectedEndOfInput{ reason: "Attempted to advance beyond end of input".to_string() });
        }
        
        let token = self.peek()?;
        
        self.token_pos += 1;
        self.char_pos = 0;
        
        Ok(token)
    }
    
    fn advance_chars(&mut self, count: usize) -> ParserResult<()> {
        let current_token = self.peek()?;
        
        if let Token::Lexeme(text, _) = &current_token {
            if count > text.len() {
                Err(ParserError::InvalidCharPosition{ reason: "Advancing beyond token length".to_string() })
            } else if count == text.len() {
                self.token_pos += 1;
                self.char_pos = 0;
                Ok(())
            } else {
                self.char_pos += count;
                Ok(())
            }
        } else if self.char_pos + count == 1 {
            self.token_pos += 1;
            self.char_pos = 0;
            Ok(())
        } else {
            Err(ParserError::InvalidCharPosition{ reason: "Advancing beyond single non-lexeme token".to_string() })
        }
    }
    
    fn at_end(&self) -> bool {
        if self.token_pos >= self.tokens.len() {
            true
        } else {
            self.tokens[self.token_pos].is_end_of_file()
        }
    }
    
    fn expect_keyword(&mut self, expected: &str) -> ParserResult<()> {
        if !self.match_keyword(expected)? {
            let token = self.peek()?;
            Err(ParserError::UnexpectedToken{
                expected: format!("keyword '{}'", expected),
                found: token,
            })
        } else {
            Ok(())
        }
    }
    
    fn expect_lexeme(&mut self, expected: &str) -> ParserResult<()> {
        if !self.match_lexeme(expected)? {
            let token = self.peek()?;
            Err(ParserError::UnexpectedToken{
                expected: format!("lexeme '{}'", expected),
                found: token,
            })
        } else {
            Ok(())
        }
    }
    
    fn expect_identifier(&mut self) -> ParserResult<Token> {
        if let Some(identifier) = self.match_identifier()? {
            Ok(identifier)
        } else {
            let token = self.peek()?;
            Err(ParserError::UnexpectedToken{
                expected: "identifier".to_string(),
                found: token,
            })
        }
    }
    
    fn expect_identifier_until(&mut self, terminators: &[&str]) -> ParserResult<Token> {
        if let Some(identifier) = self.match_identifier_until(terminators)? {
            Ok(identifier)
        } else {
            let token = self.peek()?;
            Err(ParserError::UnexpectedToken{
                expected: "identifier".to_string(),
                found: token,
            })
        }
    }
    
    fn match_keyword(&mut self, expected: &str) -> ParserResult<bool> {
        let token = self.peek()?;
        if token.matches_keyword(expected) {
            self.advance_token()?;
            Ok(true)
        } else {
            Ok(false)
        }
    }
    
    fn match_lexeme(&mut self, expected: &str) -> ParserResult<bool> {
        let token = self.peek()?;
        if let Token::Lexeme(content, _) = &token {
            if content.starts_with(expected) {
                self.advance_chars(expected.len())?;
                Ok(true)
            } else {
                Ok(false)
            }
        } else {
            Ok(false)
        }
    }
    
    fn match_identifier(&mut self) -> ParserResult<Option<Token>> {
        let token = self.peek()?;
        if token.is_lexeme() {
            self.advance_token()?;
            Ok(Some(token))
        } else {
            Ok(None)
        }
    }
    
    fn match_identifier_until(&mut self, terminators: &[&str]) -> ParserResult<Option<Token>> {
        let token = self.peek()?;
        
        if let Token::Lexeme(content, span) = &token {
            let mut pos = None;
            for terminator in terminators {
                if let Some(t_pos) = content.find(terminator) {
                    if let Some(current_pos) = pos {
                        if t_pos < current_pos {
                            pos = Some(t_pos);
                        }
                    } else {
                        pos = Some(t_pos);
                    }
                }
            }
            
            if let Some(pos) = pos {
                let identifier_str = &content[..pos];
                let identifier_span = SourceSpan {
                    file: span.file,
                    start: span.start.clone(),
                    end: SourcePosition {
                        line: span.start.line,
                        column: span.start.column + pos,
                    },
                };
                
                let identifier_token = Token::Lexeme(identifier_str.to_string(), identifier_span);
                
                self.advance_chars(pos)?;
                Ok(Some(identifier_token))
            } else {
                self.advance_token()?;
                Ok(Some(token))
            }
        } else {
            Ok(None)
        }
    }
    
    fn collect_until(&mut self, terminators: &[&Terminator]) -> ParserResult<Vec<Token>> {
        let mut collected_tokens = Vec::new();
        let lexeme_terminators = terminators.iter().filter_map(|t| {
            if let Terminator::Lexeme(s) = t {
                Some(*s)
            } else {
                None
            }
        }).collect::<Vec<&str>>();
        
        while !self.at_end() {
            let token = self.peek()?;
            
            match token {
                Token::Lexeme(_, _) => {
                    let ident_until_terminator = self.expect_identifier_until(&lexeme_terminators)?;
                    
                    if token != ident_until_terminator {
                        if !ident_until_terminator.matches_lexeme("") {
                            collected_tokens.push(ident_until_terminator);
                        }
                        break;
                    } else {
                        collected_tokens.push(token);
                    }
                },
                Token::Keyword(_, _) => {
                    if terminators.iter().any(|t| matches!(t, Terminator::AnyKeyword)) {
                        break;
                    } else {
                        collected_tokens.push(self.advance_token()?);
                    }
                },
                _ => break
            }
        }
        
        Ok(collected_tokens)
    }
}

enum Terminator<'a> {
    Lexeme(&'a str),
    AnyKeyword,
}