
use crate::includes::{
    frontend::lexing::*,
};

#[derive(Clone, PartialEq, Eq)]
pub enum Command {
    LoadModule{ module: Token },
    CoercionDef { rule_identifier: Token },
    TypeDecl{
        name: Token,
        type_var_tokens: Vec<Vec<Token>>,
        type_def: TypeDefinition
    },
    TermDecl{
        name: Token,
        type_term_var_tokens: Vec<Vec<Token>>,
        type_tokens: Vec<Token>,
        term_def: TermDefinition
    },
    PredDecl{
        name: Token,
        type_term_pred_var_tokens: Vec<Vec<Token>>,
        arity_tokens: Vec<Token>,
        pred_def: PredicateDefinition
    },
    Axiom{
        name: Token,
        proof_goal: ProofGoal
    },
    Theorem{
        name: Token,
        proof_goal: ProofGoal
    },
}

#[derive(Clone, PartialEq, Eq)]
pub enum TypeDefinition {
    TypeVariable,
    Algebra{ constructors: Vec<AlgebraConstructor> },
    TypeTerm{ term_tokens: Vec<Token> },
}

#[derive(Clone, PartialEq, Eq)]
pub struct AlgebraConstructor {
    pub name: Token,
    pub type_tokens: Vec<Token>,
}

#[derive(Clone, PartialEq, Eq)]
pub enum TermDefinition {
    Variable,
    ProgramConstant{ definition: ProgramConstantDefinition },
    Term{ term_tokens: Vec<Token> },
}

#[derive(Clone, PartialEq, Eq)]
pub struct ProgramConstantDefinition {
    pub comp_rules: Vec<(Vec<Token>, Vec<Token>)>,
}

#[derive(Clone, PartialEq, Eq)]
pub enum PredicateDefinition {
    Variable,
    IDP{ definition: IDPDefinition },
    PredTerm{ predicate_tokens: Vec<Token> },
}
  
#[derive(Clone, PartialEq, Eq)]
pub struct IDPDefinition {
    pub clause_terms: Vec<(Token, Vec<Token>)>,
}

#[derive(Clone, PartialEq, Eq)]
pub enum ProofGoal {
    Inline{ predicate_tokens: Vec<Token> },
    Full{
        given: Vec<Command>,
        using: Vec<Command>,
        goal: Vec<Token>,
    }
}