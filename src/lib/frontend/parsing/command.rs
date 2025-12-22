
use crate::includes::{
    core::terms::*,
    frontend::lexing::*,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Command {
    LoadModule{ module: Token },
    TypeDecl{ names: Vec<Token>, type_def: TypeDefinition },
    TermDecl{ names: Vec<Token>, type_term: MinlogTerm, term_def: TermDefinition },
    PredDecl{ names: Vec<Token>, arity_term: MinlogTerm, pred_def: PredicateDefinition },
    Axiom{ name: Token, predicate: MinlogTerm},
    Theorem{ name: Token, predicate: MinlogTerm},
    Proof{ theorem_name: Token, body: ProofBody },
}

// TODO: Make TypeVariables explicit
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeDefinition {
    TypeVariable,
    Algebra{ constructors: Vec<AlgebraConstructor> },
    TypeTerm{ term: MinlogTerm },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AlgebraConstructor {
    pub name: Token,
    pub type_term: MinlogTerm,
}

// TODO: Make Type/TermVariables explicit
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TermDefinition {
    Variable,
    ProgramConstant{ definition: ProgramConstantDefinition },
    Term{ term: MinlogTerm },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProgramConstantDefinition {
    pub type_term: MinlogTerm,
    pub comp_rules: Vec<(MinlogTerm, MinlogTerm)>,
}

// TODO: Make Type/Term/PredVariables explicit
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PredicateDefinition {
    Variable,
    IDP{ definition: IDPDefinition },
    PredTerm{ term: MinlogTerm },
}
  
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IDPDefinition {
    pub arity_term: MinlogTerm,
    pub clause_terms: Vec<(Token, MinlogTerm)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProofBody {
    pub steps: Vec<ProofStep>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProofStep {
    
}