
use crate::includes::{
    essential::*,
    utils::*,
    core::{
        types::*,
        terms::*,
    }
};

crate::wrapper_enum! {
    pub trait TermBody: PrettyPrintable, Clone, PartialEq, Eq, Hash {
        pub fn minlog_type(&Self) -> Rc<MinlogType>
        
        pub fn normalize(&Self, eta: bool, pi: bool) -> Rc<MinlogTerm>
        
        pub fn remove_nulls(&Self) -> Option<Rc<MinlogTerm>>
        
        pub fn length(&Self) -> usize {
            0
        }
        
        pub fn depth(&Self) -> usize {
            0
        }
        
        pub fn constructor_pattern(&Self) -> bool {
            false
        }
        
        pub fn get_type_variables(&Self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogType>> {
            IndexSet::new()
        }

        pub fn get_algebra_types(&Self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogType>> {
            IndexSet::new()
        }

        pub fn get_free_variables(&Self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogTerm>> {
            IndexSet::new()
        }
        
        pub fn get_bound_variables(&Self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogTerm>> {
            IndexSet::new()
        }
        
        pub fn get_constructors(&Self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogTerm>> {
            IndexSet::new()
        }
        
        pub fn get_program_terms(&Self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogTerm>> {
            IndexSet::new()
        }
        
        pub fn get_internal_constants(&Self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogTerm>> {
            IndexSet::new()
        }
        
        pub fn alpha_equivalent(&Self, other: &Rc<MinlogTerm>,
            forward: &mut Vec<(TermVariable, TermVariable)>,
            backward: &mut Vec<(TermVariable, TermVariable)>) -> bool
        
        pub fn substitute(&Self, from: &TermSubstEntry, to: &TermSubstEntry) -> Rc<MinlogTerm>
        
        pub fn first_conflict_with(&Self, other: &Rc<MinlogTerm>) -> Option<(TermSubstEntry, TermSubstEntry)>
        
        pub fn match_with(&Self, instance: &Rc<MinlogTerm>) -> MatchOutput<TermSubstEntry>
    }
    
    #[derive(PartialEq, Eq, Hash)]
    pub enum MinlogTerm {
        Wildcard(|wildcard| TermWildcard),
        Variable(||variable|| TermVariable),
        Constructor(||constructor|| Constructor),
        ProgramTerm(||program_term|| ProgramTerm),
        Abstraction(||abstraction|| Abstraction),
        Application(||application|| Application),
        Tuple(||tuple|| Tuple),
        Projection(||projection|| Projection),
        MatchTerm(||match_term|| MatchTerm),
    }
    
    impl PrettyPrintable {
        fn to_pp_element(&Self, detail: bool) -> PPElement;

        fn requires_parens(&Self, detail: bool) -> bool;

        fn open_paren(&Self) -> String;

        fn close_paren(&Self) -> String;
    }
}

impl MinlogTerm {
    pub fn contains_type_variable(&self, var: &Rc<MinlogType>) -> bool {
        self.minlog_type().contains_type_variable(var)
    }

    pub fn contains_algebra_type(&self, var: &Rc<MinlogType>) -> bool {
        self.minlog_type().contains_algebra_type(var)
    }

    pub fn contains_free_variable(&self, var: &Rc<MinlogTerm>) -> bool {
        var.is_variable() && self.get_free_variables(&mut IndexSet::new()).contains(var)
    }
    
    pub fn contains_bound_variable(&self, var: &Rc<MinlogTerm>) -> bool {
        var.is_variable() && self.get_bound_variables(&mut IndexSet::new()).contains(var)
    }
    
    pub fn contains_constructor(&self, con: &Rc<MinlogTerm>) -> bool {
        con.is_constructor() && self.get_constructors(&mut IndexSet::new()).contains(con)
    }
    
    pub fn contains_program_term(&self, prog: &Rc<MinlogTerm>) -> bool {
        prog.is_program_term() && self.get_program_terms(&mut IndexSet::new()).contains(prog)
    }
}