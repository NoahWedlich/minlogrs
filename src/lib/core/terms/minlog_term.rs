
use crate::includes::{
    essential::*,
    utils::*,
    core::{
        types::*,
        terms::*,
    }
};

wrapper_enum::wrapper_enum! {
    pub fwd bnd trait TermBody: PrettyPrintable + Clone + PartialEq + Eq + Hash {
        pub fwd fn minlog_type(&self) -> Rc<MinlogType>
        
        pub fwd fn normalize(&self, eta: bool, pi: bool) -> Rc<MinlogTerm>
        
        pub fwd fn apply_args(&self, _args: &Vec<Rc<MinlogTerm>>) -> Option<Rc<MinlogTerm>> {
            None
        }
        
        pub fwd fn remove_nulls(&self) -> Option<Rc<MinlogTerm>>
        
        pub fwd fn length(&self) -> usize {
            0
        }
        
        pub fwd fn depth(&self) -> usize {
            0
        }
        
        pub fwd fn constructor_pattern(&self) -> bool {
            false
        }
        
        pub fwd fn get_type_variables(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogType>> {
            IndexSet::new()
        }

        pub fwd fn get_algebra_types(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogType>> {
            IndexSet::new()
        }

        pub fwd fn get_free_variables(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogTerm>> {
            IndexSet::new()
        }
        
        pub fwd fn get_bound_variables(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogTerm>> {
            IndexSet::new()
        }
        
        pub fwd fn get_constructors(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogTerm>> {
            IndexSet::new()
        }
        
        pub fwd fn get_program_terms(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogTerm>> {
            IndexSet::new()
        }
        
        pub fwd fn alpha_equivalent(&self, other: &Rc<MinlogTerm>,
            forward: &mut Vec<(TermVariable, TermVariable)>,
            backward: &mut Vec<(TermVariable, TermVariable)>) -> bool
        
        pub fwd fn substitute(&self, from: &TermSubstEntry, to: &TermSubstEntry) -> Rc<MinlogTerm>
        
        pub fwd fn first_conflict_with(&self, other: &Rc<MinlogTerm>) -> Option<(TermSubstEntry, TermSubstEntry)>
        
        pub fwd fn match_with(&self, instance: &Rc<MinlogTerm>) -> MatchOutput<TermSubstEntry>
    }
    
    #[derive(PartialEq, Eq, Hash)]
    pub enum MinlogTerm {
        Wildcard(wildcard: TermWildcard),
        Variable(variable: TermVariable),
        Constructor(constructor: Constructor),
        ProgramTerm(program_term: ProgramTerm),
        Abstraction(abstraction: Abstraction),
        Application(application: Application),
        Tuple(tuple: Tuple),
        Projection(projection: Projection),
        MatchTerm(match_term: MatchTerm),
    }
    
    ext bnd trait PrettyPrintable {
        fwd fn to_pp_element(&self, detail: bool) -> PPElement

        fwd fn requires_parens(&self, detail: bool) -> bool

        fwd fn open_paren(&self) -> String

        fwd fn close_paren(&self) -> String
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