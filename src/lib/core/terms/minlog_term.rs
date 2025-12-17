
use crate::includes::{
    essential::*,
    utils::*,
    core::{
        types::*,
        terms::*,
    }
};

wrapper_enum::wrapper_enum! {
    pub fwd bnd trait TermBody: PrettyPrintable {
        pub fwd fn minlog_type(&self) -> Rc<MinlogType>
        
        pub fwd fn normalize(&self, eta: bool, pi: bool) -> MinlogTerm
        
        pub fwd fn apply_args(&self, _args: &Vec<MinlogTerm>) -> Option<MinlogTerm> {
            None
        }
        
        pub fwd fn remove_nulls(&self) -> Option<MinlogTerm>
        
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

        pub fwd fn get_free_variables(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
            IndexSet::new()
        }
        
        pub fwd fn get_bound_variables(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
            IndexSet::new()
        }
        
        pub fwd fn get_constructors(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
            IndexSet::new()
        }
        
        pub fwd fn get_program_terms(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
            IndexSet::new()
        }
        
        pub fwd fn alpha_equivalent(&self, other: &MinlogTerm,
            forward: &mut Vec<(TermVariable, TermVariable)>,
            backward: &mut Vec<(TermVariable, TermVariable)>) -> bool
        
        pub fwd fn substitute(&self, from: &TermSubstEntry, to: &TermSubstEntry) -> MinlogTerm
        
        pub fwd fn first_conflict_with(&self, other: &MinlogTerm) -> Option<(TermSubstEntry, TermSubstEntry)>
        
        pub fwd fn match_with(&self, instance: &MinlogTerm) -> MatchOutput<TermSubstEntry>
    }
    
    #[derive(Clone, PartialEq, Eq, Hash)]
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

    pub fn contains_free_variable(&self, var: &MinlogTerm) -> bool {
        var.is_variable() && self.get_free_variables(&mut IndexSet::new()).contains(var)
    }
    
    pub fn contains_bound_variable(&self, var: &MinlogTerm) -> bool {
        var.is_variable() && self.get_bound_variables(&mut IndexSet::new()).contains(var)
    }
    
    pub fn contains_constructor(&self, con: &MinlogTerm) -> bool {
        con.is_constructor() && self.get_constructors(&mut IndexSet::new()).contains(con)
    }
    
    pub fn contains_program_term(&self, prog: &MinlogTerm) -> bool {
        prog.is_program_term() && self.get_program_terms(&mut IndexSet::new()).contains(prog)
    }
}

pub trait NativeTermBody: TermBody + Any {
    fn minlog_to_native(term: &MinlogTerm) -> Option<Rc<Self>> where Self: Sized;
    
    fn native_to_minlog(&self) -> MinlogTerm;
    
    fn eq(&self, other: &dyn NativeTermBody) -> bool {
        self.native_to_minlog() == other.native_to_minlog()
    }
}

impl PartialEq for dyn NativeTermBody {
    fn eq(&self, other: &Self) -> bool {
        self.eq(other)
    }
}

impl Eq for dyn NativeTermBody {}