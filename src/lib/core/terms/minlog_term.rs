
use std::{rc::Rc, hash::Hash, collections::HashSet};
use crate::utils::pretty_printer::{PrettyPrintable, PPElement};

use crate::core::substitution::{MatchContext, MatchOutput};

use crate::core::types::minlog_type::MinlogType;

use crate::core::terms::term_wildcard::TermWildcard;
use crate::core::terms::term_variable::TermVariable;
use crate::core::terms::constructor::Constructor;
use crate::core::terms::program_term::ProgramTerm;
use crate::core::terms::abstraction::Abstraction;
use crate::core::terms::application::Application;
use crate::core::terms::tuple::Tuple;
use crate::core::terms::projection::Projection;

use crate::core::terms::term_substitution::TermSubstEntry;

#[derive(PartialEq, Eq, Clone, Hash)]
pub enum Totality {
    Total,
    Partial,
}

impl Totality {
    pub fn is_total(&self) -> bool {
        matches!(self, Totality::Total)
    }
    
    pub fn is_partial(&self) -> bool {
        matches!(self, Totality::Partial)
    }
}

crate::wrapper_enum! {
    
    @default { EmptyTermBody }
    pub trait TermBody: PrettyPrintable, Clone, PartialEq, Eq, Hash {
        pub fn minlog_type(&Self) -> Rc<MinlogType>
        
        pub fn normalize(&Self, eta: bool, pi: bool) -> Rc<MinlogTerm>
        
        pub fn length(&Self) -> usize {
            0
        }
        
        pub fn depth(&Self) -> usize {
            0
        }
        
        pub fn constructor_pattern(&Self) -> bool {
            false
        }
        
        pub fn get_type_variables(&Self, _visited: &mut HashSet<MinlogTerm>) -> HashSet<Rc<MinlogType>> {
            HashSet::new()
        }

        pub fn get_algebra_types(&Self, _visited: &mut HashSet<MinlogTerm>) -> HashSet<Rc<MinlogType>> {
            HashSet::new()
        }

        pub fn get_free_variables(&Self, _visited: &mut HashSet<MinlogTerm>) -> HashSet<Rc<MinlogTerm>> {
            HashSet::new()
        }
        
        pub fn get_bound_variables(&Self, _visited: &mut HashSet<MinlogTerm>) -> HashSet<Rc<MinlogTerm>> {
            HashSet::new()
        }
        
        pub fn get_constructors(&Self, _visited: &mut HashSet<MinlogTerm>) -> HashSet<Rc<MinlogTerm>> {
            HashSet::new()
        }
        
        pub fn get_program_terms(&Self, _visited: &mut HashSet<MinlogTerm>) -> HashSet<Rc<MinlogTerm>> {
            HashSet::new()
        }
        
        pub fn get_internal_constants(&Self, _visited: &mut HashSet<MinlogTerm>) -> HashSet<Rc<MinlogTerm>> {
            HashSet::new()
        }
        
        pub fn alpha_equivalent(&Self, other: &Rc<MinlogTerm>,
            forward: &mut Vec<(TermVariable, TermVariable)>,
            backward: &mut Vec<(TermVariable, TermVariable)>) -> bool

        pub fn totality(&Self, _bound: &mut HashSet<TermVariable>) -> Totality {
            Totality::Partial
        }
        
        pub fn substitute(&Self, from: &TermSubstEntry, to: &TermSubstEntry) -> Rc<MinlogTerm>
        
        pub fn first_conflict_with(&Self, other: &Rc<MinlogTerm>) -> Option<(TermSubstEntry, TermSubstEntry)>
        
        pub fn match_with(&Self, ctx: &mut impl MatchContext<TermSubstEntry>) -> MatchOutput<TermSubstEntry>
    }
    
    #[derive(PartialEq, Eq, Hash)]
    pub enum MinlogTerm {
        Wildcard(|wildcard| TermWildcard),
        Variable(||variable|| TermVariable),
        Constructor(||constructor|| Constructor),
        ProgramTerm(||program_term|| ProgramTerm),
        InternalConstant(||internal_constant||),
        Abstraction(||abstraction|| Abstraction),
        Application(||application|| Application),
        Tuple(||tuple|| Tuple),
        Projection(||projection|| Projection),
        Conditional(||conditional||),
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
        var.is_variable() && self.get_free_variables(&mut HashSet::new()).contains(var)
    }
    
    pub fn contains_bound_variable(&self, var: &Rc<MinlogTerm>) -> bool {
        var.is_variable() && self.get_bound_variables(&mut HashSet::new()).contains(var)
    }
    
    pub fn contains_constructor(&self, con: &Rc<MinlogTerm>) -> bool {
        con.is_constructor() && self.get_constructors(&mut HashSet::new()).contains(con)
    }
    
    pub fn contains_program_term(&self, prog: &Rc<MinlogTerm>) -> bool {
        prog.is_program_term() && self.get_program_terms(&mut HashSet::new()).contains(prog)
    }
    
    pub fn contains_internal_constant(&self, ic: &Rc<MinlogTerm>) -> bool {
        ic.is_internal_constant() && self.get_internal_constants(&mut HashSet::new()).contains(ic)
    }
}

#[derive(PartialEq, Eq, Clone, Hash)]
pub struct EmptyTermBody;

impl PrettyPrintable for EmptyTermBody {
    fn to_pp_element(&self, _detail: bool) -> PPElement {
        PPElement::text("<?>".to_string())
    }
}

impl TermBody for EmptyTermBody {
    fn minlog_type(&self) -> Rc<MinlogType> {
        unimplemented!()
    }
    
    fn normalize(&self, _eta: bool, _pi: bool) -> Rc<MinlogTerm> {
        unimplemented!()
    }
    
    fn alpha_equivalent(&self, _other: &Rc<MinlogTerm>,
        _forward: &mut Vec<(TermVariable, TermVariable)>,
        _backward: &mut Vec<(TermVariable, TermVariable)>) -> bool {
            unimplemented!()
    }
    
    fn substitute(&self, _from: &TermSubstEntry, _to: &TermSubstEntry) -> Rc<MinlogTerm> {
        unimplemented!()
    }

    fn first_conflict_with(&self, _other: &Rc<MinlogTerm>) -> Option<(TermSubstEntry, TermSubstEntry)> {
        unimplemented!()
    }
    
    fn match_with(&self, _ctx: &mut impl MatchContext<TermSubstEntry>) -> MatchOutput<TermSubstEntry> {
            unimplemented!()
    }
}