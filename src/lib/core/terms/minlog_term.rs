
use indexmap::IndexSet;
use std::{rc::Rc, hash::Hash};
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
use crate::core::terms::match_term::MatchTerm;

use crate::core::terms::term_substitution::TermSubstEntry;

crate::wrapper_enum! {
    
    @default { EmptyTermBody }
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
    
    pub fn contains_internal_constant(&self, ic: &Rc<MinlogTerm>) -> bool {
        ic.is_internal_constant() && self.get_internal_constants(&mut IndexSet::new()).contains(ic)
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
    
    fn remove_nulls(&self) -> Option<Rc<MinlogTerm>> {
        None
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