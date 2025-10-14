
use std::rc::Rc;
use crate::utils::pretty_printer::{PrettyPrintable, PPElement};

use crate::core::substitution::MatchContext;

use crate::core::types::minlog_type::MinlogType;

use crate::core::terms::term_variable::TermVariable;
use crate::core::terms::abstraction::Abstraction;

use crate::core::terms::term_substitution::TermSubstEntry;

#[derive(PartialEq, Eq, Clone)]
pub enum Totality {
    Total,
    Partial,
}

crate::wrapper_enum! {
    
    @default { EmptyTermBody }
    pub trait TermBody: PrettyPrintable, Clone, PartialEq, Eq {
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
        
        pub fn inner_free_variables(&Self) -> Vec<Rc<MinlogTerm>> {
            vec![]
        }
        
        pub fn inner_bound_variables(&Self) -> Vec<Rc<MinlogTerm>> {
            vec![]
        }
        
        pub fn inner_constructors(&Self) -> Vec<Rc<MinlogTerm>> {
            vec![]
        }
        
        pub fn inner_program_terms(&Self) -> Vec<Rc<MinlogTerm>> {
            vec![]
        }
        
        pub fn inner_internal_constants(&Self) -> Vec<Rc<MinlogTerm>> {
            vec![]
        }
        
        pub fn alpha_equivalent(&Self, other: &Rc<MinlogTerm>,
            forward: &mut Vec<(TermVariable, TermVariable)>,
            backward: &mut Vec<(TermVariable, TermVariable)>) -> bool

        pub fn totality(&Self, _bound: &mut Vec<TermVariable>) -> Totality {
            Totality::Partial
        }
        
        pub fn substitute(&Self, from: &TermSubstEntry, to: &TermSubstEntry) -> Rc<MinlogTerm>
        
        pub fn first_conflict_with(&Self, other: &Rc<MinlogTerm>) -> Option<(Rc<MinlogTerm>, Rc<MinlogTerm>)>
        
        pub fn match_with(&Self, ctx: &mut impl MatchContext<TermSubstEntry>)
            -> Result<Option<(TermSubstEntry, TermSubstEntry)>, ()>
    }
    
    #[derive(PartialEq, Eq)]
    pub enum MinlogTerm {
        Variable(||variable|| TermVariable),
        Constructor(||constructor||),
        ProgramTerm(||program_term||),
        InternalConstant(||internal_constant||),
        Abstraction(||abstraction|| Abstraction),
        Application(||application||),
        Pairing(||pairing||),
        Projection(||projection||),
        Conditional(||conditional||),
    }
    
    impl PrettyPrintable {
        fn to_pp_element(&Self, detail: bool) -> PPElement;

        fn requires_parens(&Self, _detail: bool) -> bool;

        fn open_paren(&Self) -> String;

        fn close_paren(&Self) -> String;
    }
}

impl MinlogTerm {
    pub fn get_free_variables(term: &Rc<MinlogTerm>) -> Vec<Rc<MinlogTerm>> {
        let mut inner = term.inner_free_variables();
        
        if term.is_variable() {
            inner.push(term.clone());
        }
        
        inner
    }
    
    pub fn contains_free_variable(term: &Rc<MinlogTerm>, var: &Rc<MinlogTerm>) -> bool {
        var.is_variable() && MinlogTerm::get_free_variables(term).contains(var)
    }
    
    pub fn get_bound_variables(term: &Rc<MinlogTerm>) -> Vec<Rc<MinlogTerm>> {
        let mut inner = term.inner_bound_variables();
        
        if term.is_variable() {
            inner.push(term.clone());
        }
        
        inner
    }
    
    pub fn contains_bound_variable(term: &Rc<MinlogTerm>, var: &Rc<MinlogTerm>) -> bool {
        var.is_variable() && MinlogTerm::get_bound_variables(term).contains(var)
    }
    
    pub fn get_constructors(term: &Rc<MinlogTerm>) -> Vec<Rc<MinlogTerm>> {
        let mut inner = term.inner_constructors();
        
        if term.is_constructor() {
            inner.push(term.clone());
        }
        
        inner
    }
    
    pub fn contains_constructor(term: &Rc<MinlogTerm>, con: &Rc<MinlogTerm>) -> bool {
        con.is_constructor() && MinlogTerm::get_constructors(term).contains(con)
    }
    
    pub fn get_program_terms(term: &Rc<MinlogTerm>) -> Vec<Rc<MinlogTerm>> {
        let mut inner = term.inner_program_terms();
        
        if term.is_program_term() {
            inner.push(term.clone());
        }
        
        inner
    }
    
    pub fn contains_program_term(term: &Rc<MinlogTerm>, prog: &Rc<MinlogTerm>) -> bool {
        prog.is_program_term() && MinlogTerm::get_program_terms(term).contains(prog)
    }
    
    pub fn get_internal_constants(term: &Rc<MinlogTerm>) -> Vec<Rc<MinlogTerm>> {
        let mut inner = term.inner_internal_constants();
        
        if term.is_internal_constant() {
            inner.push(term.clone());
        }
        
        inner
    }
    
    pub fn contains_internal_constant(term: &Rc<MinlogTerm>, ic: &Rc<MinlogTerm>) -> bool {
        ic.is_internal_constant() && MinlogTerm::get_internal_constants(term).contains(ic)
    }
}

#[derive(PartialEq, Eq, Clone)]
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
    
    fn normalize(self: &Self, _eta: bool, _pi: bool) -> Rc<MinlogTerm> {
        unimplemented!()
    }
    
    fn alpha_equivalent(&self, _other: &Rc<MinlogTerm>,
        _forward: &mut Vec<(TermVariable, TermVariable)>,
        _backward: &mut Vec<(TermVariable, TermVariable)>) -> bool {
            unimplemented!()
    }
    
    fn substitute(self: &Self, _from: &TermSubstEntry, _to: &TermSubstEntry) -> Rc<MinlogTerm> {
        unimplemented!()
    }

    fn first_conflict_with(self: &Self, _other: &Rc<MinlogTerm>) -> Option<(Rc<MinlogTerm>, Rc<MinlogTerm>)> {
        unimplemented!()
    }
    
    fn match_with(self: &Self, _ctx: &mut impl MatchContext<TermSubstEntry>)
        -> Result<Option<(TermSubstEntry, TermSubstEntry)>, ()> {
            unimplemented!()
    }
}