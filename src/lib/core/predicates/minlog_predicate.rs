
use std::rc::Rc;
use crate::utils::pretty_printer::{PrettyPrintable, PPElement};

use crate::core::substitution::{MatchContext, MatchOutput};

use crate::core::types::minlog_type::MinlogType;
use crate::core::terms::minlog_term::MinlogTerm;
use crate::core::formulas::minlog_formula::MinlogFormula;

use crate::core::predicates::predicate_substitution::PredSubstEntry;

#[derive(PartialEq, Eq, Clone)]
pub struct PredicateDegree {
    positive_content: bool,
    negative_content: bool,
}

crate::wrapper_enum! {
    
    @default { EmptyPredicateBody }
    pub trait PredicateBody: PrettyPrintable, Clone, PartialEq, Eq {
        pub fn arity(&Self) -> Vec<Rc<MinlogType>>
        
        pub fn degree(&Self) -> PredicateDegree {
            PredicateDegree { positive_content: true, negative_content: true }
        }
        
        pub fn get_free_variables(&Self) -> Vec<Rc<MinlogTerm>> {
            vec![]
        }
        
        pub fn get_bound_variables(&Self) -> Vec<Rc<MinlogTerm>> {
            vec![]
        }
        
        pub fn get_type_variables(&Self) -> Vec<Rc<MinlogType>> {
            vec![]
        }
        
        pub fn get_predicate_variables(&Self) -> Vec<Rc<MinlogPredicate>> {
            vec![]
        }
        
        pub fn get_comprehension_terms(&Self) -> Vec<Rc<MinlogPredicate>> {
            vec![]
        }
        
        pub fn get_inductive_predicates(&Self) -> Vec<Rc<MinlogPredicate>> {
            vec![]
        }
        
        pub fn get_formulas(&Self) -> Vec<Rc<MinlogFormula>> {
            vec![]
        }
        
        pub fn substitute(&Self, from: &PredSubstEntry, to: &PredSubstEntry) -> Rc<MinlogPredicate>
        
        pub fn first_conflict_with(&Self, other: &Rc<MinlogPredicate>) -> Option<(PredSubstEntry, PredSubstEntry)>
        
        pub fn match_with(&Self, ctx: &mut impl MatchContext<PredSubstEntry>) -> MatchOutput<PredSubstEntry>
    }
    
    #[derive(PartialEq, Eq)]
    pub enum MinlogPredicate {
        Constant(||constant||),
        Variable(||variable||),
        Comprehension(||comprehension||),
        Inductive(||inductive||),
    }
    
    impl PrettyPrintable {
        fn to_pp_element(&Self, detail: bool) -> PPElement;

        fn requires_parens(&Self, detail: bool) -> bool;

        fn open_paren(&Self) -> String;

        fn close_paren(&Self) -> String;
    }
}

impl MinlogPredicate {
    pub fn to_cterm(pred: &Rc<MinlogPredicate>) -> Rc<MinlogPredicate> {
        todo!()
    }
    
    pub fn contains_free_variable(pred: &Rc<MinlogPredicate>, var: &Rc<MinlogTerm>) -> bool {
        var.is_variable() && pred.get_free_variables().contains(var)
    }
    
    pub fn contains_bound_variable(pred: &Rc<MinlogPredicate>, var: &Rc<MinlogTerm>) -> bool {
        var.is_variable() && pred.get_bound_variables().contains(var)
    }
    
    pub fn contains_type_variable(pred: &Rc<MinlogPredicate>, tvar: &Rc<MinlogType>) -> bool {
        pred.get_type_variables().contains(tvar)
    }

    pub fn contains_predicate_variable(pred: &Rc<MinlogPredicate>, pvar: &Rc<MinlogPredicate>) -> bool {
        pred.get_predicate_variables().contains(pvar)
    }
    
    pub fn contains_comprehension_term(pred: &Rc<MinlogPredicate>, cterm: &Rc<MinlogPredicate>) -> bool {
        pred.get_comprehension_terms().contains(cterm)
    }
    
    pub fn contains_inductive_predicate(pred: &Rc<MinlogPredicate>, ipred: &Rc<MinlogPredicate>) -> bool {
        pred.get_inductive_predicates().contains(ipred)
    }
    
    pub fn contains_formula(pred: &Rc<MinlogPredicate>, formula: &Rc<MinlogFormula>) -> bool {
        pred.get_formulas().contains(formula)
    }
}

#[derive(PartialEq, Eq, Clone)]
pub struct EmptyPredicateBody;

impl PrettyPrintable for EmptyPredicateBody {
    fn to_pp_element(&self, _detail: bool) -> PPElement {
        PPElement::text("<<empty predicate>>".to_string())
    }
}

impl PredicateBody for EmptyPredicateBody {
    fn arity(&self) -> Vec<Rc<MinlogType>> {
        unimplemented!()
    }
    
    fn substitute(&self, _from: &PredSubstEntry, _to: &PredSubstEntry) -> Rc<MinlogPredicate> {
        unimplemented!()
    }
    
    fn first_conflict_with(&self, _other: &Rc<MinlogPredicate>) -> Option<(PredSubstEntry, PredSubstEntry)> {
        unimplemented!()
    }
    
    fn match_with(&self, _ctx: &mut impl MatchContext<PredSubstEntry>) -> MatchOutput<PredSubstEntry> {
        unimplemented!()
    }
}