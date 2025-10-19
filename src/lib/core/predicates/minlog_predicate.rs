
use std::rc::Rc;
use crate::utils::pretty_printer::{PrettyPrintable, PPElement};

use crate::core::types::minlog_type::MinlogType;
use crate::core::terms::minlog_term::MinlogTerm;
use crate::core::formulas::minlog_formula::MinlogFormula;

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
    pub fn get_type_variables(pred: &Rc<MinlogPredicate>) -> Vec<Rc<MinlogType>> {
        let types = pred.arity();
        let mut type_vars = vec![];
        
        for t in types {
            for var in t.get_type_variables() {
                if !type_vars.contains(&var) {
                    type_vars.push(var);
                }
            }
        }
        
        type_vars
    }
    
    pub fn to_cterm(pred: &Rc<MinlogPredicate>) -> Rc<MinlogPredicate> {
        todo!()
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
}