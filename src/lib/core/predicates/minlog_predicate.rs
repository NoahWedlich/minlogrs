
use std::rc::Rc;
use crate::utils::pretty_printer::{PrettyPrintable, PPElement};

use crate::core::types::minlog_type::MinlogType;

crate::wrapper_enum! {
    
    @default { EmptyPredicateBody }
    pub trait PredicateBody: PrettyPrintable, Clone, PartialEq, Eq {
        pub fn arity(&Self) -> Vec<Rc<MinlogType>>
    }
    
    #[derive(PartialEq, Eq)]
    pub enum MinlogPredicate {
        Constant(|constant|),
        Variable(|variable|),
        Comprehension(|comprehension|),
        Inductive(|inductive|),
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
            for var in MinlogType::get_type_variables(&t) {
                if !type_vars.contains(&var) {
                    type_vars.push(var);
                }
            }
        }
        
        type_vars
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