
use std::rc::Rc;
use crate::utils::pretty_printer::{PrettyPrintable, PPElement};

use crate::core::substitution::{MatchContext, MatchOutput};

use crate::core::types::minlog_type::MinlogType;
use crate::core::terms::minlog_term::MinlogTerm;
use crate::core::predicates::minlog_predicate::MinlogPredicate;

use crate::core::predicates::predicate_substitution::PredSubstEntry;

#[derive(PartialEq, Eq, Clone)]
pub struct FormulaOfNulltype {
    pub positive_nulltype: bool,
    pub negative_nulltype: bool,
}

crate::wrapper_enum! {
    
    @default { EmptyFormulaBody }
    pub trait FormulaBody: PrettyPrintable, Clone, PartialEq, Eq {
        pub fn of_nulltype(&Self) -> FormulaOfNulltype {
            FormulaOfNulltype { positive_nulltype: false, negative_nulltype: false }
        }
        
        pub fn fold(&Self) -> Rc<MinlogFormula>
        pub fn unfold(&Self) -> Rc<MinlogFormula>

        pub fn normalize(&Self, eta: bool, pi: bool) -> Rc<MinlogFormula>
        
        pub fn depth(&Self) -> usize {
            0
        }
        
        pub fn ex_free(&Self) -> bool {
            false
        }
        
        pub fn to_nc_formula(&Self) -> Rc<MinlogFormula>

        pub fn to_normal_form(&Self, eta: bool, pi: bool) -> Rc<MinlogFormula>

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
        
        pub fn get_prime_formulas(&Self) -> Vec<Rc<MinlogFormula>> {
            vec![]
        }
        
        pub fn equivalent(&Self, other: &Rc<MinlogFormula>) -> bool
        
        pub fn substitute(&Self, from: &PredSubstEntry, to: &PredSubstEntry) -> Rc<MinlogFormula>
        
        pub fn first_conflict_with(&Self, other: &Rc<MinlogFormula>) -> Option<(PredSubstEntry, PredSubstEntry)>
        
        pub fn match_with(&Self, ctx: &mut impl MatchContext<PredSubstEntry>) -> MatchOutput<PredSubstEntry>
    }
    
    #[derive(PartialEq, Eq)]
    pub enum MinlogFormula {
        Atom(||atom||),
        Implication(||implication||),
        AllQuantifier(||all_quantifier||),
        Tensor(||tensor||),
    }
    
    impl PrettyPrintable {
        fn to_pp_element(&Self, detail: bool) -> PPElement;

        fn requires_parens(&Self, detail: bool) -> bool;

        fn open_paren(&Self) -> String;

        fn close_paren(&Self) -> String;
    }
}

impl MinlogFormula {
    
}

#[derive(PartialEq, Eq, Clone)]
pub struct EmptyFormulaBody;

impl PrettyPrintable for EmptyFormulaBody {
    fn to_pp_element(&self, _detail: bool) -> PPElement {
        PPElement::text("<<empty formula>>".to_string())
    }
}

impl FormulaBody for EmptyFormulaBody {
    fn fold(&self) -> Rc<MinlogFormula> {
        unimplemented!()
    }
    
    fn unfold(&self) -> Rc<MinlogFormula> {
        unimplemented!()
    }
    
    fn normalize(&self, _eta: bool, _pi: bool) -> Rc<MinlogFormula> {
        unimplemented!()
    }
    
    fn to_nc_formula(&self) -> Rc<MinlogFormula> {
        unimplemented!()
    }
    
    fn to_normal_form(&self, _eta: bool, _pi: bool) -> Rc<MinlogFormula> {
        unimplemented!()
    }
    
    fn equivalent(&self, _other: &Rc<MinlogFormula>) -> bool {
        unimplemented!()
    }
    
    fn substitute(&self, _from: &PredSubstEntry, _to: &PredSubstEntry) -> Rc<MinlogFormula> {
        unimplemented!()
    }
    
    fn first_conflict_with(&self, _other: &Rc<MinlogFormula>) -> Option<(PredSubstEntry, PredSubstEntry)> {
        unimplemented!()
    }
    
    fn match_with(&self, _ctx: &mut impl MatchContext<PredSubstEntry>) -> MatchOutput<PredSubstEntry> {
        unimplemented!()
    }
}