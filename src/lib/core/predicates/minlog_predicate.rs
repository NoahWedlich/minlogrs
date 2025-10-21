
use std::rc::Rc;
use crate::utils::pretty_printer::{PrettyPrintable, PPElement};

use crate::core::substitution::{MatchContext, MatchOutput};
use crate::core::polarity::{Polarity, Polarized};

use crate::core::types::minlog_type::MinlogType;
use crate::core::terms::minlog_term::MinlogTerm;

use crate::core::formulas::minlog_formula::MinlogFormula;
use crate::core::formulas::prime_formula::PrimeFormula;

use crate::core::predicates::predicate_variable::PredicateVariable;
use crate::core::predicates::comprehension_term::ComprehensionTerm;

use crate::core::predicates::predicate_substitution::PredSubstEntry;

#[derive(PartialEq, Eq, Clone)]
pub struct PredicateDegree {
    pub positive_content: bool,
    pub negative_content: bool,
}

crate::wrapper_enum! {
    
    @default { EmptyPredicateBody }
    pub trait PredicateBody: PrettyPrintable, Clone, PartialEq, Eq {
        pub fn arity(&Self) -> Vec<Rc<MinlogType>>
        
        pub fn degree(&Self) -> PredicateDegree {
            PredicateDegree { positive_content: false, negative_content: false }
        }
        
        pub fn depth(&Self) -> usize {
            0
        }
        
        pub fn extracted_type(&Self) -> Rc<MinlogType>
        
        pub fn get_type_variables(&Self) -> Vec<Rc<MinlogType>> {
            vec![]
        }
        
        pub fn get_algebra_types(&Self) -> Vec<Rc<MinlogType>> {
            vec![]
        }

        pub fn get_free_variables(&Self) -> Vec<Rc<MinlogTerm>> {
            vec![]
        }
        
        pub fn get_bound_variables(&Self) -> Vec<Rc<MinlogTerm>> {
            vec![]
        }
        
        pub fn get_polarized_pred_vars(&Self, _current: Polarity) -> Vec<Polarized<Rc<MinlogPredicate>>> {
            vec![]
        }
        
        pub fn get_polarized_comp_terms(&Self, _current: Polarity) -> Vec<Polarized<Rc<MinlogPredicate>>> {
            vec![]
        }
        
        pub fn get_polarized_inductive_preds(&Self, _current: Polarity) -> Vec<Polarized<Rc<MinlogPredicate>>> {
            vec![]
        }
        
        pub fn get_polarized_prime_formulas(&Self, _current: Polarity) -> Vec<Polarized<Rc<MinlogFormula>>> {
            vec![]
        }
        
        pub fn substitute(&Self, from: &PredSubstEntry, to: &PredSubstEntry) -> Rc<MinlogPredicate>
        
        pub fn first_conflict_with(&Self, other: &Rc<MinlogPredicate>) -> Option<(PredSubstEntry, PredSubstEntry)>
        
        pub fn match_with(&Self, ctx: &mut impl MatchContext<PredSubstEntry>) -> MatchOutput<PredSubstEntry>
    }
    
    #[derive(PartialEq, Eq)]
    pub enum MinlogPredicate {
        InternalPredicate(||internal_predicate||),
        Variable(||variable|| PredicateVariable),
        Comprehension(||comprehension_term|| ComprehensionTerm),
        Inductive(||inductive_predicate||),
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
        let free_vars = pred.get_free_variables();
        let body = PrimeFormula::create(pred.clone(), free_vars.clone());
        ComprehensionTerm::create(free_vars, body)
    }
    
    pub fn get_predicate_variables(&self) -> Vec<Rc<MinlogPredicate>> {
        self.get_polarized_pred_vars(Polarity::Unknown)
            .into_iter().map(|p| p.value).collect()
    }
    
    pub fn get_comprehension_terms(&self) -> Vec<Rc<MinlogPredicate>> {
        self.get_polarized_comp_terms(Polarity::Unknown)
            .into_iter().map(|p| p.value).collect()
    }
    
    pub fn get_inductive_predicates(&self) -> Vec<Rc<MinlogPredicate>> {
        self.get_polarized_inductive_preds(Polarity::Unknown)
            .into_iter().map(|p| p.value).collect()
    }

    pub fn get_prime_formulas(&self) -> Vec<Rc<MinlogFormula>> {
        self.get_polarized_prime_formulas(Polarity::Unknown)
            .into_iter().map(|p| p.value).collect()
    }

    pub fn contains_type_variable(&self, var: &Rc<MinlogType>) -> bool {
        var.is_variable() && self.get_type_variables().contains(var)
    }
    
    pub fn contains_algebra_type(&self, alg: &Rc<MinlogType>) -> bool {
        alg.is_algebra() && self.get_algebra_types().contains(alg)
    }
    
    pub fn contains_free_variable(&self, var: &Rc<MinlogTerm>) -> bool {
        var.is_variable() && self.get_free_variables().contains(var)
    }
    
    pub fn contains_bound_variable(&self, var: &Rc<MinlogTerm>) -> bool {
        var.is_variable() && self.get_bound_variables().contains(var)
    }
    
    pub fn contains_predicate_variable(&self, pvar: &Rc<MinlogPredicate>) -> bool {
        pvar.is_variable() && self.get_predicate_variables().contains(pvar)
    }
    
    pub fn contains_comprehension_term(&self, cterm: &Rc<MinlogPredicate>) -> bool {
        cterm.is_comprehension_term() && self.get_comprehension_terms().contains(cterm)
    }
    
    pub fn contains_inductive_predicate(&self, ipred: &Rc<MinlogPredicate>) -> bool {
        ipred.is_inductive_predicate() && self.get_inductive_predicates().contains(ipred)
    }
    
    pub fn contains_prime_formula(&self, pform: &Rc<MinlogFormula>) -> bool {
        pform.is_prime() && self.get_prime_formulas().contains(pform)
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
    
    fn extracted_type(&self) -> Rc<MinlogType> {
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