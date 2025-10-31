
use std::{rc::Rc, hash::Hash, collections::HashSet};
use crate::utils::pretty_printer::{PrettyPrintable, PPElement};

use crate::core::substitution::{MatchContext, MatchOutput};
use crate::core::polarity::{Polarity, Polarized};

use crate::core::types::minlog_type::MinlogType;

use crate::core::terms::minlog_term::{MinlogTerm, Totality};
use crate::core::terms::term_variable::TermVariable;

use crate::core::formulas::minlog_formula::MinlogFormula;
use crate::core::formulas::prime_formula::PrimeFormula;

use crate::core::predicates::predicate_variable::PredicateVariable;
use crate::core::predicates::comprehension_term::ComprehensionTerm;
use crate::core::predicates::inductive_predicate::InductivePredicate;

use crate::core::predicates::predicate_substitution::PredSubstEntry;

#[derive(PartialEq, Eq, Clone, Hash)]
pub struct PredicateDegree {
    pub positive_content: bool,
    pub negative_content: bool,
}

crate::wrapper_enum! {
    
    @default { EmptyPredicateBody }
    pub trait PredicateBody: PrettyPrintable, Clone, PartialEq, Eq, Hash {
        pub fn arity(&Self) -> Rc<MinlogType>
        
        pub fn degree(&Self) -> PredicateDegree {
            PredicateDegree { positive_content: false, negative_content: false }
        }
        
        pub fn depth(&Self) -> usize {
            0
        }
        
        pub fn extracted_type(&Self) -> Rc<MinlogType>
        
        pub fn get_type_variables(&Self) -> HashSet<Rc<MinlogType>> {
            HashSet::new()
        }
        
        pub fn get_algebra_types(&Self) -> HashSet<Rc<MinlogType>> {
            HashSet::new()
        }

        pub fn get_free_variables(&Self) -> HashSet<Rc<MinlogTerm>> {
            HashSet::new()
        }
        
        pub fn get_bound_variables(&Self) -> HashSet<Rc<MinlogTerm>> {
            HashSet::new()
        }
        
        pub fn get_polarized_pred_vars(&Self, _current: Polarity) -> HashSet<Polarized<Rc<MinlogPredicate>>> {
            HashSet::new()
        }
        
        pub fn get_polarized_comp_terms(&Self, _current: Polarity) -> HashSet<Polarized<Rc<MinlogPredicate>>> {
            HashSet::new()
        }
        
        pub fn get_polarized_inductive_preds(&Self, _current: Polarity) -> HashSet<Polarized<Rc<MinlogPredicate>>> {
            HashSet::new()
        }
        
        pub fn get_polarized_prime_formulas(&Self, _current: Polarity) -> HashSet<Polarized<Rc<MinlogFormula>>> {
            HashSet::new()
        }
        
        pub fn substitute(&Self, from: &PredSubstEntry, to: &PredSubstEntry) -> Rc<MinlogPredicate>
        
        pub fn first_conflict_with(&Self, other: &Rc<MinlogPredicate>) -> Option<(PredSubstEntry, PredSubstEntry)>
        
        pub fn match_with(&Self, ctx: &mut impl MatchContext<PredSubstEntry>) -> MatchOutput<PredSubstEntry>
    }
    
    #[derive(PartialEq, Eq, Hash)]
    pub enum MinlogPredicate {
        InternalPredicate(||internal_predicate||),
        Variable(||variable|| PredicateVariable),
        Comprehension(||comprehension_term|| ComprehensionTerm),
        InductivePredicate(||inductive_predicate|| InductivePredicate),
    }
    
    impl PrettyPrintable {
        fn to_pp_element(&Self, detail: bool) -> PPElement;

        fn requires_parens(&Self, detail: bool) -> bool;

        fn open_paren(&Self) -> String;

        fn close_paren(&Self) -> String;
    }
}

impl MinlogPredicate {
    pub fn unpacked_arity(&self) -> Vec<Rc<MinlogType>> {
        if let Some(tuple_type) = self.arity().to_tuple() {
            tuple_type.types().clone()
        } else {
            vec![self.arity().clone()]
        }
    }
    
    pub fn is_formula(&self) -> bool {
        self.unpacked_arity().is_empty()
    }
    
    pub fn to_cterm(pred: &Rc<MinlogPredicate>) -> Rc<MinlogPredicate> {
        let arity = pred.unpacked_arity();
        let vars = arity.iter().enumerate().map(|(i, t)| {
            TermVariable::create(format!("T{}", i), t.clone(), Totality::Partial)
        }).collect::<Vec<_>>();
        let prime_formula = PrimeFormula::create(pred.clone(), vars.clone());
        ComprehensionTerm::create(vars, prime_formula)
    }
    
    pub fn get_predicate_variables(&self) -> HashSet<Rc<MinlogPredicate>> {
        self.get_polarized_pred_vars(Polarity::Unknown)
            .into_iter().map(|p| p.value).collect()
    }
    
    pub fn get_comprehension_terms(&self) -> HashSet<Rc<MinlogPredicate>> {
        self.get_polarized_comp_terms(Polarity::Unknown)
            .into_iter().map(|p| p.value).collect()
    }
    
    pub fn get_inductive_predicates(&self) -> HashSet<Rc<MinlogPredicate>> {
        self.get_polarized_inductive_preds(Polarity::Unknown)
            .into_iter().map(|p| p.value).collect()
    }

    pub fn get_prime_formulas(&self) -> HashSet<Rc<MinlogFormula>> {
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

#[derive(PartialEq, Eq, Clone, Hash)]
pub struct EmptyPredicateBody;

impl PrettyPrintable for EmptyPredicateBody {
    fn to_pp_element(&self, _detail: bool) -> PPElement {
        PPElement::text("<<empty predicate>>".to_string())
    }
}

impl PredicateBody for EmptyPredicateBody {
    fn arity(&self) -> Rc<MinlogType> {
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