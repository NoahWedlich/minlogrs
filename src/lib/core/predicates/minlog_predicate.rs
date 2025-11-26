
use indexmap::IndexSet;
use std::{rc::Rc, hash::Hash};
use crate::utils::pretty_printer::{PrettyPrintable, PPElement};

use crate::core::substitution::MatchOutput;
use crate::core::polarity::{Polarity, Polarized};

use crate::core::types::minlog_type::MinlogType;

use crate::core::terms::minlog_term::MinlogTerm;
use crate::core::terms::term_variable::TermVariable;
use crate::core::terms::term_substitution::TermSubstitution;

use crate::core::predicates::predicate_wildcard::PredicateWildcard;
use crate::core::predicates::predicate_variable::PredicateVariable;
use crate::core::predicates::comprehension_term::ComprehensionTerm;
use crate::core::predicates::inductive_predicate::InductivePredicate;
use crate::core::predicates::prime_formula::PrimeFormula;
use crate::core::predicates::implication::Implication;
use crate::core::predicates::all_quantifier::AllQuantifier;

use crate::core::predicates::predicate_substitution::PredSubstEntry;

crate::wrapper_enum! {
    pub trait PredicateBody: PrettyPrintable, Clone, PartialEq, Eq, Hash {
        pub fn arity(&Self) -> Rc<MinlogType>
        
        pub fn normalize(&Self, eta: bool, pi: bool) -> Rc<MinlogPredicate>
        
        pub fn depth(&Self) -> usize {
            0
        }
        
        pub fn extracted_type_pattern(&Self) -> Rc<MinlogType>
        pub fn extracted_type(&Self) -> Rc<MinlogType>
        pub fn et_pattern_to_et(&Self) -> TermSubstitution
        
        pub fn get_type_variables(&Self, _visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Rc<MinlogType>> {
            IndexSet::new()
        }
        
        pub fn get_algebra_types(&Self, _visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Rc<MinlogType>> {
            IndexSet::new()
        }

        pub fn get_free_variables(&Self, _visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Rc<MinlogTerm>> {
            IndexSet::new()
        }
        
        pub fn get_bound_variables(&Self, _visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Rc<MinlogTerm>> {
            IndexSet::new()
        }
        
        pub fn get_polarized_pred_vars(&Self, _current: Polarity, _visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Polarized<Rc<MinlogPredicate>>> {
            IndexSet::new()
        }
        
        pub fn get_polarized_comp_terms(&Self, _current: Polarity, _visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Polarized<Rc<MinlogPredicate>>> {
            IndexSet::new()
        }
        
        pub fn get_polarized_inductive_preds(&Self, _current: Polarity, _visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Polarized<Rc<MinlogPredicate>>> {
            IndexSet::new()
        }
        
        pub fn get_polarized_prime_formulas(&Self, _current: Polarity, _visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Polarized<Rc<MinlogPredicate>>> {
            IndexSet::new()
        }
        
        pub fn substitute(&Self, from: &PredSubstEntry, to: &PredSubstEntry) -> Rc<MinlogPredicate>
        
        pub fn first_conflict_with(&Self, other: &Rc<MinlogPredicate>) -> Option<(PredSubstEntry, PredSubstEntry)>
        
        pub fn match_with(&Self, ctx: &Rc<MinlogPredicate>) -> MatchOutput<PredSubstEntry>
    }
    
    #[derive(PartialEq, Eq, Hash)]
    pub enum MinlogPredicate {
        Wildcard(|wildcard| PredicateWildcard),
        Variable(||variable|| PredicateVariable),
        Comprehension(||comprehension_term|| ComprehensionTerm),
        InductivePredicate(||inductive_predicate|| InductivePredicate),
        Prime(||prime|| PrimeFormula),
        Implication(||implication|| Implication),
        AllQuantifier(||all_quantifier|| AllQuantifier),
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
            TermVariable::create(format!("T{}", i), t.clone())
        }).collect::<Vec<_>>();
        let prime_formula = PrimeFormula::create(pred.clone(), vars.clone());
        ComprehensionTerm::create(vars, prime_formula)
    }
    
    pub fn get_predicate_variables(&self, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Rc<MinlogPredicate>> {
        self.get_polarized_pred_vars(Polarity::Unknown, visited)
            .into_iter().map(|p| p.value).collect()
    }
    
    pub fn get_comprehension_terms(&self, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Rc<MinlogPredicate>> {
        self.get_polarized_comp_terms(Polarity::Unknown, visited)
            .into_iter().map(|p| p.value).collect()
    }
    
    pub fn get_inductive_predicates(&self, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Rc<MinlogPredicate>> {
        self.get_polarized_inductive_preds(Polarity::Unknown, visited)
            .into_iter().map(|p| p.value).collect()
    }

    pub fn get_prime_formulas(&self, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Rc<MinlogPredicate>> {
        self.get_polarized_prime_formulas(Polarity::Unknown, visited)
            .into_iter().map(|p| p.value).collect()
    }

    pub fn contains_type_variable(&self, var: &Rc<MinlogType>) -> bool {
        var.is_variable() && self.get_type_variables(&mut IndexSet::new()).contains(var)
    }
    
    pub fn contains_algebra_type(&self, alg: &Rc<MinlogType>) -> bool {
        alg.is_algebra() && self.get_algebra_types(&mut IndexSet::new()).contains(alg)
    }
    
    pub fn contains_free_variable(&self, var: &Rc<MinlogTerm>) -> bool {
        var.is_variable() && self.get_free_variables(&mut IndexSet::new()).contains(var)
    }
    
    pub fn contains_bound_variable(&self, var: &Rc<MinlogTerm>) -> bool {
        var.is_variable() && self.get_bound_variables(&mut IndexSet::new()).contains(var)
    }
    
    pub fn contains_predicate_variable(&self, pvar: &Rc<MinlogPredicate>) -> bool {
        pvar.is_variable() && self.get_predicate_variables(&mut IndexSet::new()).contains(pvar)
    }
    
    pub fn contains_comprehension_term(&self, cterm: &Rc<MinlogPredicate>) -> bool {
        cterm.is_comprehension_term() && self.get_comprehension_terms(&mut IndexSet::new()).contains(cterm)
    }
    
    pub fn contains_inductive_predicate(&self, ipred: &Rc<MinlogPredicate>) -> bool {
        ipred.is_inductive_predicate() && self.get_inductive_predicates(&mut IndexSet::new()).contains(ipred)
    }
    
    pub fn contains_prime_formula(&self, pform: &Rc<MinlogPredicate>) -> bool {
        pform.is_prime() && self.get_prime_formulas(&mut IndexSet::new()).contains(pform)
    }
}