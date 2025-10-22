
use std::{rc::Rc, collections::HashSet};
use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};

use crate::core::substitution::{MatchContext, MatchOutput};
use crate::core::polarity::{Polarity, Polarized};

use crate::core::types::minlog_type::MinlogType;
use crate::core::terms::minlog_term::MinlogTerm;
use crate::core::predicates::minlog_predicate::MinlogPredicate;

use crate::core::formulas::minlog_formula::{FormulaBody, MinlogFormula, FormulaOfNulltype};

use crate::core::predicates::predicate_substitution::PredSubstEntry;


#[derive(PartialEq, Eq, Clone, Hash)]
pub struct PrimeFormula {
    pub body: Rc<MinlogPredicate>,
    pub arguments: Vec<Rc<MinlogTerm>>,
}

impl PrimeFormula {
    pub fn create(body: Rc<MinlogPredicate>, arguments: Vec<Rc<MinlogTerm>>) -> Rc<MinlogFormula> {
        if body.arity().len() != arguments.len() {
            panic!("Number of arguments does not match predicate arity");
        }
        
        for (arg, expected_type) in arguments.iter().zip(body.arity().iter()) {
            if arg.minlog_type() != *expected_type {
                panic!("Argument type does not match predicate arity type");
            }
        }
        
        Rc::new(MinlogFormula::Prime(PrimeFormula { body, arguments }))
    }
    
    pub fn body(&self) -> &Rc<MinlogPredicate> {
        &self.body
    }
    
    pub fn argument_count(&self) -> usize {
        self.arguments.len()
    }
    
    pub fn arguments(&self) -> &Vec<Rc<MinlogTerm>> {
        &self.arguments
    }
    
    pub fn argument(&self, index: usize) -> Option<&Rc<MinlogTerm>> {
        self.arguments.get(index)
    }
}

impl FormulaBody for PrimeFormula {
    fn of_nulltype(&self) -> FormulaOfNulltype {
        let pred_degree = self.body.degree();
        FormulaOfNulltype {
            positive_nulltype: !pred_degree.positive_content,
            negative_nulltype: !pred_degree.negative_content,
        }
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> Rc<MinlogFormula> {
        let norm_args: Vec<Rc<MinlogTerm>> = self.arguments.iter()
            .map(|arg| arg.normalize(eta, pi))
            .collect();
        
        let norm_body = if self.body.is_internal_predicate() {
            todo!() // Compute normalized internal predicate
        } else {
            self.body.clone()
        };
        
        PrimeFormula::create(norm_body, norm_args)
    }
    
    fn depth(&self) -> usize {
        1 + self.arguments.iter().map(|arg| arg.depth())
            .chain(std::iter::once(self.body.depth()))
            .max().unwrap_or(0)
    }
    
    fn extracted_type(&self) -> Rc<MinlogType> {
        self.body.extracted_type()
    }
    
    fn get_type_variables(&self) -> HashSet<Rc<MinlogType>> {
        self.arguments.iter()
            .flat_map(|arg| arg.get_type_variables())
            .chain(self.body.get_type_variables())
            .collect()
    }
    
    fn get_algebra_types(&self) -> HashSet<Rc<MinlogType>> {
        self.arguments.iter()
            .flat_map(|arg| arg.get_algebra_types())
            .chain(self.body.get_algebra_types())
            .collect()
    }
    
    fn get_free_variables(&self) -> HashSet<Rc<MinlogTerm>> {
        self.arguments.iter()
            .flat_map(|arg| arg.get_free_variables())
            .chain(self.body.get_free_variables())
            .collect()
    }
    
    fn get_bound_variables(&self) -> HashSet<Rc<MinlogTerm>> {
        self.arguments.iter()
            .flat_map(|arg| arg.get_bound_variables())
            .chain(self.body.get_bound_variables())
            .collect()
    }
    
    fn get_polarized_pred_vars(&self, _current: Polarity) -> HashSet<Polarized<Rc<MinlogPredicate>>> {
        self.body.get_polarized_pred_vars(_current)
    }
    
    fn get_polarized_comp_terms(&self, _current: Polarity) -> HashSet<Polarized<Rc<MinlogPredicate>>> {
        self.body.get_polarized_comp_terms(_current)
    }
    
    fn get_polarized_inductive_preds(&self, _current: Polarity) -> HashSet<Polarized<Rc<MinlogPredicate>>> {
        self.body.get_polarized_inductive_preds(_current)
    }
    
    fn get_polarized_prime_formulas(&self, current: Polarity) -> HashSet<Polarized<Rc<MinlogFormula>>> {
        let mut primes = self.body.get_polarized_prime_formulas(current);
        primes.insert(Polarized::new(current, Rc::new(MinlogFormula::Prime(self.clone()))));
        primes
    }
    
    fn substitute(&self, from: &PredSubstEntry, to: &PredSubstEntry) -> Rc<MinlogFormula> {
        let sub_args: Vec<Rc<MinlogTerm>> = if from.is_term_subst_entry() {
            self.arguments.iter()
                .map(|arg| arg.substitute(
                    &from.to_term_subst_entry().unwrap().clone(),
                    &to.to_term_subst_entry().unwrap().clone()
                ))
                .collect()
        } else {
            self.arguments.clone()
        };
        
        let sub_body = self.body.substitute(from, to);
        
        PrimeFormula::create(sub_body, sub_args)
    }
    
    fn first_conflict_with(&self, other: &Rc<MinlogFormula>) -> Option<(PredSubstEntry, PredSubstEntry)> {
        if let Some(other_prime) = other.to_prime() {
            if self.arguments.len() != other_prime.arguments.len() {
                return Some((Rc::new(MinlogFormula::Prime(self.clone())).into(), other.clone().into()));
            }
            
            for (arg_self, arg_other) in self.arguments.iter().zip(other_prime.arguments.iter()) {
                if let Some(conflict) = arg_self.first_conflict_with(arg_other) {
                    return Some((conflict.0.into(), conflict.1.into()));
                }
            }
            
            self.body.first_conflict_with(&other_prime.body)
        } else {
            Some((Rc::new(MinlogFormula::Prime(self.clone())).into(), other.clone().into()))
        }
    }
    
    fn match_with(&self, ctx: &mut impl MatchContext<PredSubstEntry>) -> MatchOutput<PredSubstEntry> {
        let pattern = ctx.next_pattern().unwrap();
        let instance = ctx.next_instance().unwrap();
        
        match (pattern, instance) {
            (PredSubstEntry::Formula(p), PredSubstEntry::Formula(i)) => {
                if !p.is_prime() || !i.is_prime() {
                    return MatchOutput::FailedMatch;
                }
                
                let prime_pattern = p.to_prime().unwrap();
                let prime_instance = i.to_prime().unwrap();
                
                if prime_pattern.arguments.len() != prime_instance.arguments.len() {
                    return MatchOutput::FailedMatch;
                }
                
                for (arg_pattern, arg_instance) in prime_pattern.arguments.iter().zip(prime_instance.arguments.iter()) {
                    if arg_pattern != arg_instance {
                        ctx.extend(&arg_pattern.clone().into(), &arg_instance.clone().into());
                    }
                }
                
                if prime_pattern.body != prime_instance.body {
                    ctx.extend(&prime_pattern.body.clone().into(), &prime_instance.body.clone().into());
                }
                
                MatchOutput::Matched
            },
            _ => MatchOutput::FailedMatch,
        }
    }
}

impl PrettyPrintable for PrimeFormula {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        if self.arguments.is_empty() {
            return self.body.to_pp_element(detail);
        }
        
        let arguments = PPElement::list(
            self.arguments.iter().map(|arg| arg.to_pp_element(detail)).collect(),
            PPElement::break_elem(0, 0, false),
            PPElement::text(",".to_string()),
            PPElement::break_elem(1, 0, false),
            BreakType::Flexible,
        );
        
        PPElement::group(vec![
            self.body.to_enclosed_pp_element(detail),
            PPElement::text(" (".to_string()),
            PPElement::break_elem(1, 4, false),
            arguments,
            PPElement::break_elem(1, 0, false),
            PPElement::text(")".to_string()),
        ], BreakType::Consistent, 0)
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        true
    }
    
    fn open_paren(&self) -> String {
        "(".to_string()
    }
    
    fn close_paren(&self) -> String {
        ")".to_string()
    }
}