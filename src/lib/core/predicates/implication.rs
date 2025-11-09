use std::{rc::Rc, collections::HashSet};

use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};

use crate::core::substitution::{MatchContext, MatchOutput};
use crate::core::polarity::{Polarity, Polarized};

use crate::core::types::minlog_type::MinlogType;
use crate::core::types::type_constant::TypeConstant;
use crate::core::types::arrow_type::ArrowType;

use crate::core::terms::minlog_term::MinlogTerm;

use crate::core::predicates::minlog_predicate::{MinlogPredicate, PredicateBody};

use crate::core::predicates::predicate_substitution::PredSubstEntry;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Implication {
    premises: Vec<Rc<MinlogPredicate>>,
    conclusion: Rc<MinlogPredicate>,
}

impl Implication {
    pub fn create(premises: Vec<Rc<MinlogPredicate>>, conclusion: Rc<MinlogPredicate>) -> Rc<MinlogPredicate> {
        if premises.is_empty() {
            return conclusion;
        }
        
        for premise in &premises {
            if premise.arity() != conclusion.arity() {
                panic!("All premises and conclusion of an Implication must have the same arity");
            }
        }
        
        Implication::collapse(&Rc::new(MinlogPredicate::Implication(Implication { premises, conclusion })))
    }
    
    pub fn collapse(minlog_formula: &Rc<MinlogPredicate>) -> Rc<MinlogPredicate> {
        if !minlog_formula.is_implication() || !minlog_formula.to_implication().unwrap().conclusion.is_implication() {
            minlog_formula.clone()
        } else {
            let mut implication = minlog_formula.to_implication().unwrap();
            let mut premises = implication.premises().clone();
            
            while implication.conclusion.is_implication() {
                let next_implication = implication.conclusion.to_implication().unwrap();
                premises.extend(next_implication.premises().clone());
                implication = next_implication;
            }
            
            Implication::create(premises, implication.conclusion().clone())
        }
    }
    
    pub fn premises(&self) -> &Vec<Rc<MinlogPredicate>> {
        &self.premises
    }
    
    pub fn premise(&self, index: usize) -> Option<&Rc<MinlogPredicate>> {
        self.premises.get(index)
    }
    
    pub fn conclusion(&self) -> &Rc<MinlogPredicate> {
        &self.conclusion
    }
}

impl PredicateBody for Implication {
    fn arity(&self) -> Rc<MinlogType> {
        self.conclusion.arity()
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> Rc<MinlogPredicate> {
        let normalized_premises: Vec<Rc<MinlogPredicate>> = self.premises.iter()
            .map(|p| p.normalize(eta, pi))
            .collect();
        
        let normalized_conclusion = self.conclusion.normalize(eta, pi);
        
        Implication::create(normalized_premises, normalized_conclusion)
    }
    
    fn depth(&self) -> usize {
        1 + self.premises.iter().chain(std::iter::once(&self.conclusion))
            .map(|f| f.depth()).max().unwrap_or(0)
    }
    
    fn extracted_type(&self) -> Rc<MinlogType> {
        let premise_types: Vec<Rc<MinlogType>> = self.premises.iter()
            .map(|p| p.extracted_type())
            .collect();
        
        let conclusion_type = self.conclusion.extracted_type();
        
        ArrowType::create(premise_types, conclusion_type)
            .remove_nulls().unwrap_or(TypeConstant::create_null())
    }
    
    fn get_type_variables(&self, visited: &mut HashSet<MinlogPredicate>) -> HashSet<Rc<MinlogType>> {
        if visited.contains(&MinlogPredicate::Implication(self.clone())) {
            HashSet::new()
        } else {
            visited.insert(MinlogPredicate::Implication(self.clone()));
            
            self.conclusion.get_type_variables(visited).union(
                &self.premises.iter()
                    .flat_map(|p| p.get_type_variables(visited))
                    .collect()
            ).cloned().collect()
        }
    }
    
    fn get_algebra_types(&self, visited: &mut HashSet<MinlogPredicate>) -> HashSet<Rc<MinlogType>> {
        if visited.contains(&MinlogPredicate::Implication(self.clone())) {
            HashSet::new()
        } else {
            visited.insert(MinlogPredicate::Implication(self.clone()));
            
            self.conclusion.get_algebra_types(visited).union(
                &self.premises.iter()
                    .flat_map(|p| p.get_algebra_types(visited))
                    .collect()
            ).cloned().collect()
        }
    }
    
    fn get_free_variables(&self, visited: &mut HashSet<MinlogPredicate>) -> HashSet<Rc<MinlogTerm>> {
        if visited.contains(&MinlogPredicate::Implication(self.clone())) {
            HashSet::new()
        } else {
            visited.insert(MinlogPredicate::Implication(self.clone()));
            
            self.conclusion.get_free_variables(visited).union(
                &self.premises.iter()
                    .flat_map(|p| p.get_free_variables(visited))
                    .collect()
            ).cloned().collect()
        }
    }
    
    fn get_bound_variables(&self, visited: &mut HashSet<MinlogPredicate>) -> HashSet<Rc<MinlogTerm>> {
        if visited.contains(&MinlogPredicate::Implication(self.clone())) {
            HashSet::new()
        } else {
            visited.insert(MinlogPredicate::Implication(self.clone()));
            
            self.conclusion.get_bound_variables(visited).union(
                &self.premises.iter()
                    .flat_map(|p| p.get_bound_variables(visited))
                    .collect()
            ).cloned().collect()
        }
    }

    fn get_polarized_pred_vars(&self, current: Polarity, visited: &mut HashSet<MinlogPredicate>) -> HashSet<Polarized<Rc<MinlogPredicate>>> {
        if visited.contains(&MinlogPredicate::Implication(self.clone())) {
            HashSet::new()
        } else {
            visited.insert(MinlogPredicate::Implication(self.clone()));
            
            self.conclusion.get_polarized_pred_vars(current, visited).union(
                &self.premises.iter()
                    .flat_map(|p| p.get_polarized_pred_vars(current.invert(), visited))
                    .collect()
            ).cloned().collect()
        }
    }

    fn get_polarized_comp_terms(&self, current: Polarity, visited: &mut HashSet<MinlogPredicate>) -> HashSet<Polarized<Rc<MinlogPredicate>>> {
        if visited.contains(&MinlogPredicate::Implication(self.clone())) {
            HashSet::new()
        } else {
            visited.insert(MinlogPredicate::Implication(self.clone()));
            
            self.conclusion.get_polarized_comp_terms(current, visited).union(
                &self.premises.iter()
                    .flat_map(|p| p.get_polarized_comp_terms(current.invert(), visited))
                    .collect()
            ).cloned().collect()
        }
    }

    fn get_polarized_inductive_preds(&self, current: Polarity, visited: &mut HashSet<MinlogPredicate>) -> HashSet<Polarized<Rc<MinlogPredicate>>> {
        if visited.contains(&MinlogPredicate::Implication(self.clone())) {
            HashSet::new()
        } else {
            visited.insert(MinlogPredicate::Implication(self.clone()));
            
            self.conclusion.get_polarized_inductive_preds(current, visited).union(
                &self.premises.iter()
                    .flat_map(|p| p.get_polarized_inductive_preds(current.invert(), visited))
                    .collect()
            ).cloned().collect()
        }
    }

    fn get_polarized_prime_formulas(&self, current: Polarity, visited: &mut HashSet<MinlogPredicate>) -> HashSet<Polarized<Rc<MinlogPredicate>>> {
        if visited.contains(&MinlogPredicate::Implication(self.clone())) {
            HashSet::new()
        } else {
            visited.insert(MinlogPredicate::Implication(self.clone()));
            
            self.conclusion.get_polarized_prime_formulas(current, visited).union(
                &self.premises.iter()
                    .flat_map(|p| p.get_polarized_prime_formulas(current.invert(), visited))
                    .collect()
            ).cloned().collect()
        }
    }
    
    fn substitute(&self, from: &PredSubstEntry, to: &PredSubstEntry) -> Rc<MinlogPredicate> {
        if let Some(pred) = from.to_predicate() && pred.is_implication() && self == pred.to_implication().unwrap() {
            to.to_predicate().unwrap()
        } else {
            let new_premises: Vec<Rc<MinlogPredicate>> = self.premises.iter()
                .map(|p| p.substitute(from, to))
                .collect();
            
            let new_conclusion = self.conclusion.substitute(from, to);
            
            Implication::create(new_premises, new_conclusion)
        }
    }
    
    fn first_conflict_with(&self, other: &Rc<MinlogPredicate>) -> Option<(PredSubstEntry, PredSubstEntry)> {
        if let MinlogPredicate::Implication(other_implication) = other.as_ref() {
            if self.premises.len() != other_implication.premises.len() {
                return Some((Rc::new(MinlogPredicate::Implication(self.clone())).into(), other.clone().into()));
            }
            
            for (p1, p2) in self.premises.iter().zip(other_implication.premises.iter()) {
                if let Some(conflict) = p1.first_conflict_with(p2) {
                    return Some(conflict);
                }
            }
            
            self.conclusion.first_conflict_with(&other_implication.conclusion)
        } else {
            Some((Rc::new(MinlogPredicate::Implication(self.clone())).into(), other.clone().into()))
        }
    }
    
    fn match_with(&self, ctx: &mut impl MatchContext<PredSubstEntry>) -> MatchOutput<PredSubstEntry> {
        let pattern = ctx.next_pattern().unwrap();
        let instance = ctx.next_instance().unwrap();
        
        match (pattern, instance) {
            (PredSubstEntry::Predicate(p), PredSubstEntry::Predicate(i)) => {
                if !p.is_implication() || !i.is_implication() {
                    return MatchOutput::FailedMatch;
                }
                
                let imp_pattern = p.to_implication().unwrap();
                let imp_instance = i.to_implication().unwrap();
                
                if imp_pattern.premises.len() != imp_instance.premises.len() {
                    return MatchOutput::FailedMatch;
                }
                
                for (prem_pattern, prem_instance) in imp_pattern.premises.iter().zip(imp_instance.premises.iter()) {
                    if prem_pattern != prem_instance {
                        ctx.extend(&prem_pattern.clone().into(), &prem_instance.clone().into());
                    }
                }
                
                if imp_pattern.conclusion != imp_instance.conclusion {
                    ctx.extend(&imp_pattern.conclusion.clone().into(), &imp_instance.conclusion.clone().into());
                }
                
                MatchOutput::Matched
            },
            _ => MatchOutput::FailedMatch,
        }
    }
}

impl PrettyPrintable for Implication {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        if self.premises.is_empty() {
            return self.conclusion.to_pp_element(detail);
        }
        
        PPElement::list(
            self.premises.iter().map(|p| p.to_enclosed_pp_element(detail))
                .chain(std::iter::once(self.conclusion.to_enclosed_pp_element(detail))).collect(),
            PPElement::break_elem(1, 4, false),
            PPElement::text("=>".to_string()),
            PPElement::break_elem(1, 4, false),
            BreakType::Flexible
        )
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