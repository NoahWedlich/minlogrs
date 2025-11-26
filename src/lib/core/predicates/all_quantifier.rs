use indexmap::{IndexMap, IndexSet};
use std::rc::Rc;

use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};

use crate::core::substitution::MatchOutput;
use crate::core::polarity::{Polarity, Polarized};

use crate::core::types::minlog_type::MinlogType;

use crate::core::terms::minlog_term::MinlogTerm;
use crate::core::terms::term_substitution::TermSubstitution;

use crate::core::predicates::minlog_predicate::{MinlogPredicate, PredicateBody};

use crate::core::predicates::predicate_substitution::{PredicateSubstitution, PredSubstEntry};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct AllQuantifier {
    vars: Vec<Rc<MinlogTerm>>,
    body: Rc<MinlogPredicate>,
}

impl AllQuantifier {
    pub fn create(vars: Vec<Rc<MinlogTerm>>, body: Rc<MinlogPredicate>) -> Rc<MinlogPredicate> {
        let vars = vars.into_iter()
            .filter(|v| !v.is_tuple() || !v.to_tuple().unwrap().elements().is_empty())
            .collect::<Vec<Rc<MinlogTerm>>>();
        
        if vars.is_empty() {
            return body;
        }
        
        if vars.iter().any(|v| !v.is_variable()) {
            panic!("AllQuantifier can only quantify over variable terms");
        }
        
        for (i, v) in vars.iter().enumerate() {
            for v2 in vars.iter().skip(i + 1) {
                if v == v2 {
                    panic!("AllQuantifier cannot quantify over the same variable multiple times");
                }
            }
        }
        
        AllQuantifier::collapse(&Rc::new(MinlogPredicate::AllQuantifier(AllQuantifier { vars, body })))
    }
    
    pub fn collapse(minlog_formula: &Rc<MinlogPredicate>) -> Rc<MinlogPredicate> {
        if !minlog_formula.is_all_quantifier() || !minlog_formula.to_all_quantifier().unwrap().body.is_all_quantifier() {
            minlog_formula.clone()
        } else {
            let mut aq = minlog_formula.to_all_quantifier().unwrap();
            let mut vars = aq.vars().clone();
            
            while aq.body.is_all_quantifier() {
                let next_aq = aq.body.to_all_quantifier().unwrap();
                vars.extend(next_aq.vars().clone());
                aq = next_aq;
            }
            
            AllQuantifier::create(vars, aq.body().clone())
        }
    }
    
    pub fn closure(minlog_formula: &Rc<MinlogPredicate>) -> Rc<MinlogPredicate> {
        let free_vars: Vec<Rc<MinlogTerm>> = minlog_formula.get_free_variables(&mut IndexSet::new()).into_iter().collect();
        if free_vars.is_empty() {
            minlog_formula.clone()
        } else {
            AllQuantifier::create(free_vars, minlog_formula.clone())
        }
    }
    
    pub fn vars(&self) -> &Vec<Rc<MinlogTerm>> {
        &self.vars
    }
    
    pub fn var(&self, index: usize) -> Option<&Rc<MinlogTerm>> {
        self.vars.get(index)
    }
    
    pub fn body(&self) -> &Rc<MinlogPredicate> {
        &self.body
    }
}

impl PredicateBody for AllQuantifier {
    fn arity(&self) -> Rc<MinlogType> {
        self.body.arity()
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> Rc<MinlogPredicate> {
        let normalized_body = self.body.normalize(eta, pi);
        AllQuantifier::create(self.vars.clone(), normalized_body)
    }
    
    fn depth(&self) -> usize {
        1 + self.body.depth()
    }
    
    fn extracted_type_pattern(&self) -> Rc<MinlogType> {
        self.body.extracted_type_pattern()
    }
    
    fn extracted_type(&self) -> Rc<MinlogType> {
        self.body.extracted_type()
    }
    
    fn et_pattern_to_et(&self) -> TermSubstitution {
        self.body.et_pattern_to_et()
    }
    
    fn get_type_variables(&self, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Rc<MinlogType>> {
        self.vars.iter()
            .flat_map(|v| v.get_type_variables(&mut IndexSet::new()))
            .chain(self.body.get_type_variables(visited))
            .collect()
    }
    
    fn get_algebra_types(&self, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Rc<MinlogType>> {
        self.vars.iter()
            .flat_map(|v| v.get_algebra_types(&mut IndexSet::new()))
            .chain(self.body.get_algebra_types(visited))
            .collect()
    }
    
    fn get_free_variables(&self, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Rc<MinlogTerm>> {
        self.body.get_free_variables(visited).into_iter()
            .filter(|v| !self.vars.contains(v))
            .collect()
    }
    
    fn get_bound_variables(&self, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Rc<MinlogTerm>> {
        self.vars.iter().cloned()
            .chain(self.body.get_bound_variables(visited))
            .collect()
    }
    
    fn get_polarized_pred_vars(&self, current: Polarity, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Polarized<Rc<MinlogPredicate>>> {
        self.body.get_polarized_pred_vars(current, visited)
    }
    
    fn get_polarized_comp_terms(&self, current: Polarity, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Polarized<Rc<MinlogPredicate>>> {
        self.body.get_polarized_comp_terms(current, visited)
    }
    
    fn get_polarized_inductive_preds(&self, current: Polarity, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Polarized<Rc<MinlogPredicate>>> {
        self.body.get_polarized_inductive_preds(current, visited)
    }
    
    fn get_polarized_prime_formulas(&self, current: Polarity, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Polarized<Rc<MinlogPredicate>>> {
        self.body.get_polarized_prime_formulas(current, visited)
    }
    
    fn substitute(&self, from: &PredSubstEntry, to: &PredSubstEntry) -> Rc<MinlogPredicate> {
        if let Some(tm) = from.to_term() && self.vars.contains(&tm) {
            Rc::new(MinlogPredicate::AllQuantifier(self.clone()))
        } else if let Some(pred) = from.to_predicate() && pred.is_all_quantifier() && self == pred.to_all_quantifier().unwrap() {
            to.to_predicate().unwrap()
        } else {
            let new_vars = if let Some(tse) = from.to_term_subst_entry() {
                self.vars.iter()
                    .map(|v| v.substitute(&tse, &to.to_term_subst_entry().unwrap()))
                    .collect()
            } else {
                self.vars.clone()
            };
            
            let new_body = self.body.substitute(from, to);
            
            AllQuantifier::create(new_vars, new_body)
        }
    }
    
    fn first_conflict_with(&self, other: &Rc<MinlogPredicate>) -> Option<(PredSubstEntry, PredSubstEntry)> {
        if let MinlogPredicate::AllQuantifier(other_aq) = other.as_ref() {
            if self.vars.len() != other_aq.vars.len() {
                return Some((Rc::new(MinlogPredicate::AllQuantifier(self.clone())).into(), other.clone().into()));
            }
            
            let mut subst = PredicateSubstitution::make_empty();
            
            for (v1, v2) in self.vars.iter().zip(other_aq.vars.iter()) {
                if v1 == v2 {
                    continue;
                } else if v1.minlog_type() == v2.minlog_type() {
                    subst.extend((v2.clone().into(), v1.clone().into()));
                } else {
                    return Some((v1.clone().into(), v2.clone().into()));
                }
            }
            
            let substituted_body = subst.substitute::<PredSubstEntry>(&other_aq.body().into());
            self.body.first_conflict_with(&substituted_body.to_predicate().unwrap())
        } else {
            Some((Rc::new(MinlogPredicate::AllQuantifier(self.clone())).into(), other.clone().into()))
        }
    }
    
    fn match_with(&self, instance: &Rc<MinlogPredicate>) -> MatchOutput<PredSubstEntry> {
        if !instance.is_all_quantifier() {
            return MatchOutput::FailedMatch;
        }
        
        let aq_instance = instance.to_all_quantifier().unwrap();
        
        if self.vars.len() != aq_instance.vars.len() {
            return MatchOutput::FailedMatch;
        }
        
        let mut conditions = self.vars.iter().zip(aq_instance.vars.iter())
            .filter_map(|(v1, v2)| {
                if v1 != v2 {
                    Some((v1.clone().into(), v2.clone().into()))
                } else {
                    None
                }
            })
            .collect::<IndexMap<_, _>>();
        
        if self.body != aq_instance.body {
            conditions.insert(self.body.clone().into(), aq_instance.body.clone().into());
        }
        
        MatchOutput::Matched(conditions)
    }
}

impl PrettyPrintable for AllQuantifier {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        let variables = PPElement::list(
            self.vars.iter().map(|v| v.to_pp_element(detail)).collect(),
            PPElement::break_elem(0, 0, false),
            PPElement::text(",".to_string()),
            PPElement::break_elem(1, 0, false),
            BreakType::Flexible,
        );
        
        PPElement::group(vec![
            PPElement::text("all".to_string()),
            PPElement::break_elem(1, 4, false),
            variables,
            PPElement::break_elem(0, 4, false),
            PPElement::text(":".to_string()),
            PPElement::break_elem(1, 4, false),
            self.body.to_pp_element(detail),
        ], BreakType::Flexible, 0)
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