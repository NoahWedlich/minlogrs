
use std::{rc::Rc, collections::HashSet};

use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};

use crate::core::substitution::{MatchContext, MatchOutput};
use crate::core::polarity::{Polarity, Polarized};

use crate::core::types::minlog_type::MinlogType;
use crate::core::types::tuple_type::TupleType;

use crate::core::terms::minlog_term::MinlogTerm;
use crate::core::formulas::minlog_formula::MinlogFormula;

use crate::core::predicates::minlog_predicate::{MinlogPredicate, PredicateBody, PredicateDegree};

use crate::core::predicates::predicate_substitution::{PredicateSubstitution, PredSubstEntry};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ComprehensionTerm {
    vars: Vec<Rc<MinlogTerm>>,
    body: Rc<MinlogFormula>,
    arity: Rc<MinlogType>,
}

impl ComprehensionTerm {
    pub fn create(vars: Vec<Rc<MinlogTerm>>, body: Rc<MinlogFormula>) -> Rc<MinlogPredicate> {
        if vars.iter().any(|v| !v.is_variable()) {
            panic!("Tried to create comprehension term with non-variable bound terms");
        }
        
        for (i, var) in vars.iter().enumerate() {
            for other in &vars[i+1..] {
                if var == other {
                    panic!("Tried to create comprehension term with duplicate bound variable '{}'", var.debug_string());
                }
            }
        }
        
        let arity = TupleType::create(vars.iter().map(|v| v.minlog_type()).collect());
        Rc::new(MinlogPredicate::Comprehension(ComprehensionTerm { vars, body, arity }))
    }
    
    pub fn closure(minlog_formula: &Rc<MinlogFormula>) -> Rc<MinlogPredicate> {
        let mut vars = vec![];
        for free_var in minlog_formula.get_free_variables() {
            if free_var.is_variable() && !vars.contains(&free_var) {
                vars.push(free_var);
            }
        }
        
        ComprehensionTerm::create(vars, minlog_formula.clone())
    }
    
    pub fn vars(&self) -> &Vec<Rc<MinlogTerm>> {
        &self.vars
    }
    
    pub fn var(&self, index: usize) -> Option<&Rc<MinlogTerm>> {
        self.vars.get(index)
    }
    
    pub fn body(&self) -> &Rc<MinlogFormula> {
        &self.body
    }
}

impl PredicateBody for ComprehensionTerm {
    fn arity(&self) -> Rc<MinlogType> {
        self.arity.clone()
    }
    
    fn degree(&self) -> PredicateDegree {
        let body_of_nulltype = self.body.of_nulltype();
        PredicateDegree {
            positive_content: body_of_nulltype.positive_nulltype,
            negative_content: body_of_nulltype.negative_nulltype,
        }
    }
    
    fn depth(&self) -> usize {
        self.body.depth() + 1
    }
    
    fn extracted_type(&self) -> Rc<MinlogType> {
        self.body.extracted_type()
    }
    
    fn get_type_variables(&self) -> HashSet<Rc<MinlogType>> {
        self.vars.iter().flat_map(|v| v.get_type_variables()).chain(
            self.body.get_type_variables()
        ).collect()
    }

    fn get_algebra_types(&self) -> HashSet<Rc<MinlogType>> {
        self.vars.iter().flat_map(|v| v.get_algebra_types()).chain(
            self.body.get_algebra_types()
        ).collect()
    }

    fn get_free_variables(&self) -> HashSet<Rc<MinlogTerm>> {
        self.body.get_free_variables().difference(&self.vars.iter().cloned().collect()).cloned().collect()
    }
    
    fn get_bound_variables(&self) -> HashSet<Rc<MinlogTerm>> {
        self.body.get_bound_variables().union(&self.vars.iter().cloned().collect()).cloned().collect()
    }
    
    fn get_polarized_pred_vars(&self, current: Polarity) -> HashSet<Polarized<Rc<MinlogPredicate>>> {
        self.body.get_polarized_pred_vars(current)
    }

    fn get_polarized_comp_terms(&self, current: Polarity) -> HashSet<Polarized<Rc<MinlogPredicate>>> {
        let mut result = self.body.get_polarized_comp_terms(current);
        result.insert(Polarized::new(current, Rc::new(MinlogPredicate::Comprehension(self.clone()))));
        result
    }
    
    fn get_polarized_inductive_preds(&self, current: Polarity) -> HashSet<Polarized<Rc<MinlogPredicate>>> {
        self.body.get_polarized_inductive_preds(current)
    }
    
    fn get_polarized_prime_formulas(&self, current: Polarity) -> HashSet<Polarized<Rc<MinlogFormula>>> {
        self.body.get_polarized_prime_formulas(current)
    }
    
    fn substitute(&self, from: &PredSubstEntry, to: &PredSubstEntry) -> Rc<MinlogPredicate> {
        match from {
            PredSubstEntry::Type(_) => {
                let new_vars = self.vars.iter()
                    .map(|v| v.substitute(&from.to_term_subst_entry().unwrap(), &to.to_term_subst_entry().unwrap()))
                    .collect();
                
                let new_body = self.body.substitute(from, to);
                
                ComprehensionTerm::create(new_vars, new_body)
            },
            PredSubstEntry::Term(from_tm) => {
                if from_tm.is_variable() && self.vars.contains(from_tm) {
                    Rc::new(MinlogPredicate::Comprehension(self.clone()))
                } else {
                    let new_vars = self.vars.iter()
                        .map(|v| v.substitute(&from.to_term_subst_entry().unwrap(), &to.to_term_subst_entry().unwrap()))
                        .collect();
                    
                    let new_body = self.body.substitute(from, to);
                    
                    ComprehensionTerm::create(new_vars, new_body)
                }
            },
            PredSubstEntry::Predicate(from_p) => {
                if from_p.is_comprehension_term() && self == from_p.to_comprehension_term().unwrap() {
                    to.to_predicate().unwrap()
                } else {
                    let new_body = self.body.substitute(from, to);
                    ComprehensionTerm::create(self.vars.clone(), new_body)
                }
            },
            PredSubstEntry::Formula(_) => {
                let new_body = self.body.substitute(from, to);
                ComprehensionTerm::create(self.vars.clone(), new_body)
            }
        }
    }
    
    fn first_conflict_with(&self, other: &Rc<MinlogPredicate>) -> Option<(PredSubstEntry, PredSubstEntry)> {
        if let Some(conflict) = self.arity.first_conflict_with(&other.arity()) {
            return Some((conflict.0.into(), conflict.1.into()));
        }
        
        if let Some(other_cterm) = other.to_comprehension_term() {
            if self.vars.len() != other_cterm.vars.len() {
                return Some((Rc::new(MinlogPredicate::Comprehension(self.clone())).into(), other.clone().into()));
            }

            let mut subst = PredicateSubstitution::make_empty();
            
            for (v1, v2) in self.vars.iter().zip(other_cterm.vars.iter()) {
                if v1 == v2 {
                    continue;
                } else if v1.minlog_type() == v2.minlog_type() {
                    subst.extend((v2.clone().into(), v1.clone().into()));
                } else {
                    return Some((v1.clone().into(), v2.clone().into()));
                }
            }
            
            let substituted_body = subst.substitute::<PredSubstEntry>(&other_cterm.body().into());
            self.body.first_conflict_with(&substituted_body.to_formula().unwrap())
        } else {
            Some((Rc::new(MinlogPredicate::Comprehension(self.clone())).into(), other.clone().into()))
        }
    }
    
    fn match_with(&self, ctx: &mut impl MatchContext<PredSubstEntry>) -> MatchOutput<PredSubstEntry> {
        let pattern = ctx.next_pattern().unwrap();
        let instance = ctx.next_instance().unwrap();
        
        match (pattern, instance) {
            (PredSubstEntry::Predicate(p), PredSubstEntry::Predicate(i)) => {
                if !p.is_comprehension_term() || !i.is_comprehension_term() {
                    return MatchOutput::FailedMatch;
                }
                
                if p.arity() != i.arity() {
                    ctx.extend(&p.arity().clone().into(), &i.arity().clone().into());
                }
                
                let cterm_pattern = p.to_comprehension_term().unwrap();
                let cterm_instance = i.to_comprehension_term().unwrap();
                
                if cterm_pattern.vars.len() != cterm_instance.vars.len() {
                    return MatchOutput::FailedMatch;
                }
                
                for (pvar, ivar) in cterm_pattern.vars.iter().zip(cterm_instance.vars.iter()) {
                    if pvar != ivar {
                        ctx.extend(&PredSubstEntry::Term(pvar.clone()), &PredSubstEntry::Term(ivar.clone()));
                    }
                }
                
                if cterm_pattern.body != cterm_instance.body {
                    ctx.extend(&cterm_pattern.body.clone().into(), &cterm_instance.body.clone().into());
                }
                
                MatchOutput::Matched
            },
            _ => MatchOutput::FailedMatch,
        }
    }
}

impl PrettyPrintable for ComprehensionTerm {
    fn to_pp_element(&self, detail: bool) -> PPElement {        
        let variables = PPElement::list(
            self.vars.iter().map(|v| v.to_pp_element(detail)).collect(),
            PPElement::break_elem(0, 0, false),
            PPElement::text(",".to_string()),
            PPElement::break_elem(1, 0, false),
            BreakType::Flexible,
        );
        
        PPElement::group(vec![
            PPElement::text("{".to_string()),
            PPElement::break_elem(1, 4, false),
            variables,
            PPElement::break_elem(1, 0, false),
            PPElement::text("|".to_string()),
            PPElement::break_elem(1, 4, false),
            self.body.to_pp_element(detail),
            PPElement::break_elem(1, 0, false),
            PPElement::text("}".to_string()),
        ], BreakType::Consistent, 0)
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}