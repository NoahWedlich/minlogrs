
use std::rc::Rc;

use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};

use crate::core::substitution::{MatchContext, MatchOutput};
use crate::core::polarity::{Polarity, Polarized};

use crate::core::types::minlog_type::MinlogType;
use crate::core::terms::minlog_term::MinlogTerm;
use crate::core::formulas::minlog_formula::MinlogFormula;

use crate::core::predicates::minlog_predicate::{MinlogPredicate, PredicateBody, PredicateDegree};

use crate::core::predicates::predicate_substitution::{PredicateSubstitution, PredSubstEntry};

#[derive(Clone, PartialEq, Eq)]
pub struct ComprehensionTerm {
    vars: Vec<Rc<MinlogTerm>>,
    body: Rc<MinlogFormula>,
    arity: Vec<Rc<MinlogType>>,
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
        
        let arity = vars.iter().map(|v| v.minlog_type()).collect();
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
    fn arity(&self) -> Vec<Rc<MinlogType>> {
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
    
    fn get_type_variables(&self) -> Vec<Rc<MinlogType>> {
        let mut inner = self.body.get_type_variables();
        
        for var in &self.vars {
            for tvar in var.get_type_variables() {
                if !inner.contains(&tvar) {
                    inner.push(tvar);
                }
            }
        }
        
        inner
    }

    fn get_algebra_types(&self) -> Vec<Rc<MinlogType>> {
        let mut inner = self.body.get_algebra_types();

        for var in &self.vars {
            for tvar in var.get_algebra_types() {
                if !inner.contains(&tvar) {
                    inner.push(tvar);
                }
            }
        }

        inner
    }

    fn get_free_variables(&self) -> Vec<Rc<MinlogTerm>> {
        let mut inner = self.body.get_free_variables();
        
        inner.retain(|v| !self.vars.contains(v));
        
        inner
    }
    
    fn get_bound_variables(&self) -> Vec<Rc<MinlogTerm>> {
        let mut bound_vars = self.body.get_bound_variables();
        
        for var in &self.vars {
            if !bound_vars.contains(var) {
                bound_vars.push(var.clone());
            }
        }
        
        bound_vars
    }
    
    fn get_polarized_pred_vars(&self, current: Polarity) -> Vec<Polarized<Rc<MinlogPredicate>>> {
        self.body.get_polarized_pred_vars(current)
    }

    fn get_polarized_comp_terms(&self, current: Polarity) -> Vec<Polarized<Rc<MinlogPredicate>>> {
        let mut inner = self.body.get_polarized_comp_terms(current);
        
        if inner.iter().all(|p| p.value.to_comprehension_term().unwrap() != self) {
            inner.push(Polarized::new(current, Rc::new(MinlogPredicate::Comprehension(self.clone()))));
        }
        
        inner
    }
    
    fn get_polarized_inductive_preds(&self, current: Polarity) -> Vec<Polarized<Rc<MinlogPredicate>>> {
        self.body.get_polarized_inductive_preds(current)
    }
    
    fn get_polarized_prime_formulas(&self, current: Polarity) -> Vec<Polarized<Rc<MinlogFormula>>> {
        self.body.get_polarized_prime_formulas(current)
    }
    
    fn substitute(&self, from: &PredSubstEntry, to: &PredSubstEntry) -> Rc<MinlogPredicate> {
        if let Some(tm) = from.to_term() && self.vars.contains(&tm) {
            return Rc::new(MinlogPredicate::Comprehension(self.clone()));
        }
        
        let new_vars = if let Some(tse) = from.to_term_subst_entry() {
            self.vars.iter()
                .map(|v| v.substitute(&tse, &to.to_term_subst_entry().unwrap()))
                .collect()
        } else {
            self.vars.clone()
        };
        
        let new_body = self.body.substitute(from, to);
        
        ComprehensionTerm::create(new_vars, new_body)
    }
    
    fn first_conflict_with(&self, other: &Rc<MinlogPredicate>) -> Option<(PredSubstEntry, PredSubstEntry)> {
        if let Some(conflict) = self.arity.iter().find_map(|t| {
            other.arity().iter().find_map(|ot| t.first_conflict_with(ot))
        }) {
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
                
                for (pt, it) in p.arity().iter().zip(i.arity().iter()) {
                    if pt != it {
                        ctx.extend(&PredSubstEntry::Type(pt.clone()), &PredSubstEntry::Type(it.clone()));
                    }
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