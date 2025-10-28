
use std::{rc::Rc, collections::HashSet};
use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};

use crate::core::substitution::{MatchContext, MatchOutput};
use crate::core::polarity::{Polarity, Polarized};

use crate::core::types::minlog_type::MinlogType;
use crate::core::types::type_constant::TypeConstant;
use crate::core::types::arrow_type::ArrowType;

use crate::core::terms::minlog_term::MinlogTerm;

use crate::core::predicates::minlog_predicate::MinlogPredicate;

use crate::core::formulas::minlog_formula::{FormulaBody, MinlogFormula, FormulaOfNulltype};

use crate::core::predicates::predicate_substitution::{PredSubstEntry, PredicateSubstitution};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct AllQuantifier {
    vars: Vec<Rc<MinlogTerm>>,
    body: Rc<MinlogFormula>,
}

impl AllQuantifier {
    pub fn create(vars: Vec<Rc<MinlogTerm>>, body: Rc<MinlogFormula>) -> Rc<MinlogFormula> {
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
        
        AllQuantifier::collapse(&Rc::new(MinlogFormula::AllQuantifier(AllQuantifier { vars, body })))
    }
    
    pub fn collapse(minlog_formula: &Rc<MinlogFormula>) -> Rc<MinlogFormula> {
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
    
    pub fn closure(minlog_formula: &Rc<MinlogFormula>) -> Rc<MinlogFormula> {
        let free_vars: Vec<Rc<MinlogTerm>> = minlog_formula.get_free_variables().into_iter().collect();
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
    
    pub fn body(&self) -> &Rc<MinlogFormula> {
        &self.body
    }
}

impl FormulaBody for AllQuantifier {
    fn of_nulltype(&self) -> FormulaOfNulltype {
        FormulaOfNulltype {
            positive_nulltype: self.body.of_nulltype().positive_nulltype,
            negative_nulltype: false,
        }
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> Rc<MinlogFormula> {
        let normalized_body = self.body.normalize(eta, pi);
        AllQuantifier::create(self.vars.clone(), normalized_body)
    }
    
    fn depth(&self) -> usize {
        1 + self.body.depth()
    }
    
    fn extracted_type(&self) -> Rc<MinlogType> {
        let var_types = self.vars.iter().map(|v| {
            if v.totality(&mut HashSet::new()).is_total() {
                v.minlog_type()
            } else {
                TypeConstant::create_null()
            }
        }).collect::<Vec<_>>();
        
        ArrowType::create(var_types, self.body.extracted_type())
            .remove_nulls().unwrap_or(TypeConstant::create_null())
    }
    
    fn get_type_variables(&self) -> HashSet<Rc<MinlogType>> {
        self.vars.iter()
            .flat_map(|v| v.get_type_variables())
            .chain(self.body.get_type_variables())
            .collect()
    }
    
    fn get_algebra_types(&self) -> HashSet<Rc<MinlogType>> {
        self.vars.iter()
            .flat_map(|v| v.get_algebra_types())
            .chain(self.body.get_algebra_types())
            .collect()
    }
    
    fn get_free_variables(&self) -> HashSet<Rc<MinlogTerm>> {
        self.body.get_free_variables().into_iter()
            .filter(|v| !self.vars.contains(v))
            .collect()
    }
    
    fn get_bound_variables(&self) -> HashSet<Rc<MinlogTerm>> {
        self.vars.iter().cloned()
            .chain(self.body.get_bound_variables())
            .collect()
    }
    
    fn get_polarized_pred_vars(&self, current: Polarity) -> HashSet<Polarized<Rc<MinlogPredicate>>> {
        self.body.get_polarized_pred_vars(current)
    }
    
    fn get_polarized_comp_terms(&self, current: Polarity) -> HashSet<Polarized<Rc<MinlogPredicate>>> {
        self.body.get_polarized_comp_terms(current)
    }
    
    fn get_polarized_inductive_preds(&self, current: Polarity) -> HashSet<Polarized<Rc<MinlogPredicate>>> {
        self.body.get_polarized_inductive_preds(current)
    }
    
    fn get_polarized_prime_formulas(&self, current: Polarity) -> HashSet<Polarized<Rc<MinlogFormula>>> {
        self.body.get_polarized_prime_formulas(current)
    }
    
    fn substitute(&self, from: &PredSubstEntry, to: &PredSubstEntry) -> Rc<MinlogFormula> {
        if let Some(tm) = from.to_term() && self.vars.contains(&tm) {
            Rc::new(MinlogFormula::AllQuantifier(self.clone()))
        } else if let Some(formula) = from.to_formula() && formula.is_all_quantifier() && self == formula.to_all_quantifier().unwrap() {
            to.to_formula().unwrap()
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
    
    fn first_conflict_with(&self, other: &Rc<MinlogFormula>) -> Option<(PredSubstEntry, PredSubstEntry)> {
        if let MinlogFormula::AllQuantifier(other_aq) = other.as_ref() {
            if self.vars.len() != other_aq.vars.len() {
                return Some((Rc::new(MinlogFormula::AllQuantifier(self.clone())).into(), other.clone().into()));
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
            self.body.first_conflict_with(&substituted_body.to_formula().unwrap())
        } else {
            Some((Rc::new(MinlogFormula::AllQuantifier(self.clone())).into(), other.clone().into()))
        }
    }
    
    fn match_with(&self, ctx: &mut impl MatchContext<PredSubstEntry>) -> MatchOutput<PredSubstEntry> {
        let pattern = ctx.next_pattern().unwrap();
        let instance = ctx.next_instance().unwrap();
        
        match (pattern, instance) {
            (PredSubstEntry::Formula(p), PredSubstEntry::Formula(i)) => {
                if !p.is_all_quantifier() || !i.is_all_quantifier() {
                    return MatchOutput::FailedMatch;
                }
                
                let aq_pattern = p.to_all_quantifier().unwrap();
                let aq_instance = i.to_all_quantifier().unwrap();
                
                if aq_pattern.vars.len() != aq_instance.vars.len() {
                    return MatchOutput::FailedMatch;
                }
                
                for (v1, v2) in aq_pattern.vars.iter().zip(aq_instance.vars.iter()) {
                    if v1 != v2 {
                        ctx.extend(&v1.clone().into(), &v2.clone().into());
                    }
                }
                
                if aq_pattern.body != aq_instance.body {
                    ctx.extend(&aq_pattern.body.clone().into(), &aq_instance.body.clone().into());
                }
                
                MatchOutput::Matched
            },
            _ => MatchOutput::FailedMatch,
        }
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