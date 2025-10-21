
use std::{rc::Rc, cell::RefCell};
use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};

use crate::core::substitution::{MatchContext, MatchOutput};
use crate::core::polarity::{Polarity, Polarized};

use crate::core::types::minlog_type::MinlogType;
use crate::core::types::type_constant::TypeConstant;
use crate::core::types::algebra_type::AlgebraType;

use crate::core::terms::minlog_term::MinlogTerm;
use crate::core::predicates::minlog_predicate::{PredicateBody, MinlogPredicate, PredicateDegree};

use crate::core::formulas::minlog_formula::MinlogFormula;

use crate::core::types::type_substitution::TypeSubstitution;
use crate::core::predicates::predicate_substitution::{PredSubstEntry, PredicateSubstitution};

use crate::core::structures::inductive_constant::InductiveConstant;

#[derive(Clone, PartialEq, Eq)]
pub struct InductivePredicate {
    definition: Rc<InductiveConstant>,
    params: PredicateSubstitution,
    blocked_collection: RefCell<bool>,
}

impl InductivePredicate {
    pub fn create(definition: Rc<InductiveConstant>, params: PredicateSubstitution) -> Rc<MinlogPredicate> {
        let expected_tvars = definition.get_type_variables();
        let expected_tmvars = definition.get_term_variables();
        let expected_pvars = definition.get_predicate_variables();
        
        for (from, _) in params.pairs() {
            match from {
                PredSubstEntry::Type(tv) => {
                    if !expected_tvars.contains(tv) {
                        panic!("Type variable '{}' not expected in parameters for inductive predicate '{}'", tv.debug_string(), definition.name());
                    }
                },
                PredSubstEntry::Term(tv) => {
                    if !expected_tmvars.contains(tv) {
                        panic!("Term variable '{}' not expected in parameters for inductive predicate '{}'", tv.debug_string(), definition.name());
                    }
                },
                PredSubstEntry::Predicate(pv) => {
                    if !expected_pvars.contains(pv) {
                        panic!("Predicate variable '{}' not expected in parameters for inductive predicate '{}'", pv.debug_string(), definition.name());
                    }
                },
                _ => {
                    panic!("Invalid parameter type for inductive predicate '{}'", definition.name());
                }
            }
        }
        
        Rc::new(MinlogPredicate::InductivePredicate(InductivePredicate {
            definition,
            params,
            blocked_collection: RefCell::new(false)
        }))
    }
    
    pub fn definition(&self) -> &Rc<InductiveConstant> {
        &self.definition
    }
    
    pub fn params(&self) -> &PredicateSubstitution {
        &self.params
    }
    
    pub fn name(&self) -> &String {
        self.definition.name()
    }
    
    pub fn clauses(&self) -> Vec<(String, Rc<MinlogFormula>)> {
        self.definition.clauses().iter()
            .map(|(name, body)| (
                name.clone(),
                self.params.substitute::<PredSubstEntry>(&body.into()).to_formula().unwrap()
            )).collect()
    }
    
    pub fn get_algebra(&self) -> Option<Rc<MinlogType>> {
        let type_subst = TypeSubstitution::from_pairs(
            self.params.pairs().iter().filter_map(|(from, to)| {
                if let PredSubstEntry::Type(tv) = from {
                    Some((tv.clone(), to.to_type().unwrap().clone()))
                } else {
                    None
                }
            }).collect()
        );
        
        self.definition.get_algebra().map(|alg| {
            AlgebraType::create(alg, type_subst.pairs().clone())
        })
    }
    
    pub fn references_idp(&self, idp: &Rc<MinlogPredicate>) -> bool {
        if self.clauses().iter().any(|(_, body)| body.contains_inductive_predicate(idp)) {
            return true;
        }
        
        self.params.pairs().iter().any(|(_, to)| {
            if let PredSubstEntry::Predicate(pred) = to {
                pred.contains_inductive_predicate(idp)
            } else if let PredSubstEntry::Formula(fm) = to {
                fm.contains_inductive_predicate(idp)
            } else {
                false
            }
        })
    }
    
    pub fn ensure_well_founded(&self) {
        let self_idp_pred = Rc::new(MinlogPredicate::InductivePredicate(self.clone()));
        
        for (_, body) in self.clauses().iter() {
            if let Some(implication) = body.to_implication() {
                for premise in implication.premises().iter() {
                    let polarized_args = premise.get_polarized_inductive_preds(Polarity::StrictlyPositive);
                    for polarized_arg in polarized_args.iter() {
                        if !polarized_arg.polarity.is_strictly_positive() &&
                            polarized_arg.value.to_inductive_predicate().unwrap().references_idp(&self_idp_pred) {
                            panic!("Inductive predicate '{}' is not well-founded due to negative occurrence in clause body '{}'",
                            self.definition.name(), body.debug_string());
                        }
                    }
                }
            }
        }
    }
}

impl PredicateBody for InductivePredicate {
    fn arity(&self) -> Vec<Rc<MinlogType>> {
        self.definition.arity().iter()
            .map(|t| self.params.substitute::<PredSubstEntry>(&t.into()).to_type().unwrap())
            .collect()
    }
    
    fn degree(&self) -> PredicateDegree {
        if !self.definition.is_computational() && !self.definition.clauses().len() <= 1 {
            PredicateDegree{
                positive_content: false,
                negative_content: true
            }
        } else {
            PredicateDegree{
                positive_content: true,
                negative_content: true
            }
        }
    }
    
    fn depth(&self) -> usize {
        1 + self.params.pairs().iter()
            .map(|(_, to)| match to {
                PredSubstEntry::Term(tmv) => tmv.depth(),
                PredSubstEntry::Predicate(pv) => pv.depth(),
                PredSubstEntry::Formula(fm) => fm.depth(),
                _ => 0,
            }).chain(self.clauses().iter().map(|(_, body)| body.depth()))
            .max().unwrap_or(0)
    }
    
    fn extracted_type(&self) -> Rc<MinlogType> {
        if let Some(alg) = self.get_algebra() {
            alg
        } else {
            TypeConstant::create_null()
        }
    }
    
    fn get_type_variables(&self) -> Vec<Rc<MinlogType>> {
        self.definition.get_type_variables().iter()
            .flat_map(|tv| {
                self.params.substitute::<PredSubstEntry>(&tv.into()).to_type().unwrap().get_type_variables()
            }).collect()
    }
    
    fn get_algebra_types(&self) -> Vec<Rc<MinlogType>> {
        self.definition.get_type_variables().iter()
            .flat_map(|tv| {
                self.params.substitute::<PredSubstEntry>(&tv.into()).to_type().unwrap().get_algebra_types()
            }).chain(
                self.clauses().iter().flat_map(|(_, body)| body.get_algebra_types())
            ).collect()
    }
    
    fn get_free_variables(&self) -> Vec<Rc<MinlogTerm>> {
        self.definition.get_term_variables().iter()
            .flat_map(|tv| {
                self.params.substitute::<PredSubstEntry>(&tv.into()).to_term().unwrap().get_free_variables()
            }).chain(
                self.clauses().iter().flat_map(|(_, body)| body.get_free_variables())
            ).collect()
    }
    
    fn get_bound_variables(&self) -> Vec<Rc<MinlogTerm>> {
        self.definition.get_term_variables().iter()
            .flat_map(|tv| {
                self.params.substitute::<PredSubstEntry>(&tv.into()).to_term().unwrap().get_bound_variables()
            }).chain(
                self.clauses().iter().flat_map(|(_, body)| body.get_bound_variables())
            ).collect()
    }
    
    fn get_polarized_pred_vars(&self, current:Polarity) -> Vec<Polarized<Rc<MinlogPredicate>>> {
        if *self.blocked_collection.borrow() {
            return vec![];
        }

        *self.blocked_collection.borrow_mut() = true;

        let result = self.definition.get_predicate_variables().iter()
            .flat_map(|pv| {
                self.params.substitute::<PredSubstEntry>(&pv.into()).to_predicate().unwrap()
                    .get_polarized_pred_vars(current)
            }).chain(
                self.clauses().iter().flat_map(|(_, body)| body.get_polarized_pred_vars(current))
            ).collect();
            
        *self.blocked_collection.borrow_mut() = false;
        
        result
    }
    
    fn get_polarized_comp_terms(&self, current:Polarity) -> Vec<Polarized<Rc<MinlogPredicate>>> {
        if *self.blocked_collection.borrow() {
            return vec![];
        }
        
        *self.blocked_collection.borrow_mut() = true;

        let result = self.definition.get_predicate_variables().iter()
            .flat_map(|pv| {
                self.params.substitute::<PredSubstEntry>(&pv.into()).to_predicate().unwrap()
                    .get_polarized_comp_terms(current)
            }).chain(
                self.clauses().iter().flat_map(|(_, body)| body.get_polarized_comp_terms(current))
            ).collect();
            
        *self.blocked_collection.borrow_mut() = false;
        
        result
    }
    
    fn get_polarized_inductive_preds(&self, current:Polarity) -> Vec<Polarized<Rc<MinlogPredicate>>> {
        if *self.blocked_collection.borrow() {
            return vec![];
        }

        *self.blocked_collection.borrow_mut() = true;

        let mut results = self.definition.get_predicate_variables().iter()
            .flat_map(|pv| {
                self.params.substitute::<PredSubstEntry>(&pv.into()).to_predicate().unwrap()
                    .get_polarized_inductive_preds(current)
            }).chain(
                self.clauses().iter().flat_map(|(_, body)| body.get_polarized_inductive_preds(current))
            ).collect::<Vec<_>>();
            
        results.push(Polarized::new(current, Rc::new(MinlogPredicate::InductivePredicate(self.clone()))));

        *self.blocked_collection.borrow_mut() = false;

        results
    }
    
    fn get_polarized_prime_formulas(&self, current:Polarity) -> Vec<Polarized<Rc<MinlogFormula>>> {
        if *self.blocked_collection.borrow() {
            return vec![];
        }
        
        *self.blocked_collection.borrow_mut() = true;

        let result = self.definition.get_predicate_variables().iter()
            .flat_map(|pv| {
                self.params.substitute::<PredSubstEntry>(&pv.into()).to_predicate().unwrap()
                    .get_polarized_prime_formulas(current)
            }).chain(
                self.clauses().iter().flat_map(|(_, body)| body.get_polarized_prime_formulas(current))
            ).collect();
            
        *self.blocked_collection.borrow_mut() = false;
        
        result
    }
    
    fn substitute(&self, from: &PredSubstEntry, to: &PredSubstEntry) -> Rc<MinlogPredicate> {
        let mut new_params = self.params.clone();
        new_params.extend((from.clone(), to.clone()));
        new_params.restrict(|elem| self.params.contains(elem));
        Rc::new(MinlogPredicate::InductivePredicate(self.clone()))
    }
    
    fn first_conflict_with(&self, other: &Rc<MinlogPredicate>) -> Option<(PredSubstEntry, PredSubstEntry)> {
        if !other.is_inductive_predicate() {
            panic!("Tried to find conflict between incompatible PredSubstEntry types");
        }
        
        let other_ipred = other.to_inductive_predicate().unwrap();
        
        if self.definition != other_ipred.definition {
            return Some((Rc::new(MinlogPredicate::InductivePredicate(self.clone())).into(), other.clone().into()));
        }
        
        for (from, to) in self.params.pairs().iter() {
            let other_to = self.params.substitute::<PredSubstEntry>(&from.clone());
            if to != &other_to {
                return Some((from.clone(), other_ipred.params.substitute::<PredSubstEntry>(&from.clone())));
            }
        }
        
        None
    }
    
    fn match_with(&self, ctx: &mut impl MatchContext<PredSubstEntry>) -> MatchOutput<PredSubstEntry> {
        let pattern = ctx.next_pattern().unwrap();
        let instance = ctx.next_instance().unwrap();
        
        match (pattern, instance) {
            (PredSubstEntry::Predicate(p), PredSubstEntry::Predicate(i)) => {
                if !p.is_inductive_predicate() || !i.is_inductive_predicate() {
                    return MatchOutput::FailedMatch;
                }
                
                let ipred_pattern = p.to_inductive_predicate().unwrap();
                let ipred_instance = i.to_inductive_predicate().unwrap();
                
                if ipred_pattern.definition != ipred_instance.definition {
                    return MatchOutput::FailedMatch;
                }
                
                for (from, to) in ipred_pattern.params.pairs().iter() {
                    let instance_to = ipred_instance.params.substitute::<PredSubstEntry>(&from.clone());
                    if to != &instance_to {
                        ctx.extend(from, &instance_to);
                    }
                }
                
                MatchOutput::Matched
            },
            _ => MatchOutput::FailedMatch
        }
    }
}

impl PrettyPrintable for InductivePredicate {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        let tparams = self.definition.get_type_variables();
        let tmparams = self.definition.get_term_variables();
        let pparams = self.definition.get_predicate_variables();
        
        if tparams.is_empty() && tmparams.is_empty() && pparams.is_empty() {
            PPElement::text(self.definition.name().clone())
        } else {
            let params = PPElement::list(
                tparams.iter().map(|tv| tv.into())
                    .chain(tmparams.iter().map(|tv| tv.into()))
                    .chain(pparams.iter().map(|pv| pv.into()))
                    .map(|param| {
                        let substituted = self.params.substitute::<PredSubstEntry>(&param);
                        if detail && substituted != param {
                            PPElement::group(vec![
                                param.to_pp_element(detail),
                                PPElement::break_elem(1, 4, false),
                                PPElement::text("=".to_string()),
                                PPElement::break_elem(1, 4, false),
                                substituted.to_pp_element(detail),
                            ], BreakType::Flexible, 0)
                        } else {
                            substituted.to_pp_element(detail)
                        }
                    }).collect(),
                PPElement::break_elem(0, 4, false),
                PPElement::text(",".to_string()),
                PPElement::break_elem(1, 4, false),
                BreakType::Flexible,
            );
            
            PPElement::group(vec![
                PPElement::text(self.definition.name().clone()),
                PPElement::text("<".to_string()),
                PPElement::break_elem(1, 4, false),
                params,
                PPElement::break_elem(1, 0, false),
                PPElement::text(">".to_string())
            ], BreakType::Consistent, 0)
        }
    }
}