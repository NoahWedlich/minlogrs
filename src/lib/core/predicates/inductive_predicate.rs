
use std::{rc::Rc, cell::RefCell, hash::{Hash, Hasher}, collections::HashSet};
use crate::{utils::pretty_printer::{BreakType, PPElement, PrettyPrintable}};

use crate::core::substitution::{MatchContext, MatchOutput};
use crate::core::polarity::{Polarity, Polarized};

use crate::core::types::minlog_type::MinlogType;
use crate::core::types::type_constant::TypeConstant;
use crate::core::types::algebra_type::AlgebraType;

use crate::core::terms::minlog_term::MinlogTerm;
use crate::core::predicates::minlog_predicate::{PredicateBody, MinlogPredicate};

use crate::core::types::type_substitution::TypeSubstitution;
use crate::core::terms::term_substitution::TermSubstEntry;
use crate::core::predicates::predicate_substitution::{PredSubstEntry, PredicateSubstitution};

use crate::core::structures::inductive_constant::InductiveConstant;

#[derive(Clone, PartialEq, Eq)]
pub struct InductivePredicate {
    definition: Rc<InductiveConstant>,
    params: PredicateSubstitution,
    blocked_collection: RefCell<bool>,
}

impl InductivePredicate {
    pub fn create(definition: Rc<InductiveConstant>, mut params: PredicateSubstitution) -> Rc<MinlogPredicate> {
        let idp_vars: Vec<PredSubstEntry> = definition.get_type_variables(&mut HashSet::new()).into_iter().map(|tv| tv.into())
            .chain(definition.get_free_variables(&mut HashSet::new()).into_iter().map(|tv| tv.into()))
            .chain(definition.get_polarized_pred_vars(Polarity::Unknown, &mut HashSet::new()).into_iter().map(|pol| pol.value.into()))
            .collect();
        
        params.restrict(|from| idp_vars.contains(from));
        
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
    
    pub fn clauses(&self) -> Vec<(String, Rc<MinlogPredicate>)> {
        self.definition.clauses().iter()
            .map(|(name, body)| (
                name.clone(),
                self.params.substitute::<PredSubstEntry>(&body.into()).to_predicate().unwrap()
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
            AlgebraType::create(alg.clone(), type_subst)
        })
    }
    
    pub fn references_idp(&self, idp: &Rc<MinlogPredicate>) -> bool {
        if self == idp.to_inductive_predicate().unwrap() {
            return true;
        }
        
        if self.clauses().iter().any(|(_, body)| body.contains_inductive_predicate(idp)) {
            return true;
        }
        
        self.params.pairs().iter().any(|(_, to)| {
            if let PredSubstEntry::Predicate(pred) = to {
                pred.contains_inductive_predicate(idp)
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
                    let polarized_args = premise.get_polarized_inductive_preds(Polarity::StrictlyPositive, &mut HashSet::new());
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
    fn arity(&self) -> Rc<MinlogType> {
        self.params.substitute::<PredSubstEntry>(&self.definition.arity().into()).to_type().unwrap()
    }
    
    fn normalize(&self, _eta: bool, _pi: bool) -> Rc<MinlogPredicate> {
        Rc::new(MinlogPredicate::InductivePredicate(self.clone()))
    }
    
    fn depth(&self) -> usize {
        1 + self.params.pairs().iter()
            .map(|(_, to)| match to {
                PredSubstEntry::Term(tmv) => tmv.depth(),
                PredSubstEntry::Predicate(pv) => pv.depth(),
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
    
    fn get_type_variables(&self, visited: &mut HashSet<MinlogPredicate>) -> HashSet<Rc<MinlogType>> {
        if visited.contains(&MinlogPredicate::InductivePredicate(self.clone())) {
            HashSet::new()
        } else {
            visited.insert(MinlogPredicate::InductivePredicate(self.clone()));
            
            self.definition.get_type_variables(visited).iter()
                .flat_map(|tv| {
                    self.params.substitute::<PredSubstEntry>(&tv.into()).to_type().unwrap().get_type_variables(&mut HashSet::new())
                }).chain(
                    self.clauses().iter().flat_map(|(_, body)| body.get_type_variables(visited))
                ).collect()
        }
    }
    
    fn get_algebra_types(&self, visited: &mut HashSet<MinlogPredicate>) -> HashSet<Rc<MinlogType>> {
        if visited.contains(&MinlogPredicate::InductivePredicate(self.clone())) {
            HashSet::new()
        } else {
            visited.insert(MinlogPredicate::InductivePredicate(self.clone()));
            
            self.definition.get_type_variables(visited).iter()
                .flat_map(|tv| {
                    self.params.substitute::<PredSubstEntry>(&tv.into()).to_type().unwrap().get_algebra_types(&mut HashSet::new())
                }).chain(
                    self.clauses().iter().flat_map(|(_, body)| body.get_algebra_types(visited))
                ).collect()
        }
    }
    
    fn get_free_variables(&self, visited: &mut HashSet<MinlogPredicate>) -> HashSet<Rc<MinlogTerm>> {
        if visited.contains(&MinlogPredicate::InductivePredicate(self.clone())) {
            HashSet::new()
        } else {
            visited.insert(MinlogPredicate::InductivePredicate(self.clone()));

            self.definition.get_free_variables(visited).iter()
                .flat_map(|tv| {
                    self.params.substitute::<PredSubstEntry>(&tv.clone().into()).to_term().unwrap().get_free_variables(&mut HashSet::new())
                }).chain(
                    self.clauses().iter().flat_map(|(_, body)| body.get_free_variables(visited))
                ).collect()
        }
    }
    
    fn get_bound_variables(&self, visited: &mut HashSet<MinlogPredicate>) -> HashSet<Rc<MinlogTerm>> {
        if visited.contains(&MinlogPredicate::InductivePredicate(self.clone())) {
            HashSet::new()
        } else {
            visited.insert(MinlogPredicate::InductivePredicate(self.clone()));
            
            self.definition.get_bound_variables(visited).iter()
                .flat_map(|tv| {
                    self.params.substitute::<PredSubstEntry>(&tv.into()).to_term().unwrap().get_bound_variables(&mut HashSet::new())
                }).chain(
                    self.clauses().iter().flat_map(|(_, body)| body.get_bound_variables(visited))
                ).collect()
        }
    }
    
    fn get_polarized_pred_vars(&self, current: Polarity, visited: &mut HashSet<MinlogPredicate>) -> HashSet<Polarized<Rc<MinlogPredicate>>> {
        if visited.contains(&MinlogPredicate::InductivePredicate(self.clone())) {
            HashSet::new()
        } else {
            visited.insert(MinlogPredicate::InductivePredicate(self.clone()));
            
            let mut results = self.definition.get_polarized_pred_vars(current, visited).iter()
                .flat_map(|pol| {
                    self.params.substitute::<PredSubstEntry>(&pol.value.clone().into()).to_predicate().unwrap()
                        .get_polarized_pred_vars(current, visited)
                }).collect::<HashSet<_>>();
                
            results.extend(
                self.clauses().iter().flat_map(|(_, body)| body.get_polarized_pred_vars(current, visited))
            );
            
            results
        }
    }
    
    fn get_polarized_comp_terms(&self, current: Polarity, visited: &mut HashSet<MinlogPredicate>) -> HashSet<Polarized<Rc<MinlogPredicate>>> {
        if visited.contains(&MinlogPredicate::InductivePredicate(self.clone())) {
            HashSet::new()
        } else {
            visited.insert(MinlogPredicate::InductivePredicate(self.clone()));
            
            let mut results = self.definition.get_polarized_comp_terms(current, visited).iter()
                .flat_map(|pol| {
                    self.params.substitute::<PredSubstEntry>(&pol.value.clone().into()).to_predicate().unwrap()
                        .get_polarized_comp_terms(current, visited)
                }).collect::<HashSet<_>>();
                
            results.extend(
                self.clauses().iter().flat_map(|(_, body)| body.get_polarized_comp_terms(current, visited))
            );
            
            results
        }
    }

    fn get_polarized_inductive_preds(&self, current: Polarity, visited: &mut HashSet<MinlogPredicate>) -> HashSet<Polarized<Rc<MinlogPredicate>>> {
        if visited.contains(&MinlogPredicate::InductivePredicate(self.clone())) {
            HashSet::new()
        } else {
            visited.insert(MinlogPredicate::InductivePredicate(self.clone()));
            
            let mut results = self.definition.get_polarized_inductive_preds(current, visited).iter()
                .flat_map(|pol| {
                    self.params.substitute::<PredSubstEntry>(&pol.value.clone().into()).to_predicate().unwrap()
                        .get_polarized_inductive_preds(current, visited)
                }).collect::<HashSet<_>>();
                
            results.extend(
                self.clauses().iter().flat_map(|(_, body)| body.get_polarized_inductive_preds(current, visited))
            );
    
            results.insert(Polarized::new(current, Rc::new(MinlogPredicate::InductivePredicate(self.clone()))));
    
            results
        }
    }
    
    fn get_polarized_prime_formulas(&self, current: Polarity, visited: &mut HashSet<MinlogPredicate>) -> HashSet<Polarized<Rc<MinlogPredicate>>> {
        if visited.contains(&MinlogPredicate::InductivePredicate(self.clone())) {
            HashSet::new()
        } else {
            visited.insert(MinlogPredicate::InductivePredicate(self.clone()));
            
            let mut results = self.definition.get_polarized_prime_formulas(current, visited).iter()
                .flat_map(|pol| {
                    self.params.substitute::<PredSubstEntry>(&pol.value.clone().into()).to_predicate().unwrap()
                        .get_polarized_prime_formulas(current, visited)
                }).collect::<HashSet<_>>();
                
            results.extend(
                self.clauses().iter().flat_map(|(_, body)| body.get_polarized_prime_formulas(current, visited))
            );
            
            results
        }
    }
    
    fn substitute(&self, from: &PredSubstEntry, to: &PredSubstEntry) -> Rc<MinlogPredicate> {
        if let Some(pred) = from.to_predicate() && pred.is_inductive_predicate() && self == pred.to_inductive_predicate().unwrap() {
            to.to_predicate().unwrap()
        } else {
            let mut new_params = self.params.clone();
            new_params.extend((from.clone(), to.clone()));
            
            InductivePredicate::create(self.definition.clone(), new_params)
        }
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
        let tparams = self.definition.get_type_variables(&mut HashSet::new());
        let tmparams = self.definition.get_free_variables(&mut HashSet::new());
        let pparams = self.definition.get_polarized_pred_vars(Polarity::Unknown, &mut HashSet::new())
            .into_iter().map(|p| p.value).collect::<HashSet<_>>();
        
        let has_tparams = !tparams.is_empty();
        let has_tmparams = !tmparams.is_empty();
        let has_pparams = !pparams.is_empty();
        
        if !has_tparams && !has_tmparams && !has_pparams {
            PPElement::text(self.definition.name().clone())
        } else {
            let tvars = PPElement::list(
                tparams.iter().map(|tv| {
                    let substituted = self.params.substitute(&tv.clone());
                    if detail && &substituted != tv {
                        PPElement::group(vec![
                            tv.to_pp_element(detail),
                            PPElement::break_elem(1, 4, false),
                            PPElement::text("=".to_string()),
                            PPElement::break_elem(1, 4, false),
                            substituted.to_enclosed_pp_element(detail)
                        ], BreakType::Flexible, 0)
                    } else {
                        tv.to_enclosed_pp_element(detail)
                    }
                }).collect(),
                PPElement::break_elem(0, 4, false),
                PPElement::text(",".to_string()),
                PPElement::break_elem(1, 4, false),
                BreakType::Flexible,
            );
            
            let tmvars = PPElement::list(
                tmparams.iter().map(|tv| {
                    let substituted = self.params.substitute::<TermSubstEntry>(&tv.clone().into()).to_term().unwrap();
                    if detail && &substituted != tv {
                        PPElement::group(vec![
                            tv.to_pp_element(detail),
                            PPElement::break_elem(1, 4, false),
                            PPElement::text("=".to_string()),
                            PPElement::break_elem(1, 4, false),
                            substituted.to_enclosed_pp_element(detail)
                        ], BreakType::Flexible, 0)
                    } else {
                        tv.to_enclosed_pp_element(detail)
                    }
                }).collect(),
                PPElement::break_elem(0, 4, false),
                PPElement::text(",".to_string()),
                PPElement::break_elem(1, 4, false),
                BreakType::Flexible,
            );
            
            let pvars = PPElement::list(
                pparams.iter().map(|pv| {
                    let substituted = self.params.substitute::<PredSubstEntry>(&pv.clone().into()).to_predicate().unwrap();
                    if detail && &substituted != pv {
                        PPElement::group(vec![
                            pv.to_pp_element(detail),
                            PPElement::break_elem(1, 4, false),
                            PPElement::text("=".to_string()),
                            PPElement::break_elem(1, 4, false),
                            substituted.to_enclosed_pp_element(detail)
                        ], BreakType::Flexible, 0)
                    } else {
                        pv.to_enclosed_pp_element(detail)
                    }
                }).collect(),
                PPElement::break_elem(0, 4, false),
                PPElement::text(",".to_string()),
                PPElement::break_elem(1, 4, false),
                BreakType::Flexible,
            );
            
            let mut param_list = vec![];
            if has_tparams { param_list.push(tvars) };
            if has_tmparams { param_list.push(tmvars) };
            if has_pparams { param_list.push(pvars) };

            let parameters = PPElement::list(
                param_list,
                PPElement::break_elem(1, 0, false),
                PPElement::text("|".to_string()),
                PPElement::break_elem(1, 0, false),
                BreakType::Consistent
            );
            
            PPElement::group(vec![
                PPElement::text(self.definition.name().clone()),
                PPElement::text("<".to_string()),
                PPElement::break_elem(1, 4, false),
                parameters,
                PPElement::break_elem(1, 0, false),
                PPElement::text(">".to_string())
            ], BreakType::Consistent, 0)
        }
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}

impl Hash for InductivePredicate {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.definition.hash(state);
        self.params.hash(state);
    }
}