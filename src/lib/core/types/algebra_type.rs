
use std::{rc::Rc, cell::RefCell};
use crate::utils::pretty_printer::*;

use crate::core::substitution::{MatchContext, MatchOutput};
use crate::core::polarity::{Polarity, Polarized};

use crate::core::types::minlog_type::{MinlogType, TypeBody};

use crate::core::terms::minlog_term::MinlogTerm;

use crate::core::terms::term_substitution::{TermSubstitution, TermSubstEntry};

use crate::core::structures::algebra::Algebra;

#[derive(Clone, PartialEq, Eq)]
pub struct AlgebraType {
    algebra: Rc<Algebra>,
    parameters: Vec<(Rc<MinlogType>, Rc<MinlogType>)>,
    blocked_collection: RefCell<bool>,
}

impl AlgebraType {
    pub fn create(algebra: Rc<Algebra>, parameters: Vec<(Rc<MinlogType>, Rc<MinlogType>)>) -> Rc<MinlogType> {
        let expected_vars = algebra.get_type_variables();
        
        for (from, _) in parameters.iter() {
            if !expected_vars.iter().any(|v| v == from) {
                panic!("Type variable '{}' not expected in parameters for algebra '{}'", from.debug_string(), algebra.name());
            }
        }
        
        for var in expected_vars {
            if !parameters.iter().any(|(from, _)| from == var) {
                panic!("Missing type variable '{}' in parameters for algebra '{}'", var.debug_string(), algebra.name());
            }
        }
        
        Rc::new(MinlogType::Algebra(AlgebraType { algebra, parameters, blocked_collection: RefCell::new(false) }))
    }
    
    pub fn algebra(&self) -> &Rc<Algebra> {
        &self.algebra
    }
    
    pub fn name(&self) -> &String {
        self.algebra.name()
    }
    
    pub fn constructors(&self) -> Vec<Rc<MinlogTerm>> {
        let subst = self.substitution();

        self.algebra.constructors().iter()
            .map(|c| subst.substitute(&TermSubstEntry::Term(c.clone())))
            .map(|t| t.to_term().unwrap())
            .collect()
    }
    
    pub fn constructor(&self, name: &String) -> Option<Rc<MinlogTerm>> {
        let subst = self.substitution();

        self.algebra.constructor(name).map(|c| {
            subst.substitute(&TermSubstEntry::Term(c.clone())).to_term().unwrap()
        })
    }
    
    pub fn parameters(&self) -> &Vec<(Rc<MinlogType>, Rc<MinlogType>)> {
        &self.parameters
    }
    
    pub fn substitution(&self) -> TermSubstitution {
        TermSubstitution::from_pairs(self.parameters.iter()
            .map(|(from, to)| (from.into(), to.into()))
            .collect())
    }
    
    pub fn references_algebra(&self, algebra: &Rc<MinlogType>) -> bool {
        self.constructors().iter().any(|c| {
            c.minlog_type().contains_algebra_type(algebra)
        })
    }
    
    pub fn ensure_well_founded(&self) {
        let self_alg_type = Rc::new(MinlogType::Algebra(self.clone()));

        for constructor in self.constructors().iter() {
            let constructor_type = constructor.minlog_type();
            
            if let Some(arrow_type) = constructor_type.to_arrow() {
                for arg_type in arrow_type.arguments().iter() {
                    let polarized_algs = arg_type.get_polarized_algebras(Polarity::StrictlyPositive);
                    for polarized_alg in polarized_algs.iter() {
                        if !polarized_alg.polarity.is_strictly_positive() &&
                            polarized_alg.value.to_algebra().unwrap().references_algebra(&self_alg_type) {
                            panic!("Algebra '{}' is not well-founded due to constructor '{}'", self.name(), constructor.debug_string());
                        }
                    }
                }
            }
        }
    }
}

impl TypeBody for AlgebraType {
    fn is_object_type(&self) -> bool {
        true
    }
    
    fn remove_nulls(&self) -> Option<Rc<MinlogType>> {
        Some(AlgebraType::create(self.algebra.clone(), self.parameters.clone()))
    }
    
    fn level(&self) -> usize {
        self.parameters.iter()
            .map(|(_, to)| to.level())
            .max().unwrap_or(0)
    }
    
    fn get_polarized_tvars(&self, current: Polarity) -> Vec<Polarized<Rc<MinlogType>>> {
        // TODO: Get rid of this hacky fix to avoid infinite recursion
        if *self.blocked_collection.borrow() {
            return vec![];
        }
        
        *self.blocked_collection.borrow_mut() = true;
        
        let mut result = self.constructors().iter()
            .flat_map(|c| c.minlog_type().get_polarized_tvars(current))
            .collect::<Vec<_>>();
        
        *self.blocked_collection.borrow_mut() = false;
        
        result.extend(self.parameters.iter()
            .flat_map(|(_, to)| to.get_polarized_tvars(current)));
        
        result
    }
    
    fn get_polarized_algebras(&self, current: Polarity) -> Vec<Polarized<Rc<MinlogType>>> {
        if *self.blocked_collection.borrow() {
            return vec![];
        }
        
        *self.blocked_collection.borrow_mut() = true;
        
        let mut result = self.constructors().iter()
            .flat_map(|c| c.minlog_type().get_polarized_algebras(current))
            .collect::<Vec<_>>();
        
        *self.blocked_collection.borrow_mut() = false;
        
        result.extend(self.parameters.iter()
            .flat_map(|(_, to)| to.get_polarized_algebras(current)));
        
        result.push(Polarized::new(current, Rc::new(MinlogType::Algebra(self.clone()))));
        
        result
    }
    
    fn substitute(&self, from: &Rc<MinlogType>, to: &Rc<MinlogType>) -> Rc<MinlogType> {
        if self.parameters.iter().any(|(f, _)| f == from) {
            let mut subst = self.substitution();
            subst.extend((from.into(), to.into()));
            let mut new_params = subst.pairs().iter()
                .map(|(f, t)| (f.to_type().unwrap(), t.to_type().unwrap()))
                .collect::<Vec<_>>();
            
            for (f, _) in self.parameters.iter() {
                if !new_params.iter().any(|(nf, _)| nf == f) {
                    new_params.push((f.clone(), f.clone()));
                }
            }
            
            if new_params.len() != self.parameters.len() {
                panic!("Substitution changed the number of parameters in an algebra type.");
            }
            
            AlgebraType::create(self.algebra.clone(), new_params)
        } else {
            AlgebraType::create(self.algebra.clone(), self.parameters.clone())
        }
    }
    
    fn first_conflict_with(&self, other: &Rc<MinlogType>) -> Option<(Rc<MinlogType>, Rc<MinlogType>)> {
        if !other.is_algebra() {
            return Some((Rc::new(MinlogType::Algebra(self.clone())), other.clone()));
        }
        
        let other_alg = other.to_algebra().unwrap();
        
        if self.algebra.as_ref() != other_alg.algebra.as_ref() {
            return Some((Rc::new(MinlogType::Algebra(self.clone())), other.clone()));
        }
        
        for (from, to) in self.parameters.iter() {
            let other_to = self.substitution().substitute::<TermSubstEntry>(&from.into()).to_type().unwrap();
            if let Some(conflict) = to.first_conflict_with(&other_to) {
                return Some(conflict);
            }
        }
        
        for (from, to) in other_alg.parameters.iter() {
            let self_to = other_alg.substitution().substitute::<TermSubstEntry>(&from.into()).to_type().unwrap();
            if let Some(conflict) = to.first_conflict_with(&self_to) {
                return Some(conflict);
            }
        }
        
        None
    }
    
    fn match_with(&self, ctx: &mut impl MatchContext<Rc<MinlogType>>) -> MatchOutput<Rc<MinlogType>> {
        let pattern = ctx.next_pattern().unwrap();
        let instance = ctx.next_instance().unwrap();
        
        if !pattern.is_algebra() || !instance.is_algebra() {
            return MatchOutput::FailedMatch;
        }
        
        let pattern_alg = pattern.to_algebra().unwrap();
        let instance_alg = instance.to_algebra().unwrap();
        
        if pattern_alg.algebra.as_ref() != instance_alg.algebra.as_ref() {
            return MatchOutput::FailedMatch;
        }
        
        if pattern_alg.parameters.len() != instance_alg.parameters.len() {
            return MatchOutput::FailedMatch;
        }
        
        let subst = instance_alg.substitution();

        for (from, to) in pattern_alg.parameters.iter() {
            let instance_to = subst.substitute::<TermSubstEntry>(&from.into()).to_type().unwrap();
            ctx.extend(to, &instance_to);
        }
        
        MatchOutput::Matched
    }
}

impl PrettyPrintable for AlgebraType {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        if self.parameters.is_empty() {
            PPElement::text(self.algebra.name().clone())
        } else {
            let params = PPElement::list(
                self.parameters.iter().map(|(from, to)| {
                    if detail && from != to {
                        PPElement::group(vec![
                            from.to_pp_element(detail),
                            PPElement::break_elem(1, 4, false),
                            PPElement::text("=".to_string()),
                            PPElement::break_elem(1, 4, false),
                            to.to_enclosed_pp_element(detail)
                        ], BreakType::Flexible, 0)
                    } else {
                        to.to_enclosed_pp_element(detail)
                    }
                }).collect(),
                PPElement::break_elem(0, 4, false),
                PPElement::text(",".to_string()),
                PPElement::break_elem(1, 4, false),
                BreakType::Flexible
            );
            
            PPElement::group(vec![
                PPElement::text(self.algebra.name().clone()),
                PPElement::text("<".to_string()),
                PPElement::break_elem(1, 4, false),
                params,
                PPElement::break_elem(1, 0, false),
                PPElement::text(">".to_string())
            ], BreakType::Consistent, 0)
        }
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}