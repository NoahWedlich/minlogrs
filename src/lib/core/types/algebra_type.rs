
use std::rc::Rc;
use crate::utils::pretty_printer::*;

use crate::core::substitution::MatchContext;

use crate::core::types::minlog_type::{MinlogType, TypeBody};

use crate::core::terms::minlog_term::MinlogTerm;

use crate::core::terms::term_substitution::{TermSubstitution, TermSubstEntry};

use crate::core::structures::algebra::Algebra;

#[derive(Clone, PartialEq, Eq)]
pub struct AlgebraType {
    algebra: Rc<Algebra>,
    parameters: TermSubstitution
}

impl AlgebraType {
    pub fn create(algebra: Rc<Algebra>, parameters: TermSubstitution) -> Rc<MinlogType> {
        let expected_vars = algebra.get_type_variables();
        
        for (from, _) in parameters.pairs() {
            match from {
                TermSubstEntry::Type(t) => {
                    if !expected_vars.iter().any(|v| v == t) {
                        panic!("Type variable '{}' not expected in parameters for algebra '{}'", t.debug_string(), algebra.name());
                    }
                },
                _ => panic!("Only type variables can be used as parameters for algebra types"),
            }
        }
        
        for var in expected_vars {
            if !parameters.pairs().iter().any(|(from, _)| match from {
                TermSubstEntry::Type(t) => t == &var,
                _ => false,
            }) {
                panic!("Missing type variable '{}' in parameters for algebra '{}'", var.debug_string(), algebra.name());
            }
        }
        
        Rc::new(MinlogType::Algebra(AlgebraType { algebra, parameters }))
    }
    
    pub fn algebra(&self) -> &Rc<Algebra> {
        &self.algebra
    }
    
    pub fn name(&self) -> &String {
        self.algebra.name()
    }
    
    pub fn constructors(&self) -> Vec<Rc<MinlogTerm>> {
        self.algebra.constructors().iter()
            .map(|c| self.parameters.substitute(&TermSubstEntry::Term(c.clone())))
            .map(|t| t.to_term().unwrap())
            .collect()
    }
    
    pub fn constructor(&self, name: &String) -> Option<Rc<MinlogTerm>> {
        self.algebra.constructor(name).map(|c| {
            self.parameters.substitute(&TermSubstEntry::Term(c.clone())).to_term().unwrap()
        })
    }
    
    pub fn parameters(&self) -> &TermSubstitution {
        &self.parameters
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
        self.parameters.pairs().iter()
            .map(|(_, t)| match t {
                TermSubstEntry::Type(t) => t.level(),
                TermSubstEntry::Term(_) => 0,
            }).max().unwrap_or(0)
    }
    
    fn substitute(&self, from: &Rc<MinlogType>, to: &Rc<MinlogType>) -> Rc<MinlogType> {
        let mut new_params = self.parameters.clone();
        new_params.extend((TermSubstEntry::Type(from.clone()), TermSubstEntry::Type(to.clone())));
        AlgebraType::create(self.algebra.clone(), new_params)
    }
    
    fn first_conflict_with(&self, other: &Rc<MinlogType>) -> Option<(Rc<MinlogType>, Rc<MinlogType>)> {
        if !other.is_algebra() {
            return Some((Rc::new(MinlogType::Algebra(self.clone())), other.clone()));
        }
        
        let other_alg = other.to_algebra().unwrap();
        
        if self.algebra.as_ref() != other_alg.algebra.as_ref() {
            return Some((Rc::new(MinlogType::Algebra(self.clone())), other.clone()));
        }
        
        for (from, to) in self.parameters.pairs() {
            match (from, to) {
                (TermSubstEntry::Type(_), TermSubstEntry::Type(to_t)) => {
                    let other_to = other_alg.parameters.apply(&from).to_type().unwrap();
                    if let Some(conflict) = to_t.first_conflict_with(&other_to) {
                        return Some(conflict);
                    }
                },
                _ => {
                    panic!("Algebra type parameters must be type substitutions");
                }
            }
        }
        
        for (from, to) in other_alg.parameters.pairs() {
            match (from, to) {
                (TermSubstEntry::Type(_), TermSubstEntry::Type(to_t)) => {
                    let self_to = self.parameters.apply(&from).to_type().unwrap();
                    if let Some(conflict) = to_t.first_conflict_with(&self_to) {
                        return Some(conflict);
                    }
                },
                _ => {
                    panic!("Algebra type parameters must be type substitutions");
                }
            }
        }
        
        None
    }
    
    fn match_with(&self, ctx: &mut impl MatchContext<Rc<MinlogType>>) -> Result<Option<(Rc<MinlogType>, Rc<MinlogType>)>, ()> {
        let pattern = ctx.next_pattern().unwrap();
        let instance = ctx.next_instance().unwrap();
        
        if !pattern.is_algebra() || !instance.is_algebra() {
            return Err(());
        }
        
        let pattern_alg = pattern.to_algebra().unwrap();
        let instance_alg = instance.to_algebra().unwrap();
        
        if pattern_alg.algebra.as_ref() != instance_alg.algebra.as_ref() {
            return Err(());
        }
        
        for (from, to) in pattern_alg.parameters.pairs() {
            let instance_to = instance_alg.parameters.apply(&from).to_type().unwrap();
            ctx.extend(&to.to_type().unwrap(), &instance_to);
        }
        
        Ok(None)
    }
}

impl PrettyPrintable for AlgebraType {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        if self.parameters.empty() {
            PPElement::text(self.algebra.name().clone())
        } else {
            let mut params = vec![];
            for (i, (from, to)) in self.parameters.pairs().iter().enumerate() {
                let param_elem = if detail {
                    PPElement::group(vec![
                        from.to_pp_element(detail),
                        PPElement::break_elem(1, 4, false),
                        PPElement::text("=".to_string()),
                        PPElement::break_elem(1, 4, false),
                        to.to_enclosed_pp_element(detail)
                    ], BreakType::Flexible, 0)
                } else {
                    to.to_enclosed_pp_element(detail)
                };
                
                params.push(
                    if i < self.parameters.pairs().len() - 1 {
                        PPElement::group(vec![
                            param_elem,
                            PPElement::break_elem(0, 4, false),
                            PPElement::text(",".to_string())
                        ], BreakType::Flexible, 0)
                    } else {
                        param_elem
                    }
                );
                
                params.push(PPElement::break_elem(1, 4, false));
            }
            
            PPElement::group(vec![
                PPElement::text(self.algebra.name().clone()),
                PPElement::text("<".to_string()),
                PPElement::break_elem(1, 4, false),
                PPElement::group(params, BreakType::Flexible, 0),
                PPElement::break_elem(1, 0, false),
                PPElement::text(">".to_string())
            ], BreakType::Consistent, 0)
        }
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}