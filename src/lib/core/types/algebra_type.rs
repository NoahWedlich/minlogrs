
use std::{rc::Rc, cell::RefCell, hash::{Hash, Hasher}, collections::HashSet};
use crate::utils::pretty_printer::*;

use crate::core::substitution::{MatchContext, MatchOutput};
use crate::core::polarity::{Polarity, Polarized};

use crate::core::types::minlog_type::{MinlogType, TypeBody};

use crate::core::terms::minlog_term::MinlogTerm;

use crate::core::types::type_substitution::TypeSubstitution;

use crate::core::structures::algebra::Algebra;

#[derive(Clone, PartialEq, Eq)]
pub struct AlgebraType {
    algebra: Rc<Algebra>,
    parameters: TypeSubstitution,
    blocked_collection: RefCell<bool>,
}

impl AlgebraType {
    pub fn create(algebra: Rc<Algebra>, mut parameters: TypeSubstitution) -> Rc<MinlogType> {
        let alg_tvars = algebra.get_polarized_tvars(Polarity::Unknown)
            .into_iter().map(|ptv| ptv.value).collect::<Vec<_>>();
        parameters.restrict(|from| alg_tvars.contains(from));
        
        Rc::new(MinlogType::Algebra(AlgebraType { algebra, parameters, blocked_collection: RefCell::new(false) }))
    }
    
    pub fn algebra(&self) -> &Rc<Algebra> {
        &self.algebra
    }
    
    pub fn name(&self) -> &String {
        self.algebra.name()
    }
    
    pub fn constructors(&self) -> Vec<Rc<MinlogTerm>> {
        self.algebra.constructors().iter()
            .map(|c| {
                let mut constr = c.clone();
                
                for (from, to) in self.parameters.pairs() {
                    constr = constr.substitute(&from.into(), &to.into());
                }
                
                constr
            })
            .collect()
    }
    
    pub fn constructor(&self, name: &String) -> Option<Rc<MinlogTerm>> {
        self.algebra.constructor(name).map(|c| {
            let mut constr = c.clone();
            
            for (from, to) in self.parameters.pairs() {
                constr = constr.substitute(&from.into(), &to.into());
            }
            
            constr
        })
    }
    
    pub fn parameters(&self) -> &TypeSubstitution {
        &self.parameters
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
        self.parameters.pairs().into_iter()
            .map(|(_, to)| to.level())
            .max().unwrap_or(0)
    }
    
    fn get_polarized_tvars(&self, current: Polarity) -> HashSet<Polarized<Rc<MinlogType>>> {
        // TODO: Get rid of this hacky fix to avoid infinite recursion
        if *self.blocked_collection.borrow() {
            return HashSet::new();
        }
        
        *self.blocked_collection.borrow_mut() = true;
        
        let result = self.constructors().iter()
            .flat_map(|c| c.minlog_type().get_polarized_tvars(current))
            .collect::<HashSet<_>>();
        
        *self.blocked_collection.borrow_mut() = false;
        
        result
    }

    fn get_polarized_algebras(&self, current: Polarity) -> HashSet<Polarized<Rc<MinlogType>>> {
        if *self.blocked_collection.borrow() {
            return HashSet::new();
        }
        
        *self.blocked_collection.borrow_mut() = true;
        
        let result = self.constructors().iter()
            .flat_map(|c| c.minlog_type().get_polarized_algebras(current))
            .collect::<HashSet<_>>();
        
        *self.blocked_collection.borrow_mut() = false;
        
        result
    }
    
    fn substitute(&self, from: &Rc<MinlogType>, to: &Rc<MinlogType>) -> Rc<MinlogType> {
        let mut new_parameters = self.parameters.clone();
        new_parameters.extend((from.clone(), to.clone()));
        new_parameters.restrict(|f| self.algebra.get_polarized_tvars(Polarity::Unknown)
            .into_iter().map(|ptv| ptv.value).collect::<Vec<_>>().contains(f));
        AlgebraType::create(self.algebra.clone(), new_parameters)
    }
    
    fn first_conflict_with(&self, other: &Rc<MinlogType>) -> Option<(Rc<MinlogType>, Rc<MinlogType>)> {
        if !other.is_algebra() {
            return Some((Rc::new(MinlogType::Algebra(self.clone())), other.clone()));
        }
        
        let other_alg = other.to_algebra().unwrap();
        
        if self.algebra.as_ref() != other_alg.algebra.as_ref() {
            return Some((Rc::new(MinlogType::Algebra(self.clone())), other.clone()));
        }
        
        for (from, to) in self.parameters.pairs().iter() {
            let other_to = other_alg.parameters.substitute::<Rc<MinlogType>>(from);
            if let Some(conflict) = to.first_conflict_with(&other_to) {
                return Some(conflict);
            }
        }
        
        for (from, to) in other_alg.parameters.pairs().iter() {
            let self_to = self.parameters.substitute::<Rc<MinlogType>>(from);
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
        
        pattern_alg.parameters.add_subst_match(&instance_alg.parameters, ctx);
        
        MatchOutput::Matched
    }
}

impl PrettyPrintable for AlgebraType {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        let tparams = self.algebra.get_polarized_tvars(Polarity::Unknown)
            .into_iter().map(|ptv| ptv.value).collect::<Vec<_>>();
        
        if tparams.is_empty() {
            PPElement::text(self.algebra.name().clone())
        } else {
            let params = PPElement::list(
                tparams.iter().map(|param| {
                    let substituted = self.parameters.substitute::<Rc<MinlogType>>(param);
                    if detail && &substituted != param {
                        PPElement::group(vec![
                            param.to_pp_element(detail),
                            PPElement::break_elem(1, 4, false),
                            PPElement::text("=".to_string()),
                            PPElement::break_elem(1, 4, false),
                            substituted.to_enclosed_pp_element(detail)
                        ], BreakType::Flexible, 0)
                    } else {
                        param.to_enclosed_pp_element(detail)
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

impl Hash for AlgebraType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.algebra.hash(state);
        self.parameters.hash(state);
    }
}