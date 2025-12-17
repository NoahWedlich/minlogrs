
use crate::includes::{
    essential::*,
    utils::*,
    core::{
        structures::*,
        types::*,
        terms::*,
    }
};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct AlgebraType {
    algebra: Rc<Algebra>,
    parameters: TypeSubstitution,
}

impl AlgebraType {
    pub fn create(algebra: Rc<Algebra>, mut parameters: TypeSubstitution) -> Rc<MinlogType> {
        let alg_tvars = algebra.get_polarized_tvars(Polarity::Unknown, &mut IndexSet::new())
            .into_iter().map(|ptv| ptv.value).collect::<Vec<_>>();
        parameters.restrict(|from| alg_tvars.contains(from));
        
        Rc::new(MinlogType::Algebra(AlgebraType { algebra, parameters }))
    }
    
    pub fn algebra(&self) -> &Rc<Algebra> {
        &self.algebra
    }
    
    pub fn name(&self) -> &String {
        self.algebra.name()
    }
    
    pub fn constructors(&self) -> Vec<MinlogTerm> {
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
    
    pub fn constructor(&self, name: &str) -> Option<MinlogTerm> {
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
                    let polarized_algs = arg_type.get_polarized_algebras(Polarity::StrictlyPositive, &mut IndexSet::new());
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
    
    pub fn get_reduction(&self) -> Option<AlgebraReduction> {
        let null_types = self.algebra.get_polarized_tvars(Polarity::Unknown, &mut IndexSet::new())
            .into_iter().filter_map(|ptv| {
                let original_type = ptv.value;
                let substituted_type = self.parameters.substitute(&original_type).remove_nulls();
                if substituted_type.is_none() {
                    Some(original_type)
                } else {
                    None
                }
            }).collect::<IndexSet<_>>();
            
        self.algebra.reduce(&null_types)
    }
}

impl TypeBody for AlgebraType {
    fn is_object_type(&self) -> bool {
        true
    }
    
    fn remove_nulls(&self) -> Option<Rc<MinlogType>> {
        if let Some(reduction) = self.get_reduction() {
            let new_parameters = TypeSubstitution::from_pairs(
                self.parameters.pairs().into_iter().filter_map(|(from, to)| {
                    let substituted_from = self.parameters.substitute(&from);
                    let substituted_to = self.parameters.substitute(&to);
                    if substituted_from.is_null() || substituted_to.is_null() {
                        None
                    } else {
                        Some((from, to))
                    }
                }
            ).collect());
            
            Some(AlgebraType::create(reduction.reduced_algebra, new_parameters))
        } else {
            Some(Rc::new(MinlogType::Algebra(self.clone())))
        }
    }
    
    fn level(&self) -> usize {
        self.parameters.pairs().into_iter()
            .map(|(_, to)| to.level())
            .max().unwrap_or(0)
    }
    
    fn get_polarized_tvars(&self, current: Polarity, visited: &mut IndexSet<MinlogType>) -> IndexSet<Polarized<Rc<MinlogType>>> {
        if visited.contains(&MinlogType::Algebra(self.clone())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogType::Algebra(self.clone()));
            
            self.constructors().iter()
                .flat_map(|c| c.minlog_type().get_polarized_tvars(current, visited))
                .collect::<IndexSet<_>>()
        }
    }

    fn get_polarized_algebras(&self, current: Polarity, visited: &mut IndexSet<MinlogType>) -> IndexSet<Polarized<Rc<MinlogType>>> {
        if visited.contains(&MinlogType::Algebra(self.clone())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogType::Algebra(self.clone()));

            let mut algebras = self.constructors().iter()
                .flat_map(|c| c.minlog_type().get_polarized_algebras(current, visited))
                .collect::<IndexSet<_>>();
            
            algebras.insert(Polarized::new(current, Rc::new(MinlogType::Algebra(self.clone()))));
            
            algebras
        }
    }
    
    fn substitute(&self, from: &Rc<MinlogType>, to: &Rc<MinlogType>) -> Rc<MinlogType> {
        if from.is_algebra() && self == from.to_algebra().unwrap() {
            to.clone()
        } else {
            let mut new_params = self.parameters.clone();
            new_params.extend((from.clone(), to.clone()));
            
            AlgebraType::create(self.algebra.clone(), new_params)
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
    
    fn match_with(&self, instance: &Rc<MinlogType>) -> MatchOutput<Rc<MinlogType>> {
        if !instance.is_algebra() {
            return MatchOutput::FailedMatch;
        }
        
        let instance_alg = instance.to_algebra().unwrap();
        
        if self.algebra.as_ref() != instance_alg.algebra.as_ref() {
            return MatchOutput::FailedMatch;
        }
        
        let conditions = TypeSubstitution::collect_match_conditions(
            &self.parameters,
            &instance_alg.parameters
        );
        
        MatchOutput::Matched(conditions)
    }
}

impl PrettyPrintable for AlgebraType {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        let tparams = self.algebra.get_polarized_tvars(Polarity::Unknown, &mut IndexSet::new())
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
                        substituted.to_enclosed_pp_element(detail)
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