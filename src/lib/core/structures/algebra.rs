
use indexmap::{IndexMap, IndexSet};
use std::{rc::Rc, cell::RefCell, hash::{Hash, Hasher}};
use crate::{core::{terms::constructor::Constructor, types::{algebra_type::AlgebraType, type_constant::TypeConstant, type_substitution::TypeSubstitution}}, utils::pretty_printer::*};

use crate::core::polarity::{Polarity, Polarized};

use crate::core::types::minlog_type::MinlogType;

use crate::core::terms::minlog_term::MinlogTerm;

#[derive(Clone, PartialEq, Eq)]
pub struct AlgebraReduction {
    pub reduced_algebra: Rc<Algebra>,
    pub constructor_mapping: IndexMap<String, String>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Algebra {
    name: String,
    constructors: RefCell<Vec<Rc<MinlogTerm>>>,
    reductions: RefCell<IndexMap<Vec<Rc<MinlogType>>, AlgebraReduction>>
}

impl Algebra {
    pub fn create(name: String) -> Rc<Algebra> {
        Rc::new(Algebra { name, constructors: RefCell::new(vec![]), reductions: RefCell::new(IndexMap::new()) })
    }
    
    pub fn name(&self) -> &String {
        &self.name
    }
    
    pub fn constructors(&self) -> Vec<Rc<MinlogTerm>> {
        self.constructors.borrow().iter().cloned().collect()
    }
    
    pub fn constructor(&self, name: &String) -> Option<Rc<MinlogTerm>> {
        self.constructors.borrow().iter().find(|c| c.to_constructor().unwrap().name() == name).cloned()
    }
    
    pub fn add_constructor(&self, constructor: Rc<MinlogTerm>) {
        if !constructor.is_constructor() {
            panic!("Only constructor terms can be added to an algebra");
        }
        
        if let Some(algebra_type) = constructor.minlog_type().to_algebra() {
            if algebra_type.algebra().as_ref() != self {
                panic!("Constructor type's algebra does not match the algebra it is being added to");
            }
        } else if let Some(arrow_type) = constructor.minlog_type().to_arrow() {
            if !arrow_type.value().is_algebra() {
                panic!("Constructor type must be an algebra type or an arrow type ending in an algebra type");
            }
            
            if arrow_type.value().to_algebra().unwrap().algebra().as_ref() != self {
                panic!("Constructor type's algebra does not match the algebra it is being added to");
            }
        } else {
            panic!("Constructor type must be an algebra type or an arrow type ending in an algebra type");
        }
        
        if let Some(existing) = self.constructor(constructor.to_constructor().unwrap().name()) {
            panic!("Constructor with name '{}' already exists in algebra '{}'", existing.to_constructor().unwrap().name(), self.name);
        }
        
        self.constructors.borrow_mut().push(constructor);
    }
    
    pub fn reductions(&self) -> IndexMap<Vec<Rc<MinlogType>>, AlgebraReduction> {
        self.reductions.borrow().clone()
    }
    
    pub fn add_reduction(&self, null_types: IndexSet<Rc<MinlogType>>, reduced_algebra: Rc<Algebra>) {
        let relevant_null_types = null_types
            .intersection(
                &self.get_polarized_tvars(Polarity::Unknown, &mut IndexSet::new())
                    .into_iter().map(|ptv| ptv.value).collect::<IndexSet<_>>()
            ).cloned().collect::<Vec<_>>();
        
        if self.reductions.borrow().contains_key(&relevant_null_types) {
            panic!("Reduction for algebra '{}' with null types {:?} already exists", self.name, relevant_null_types.iter().map(|t| t.debug_string()).collect::<Vec<_>>());
        }
        
        let self_type = AlgebraType::create(Rc::new(self.clone()), TypeSubstitution::make_empty());
        let reduced_algebra_type = AlgebraType::create(reduced_algebra.clone(), TypeSubstitution::make_empty());
        
        let mut constructor_mapping = IndexMap::new();
        let mut remaining_constructors = reduced_algebra.constructors.borrow().clone();
        
            let subst = TypeSubstitution::from_pairs(
                relevant_null_types.iter().map(|t| (t.clone(), TypeConstant::create_null())).collect()
            );
        
        let substituded_self = subst.substitute(&self_type);
        
        for constructor in self.constructors.borrow().iter() {
            
            let mut reduced_type = subst.substitute(&constructor.minlog_type());
            reduced_type = reduced_type.substitute(&substituded_self, &reduced_algebra_type);
            reduced_type = reduced_type.remove_nulls().unwrap();
            
            let possible_constructors = remaining_constructors.iter().filter_map(|c| {
                if c.minlog_type() == reduced_type {
                    Some(c.to_constructor().unwrap().name().clone())
                } else {
                    None
                }
            }).collect::<Vec<_>>();
            
            if possible_constructors.is_empty() {
                panic!("No matching constructor found in reduced algebra '{}' for constructor '{}' of algebra '{}' with reduced type '{}'",
                    reduced_algebra.name, constructor.debug_string(), self.name, reduced_type.debug_string());
            } else if possible_constructors.len() > 1 {
                panic!("Multiple matching constructors found in reduced algebra '{}' for constructor '{}' of algebra '{}' with reduced type '{}': [{}]",
                    reduced_algebra.name, constructor.debug_string(), self.name, reduced_type.debug_string(), possible_constructors.join(", "));
            } else {
                constructor_mapping.insert(constructor.to_constructor().unwrap().name().clone(), possible_constructors[0].clone());
                remaining_constructors.retain(|c| c.to_constructor().unwrap().name() != &possible_constructors[0]);
            }
        }
        
        self.reductions.borrow_mut().insert(relevant_null_types, AlgebraReduction {
            reduced_algebra,
            constructor_mapping,
        });
    }
    
    pub fn add_reduction_with_mapping(&self, null_types: IndexSet<Rc<MinlogType>>, reduced_algebra: Rc<Algebra>, constructor_mapping: IndexMap<String, String>) {
        let relevant_null_types = null_types
            .intersection(
                &self.get_polarized_tvars(Polarity::Unknown, &mut IndexSet::new())
                    .into_iter().map(|ptv| ptv.value).collect::<IndexSet<_>>()
            ).cloned().collect::<Vec<_>>();
        
        if self.reductions.borrow().contains_key(&relevant_null_types) {
            panic!("Reduction for algebra '{}' with null types {:?} already exists", self.name, relevant_null_types.iter().map(|t| t.debug_string()).collect::<Vec<_>>());
        }
        
        let self_type = AlgebraType::create(Rc::new(self.clone()), TypeSubstitution::make_empty());
        let reduced_algebra_type = AlgebraType::create(reduced_algebra.clone(), TypeSubstitution::make_empty());
        
            let subst = TypeSubstitution::from_pairs(
                relevant_null_types.iter().map(|t| (t.clone(), TypeConstant::create_null())).collect()
            );
        
        let substituded_self = subst.substitute(&self_type);
        
        for constructor in self.constructors.borrow().iter() {
            
            let mut reduced_type = subst.substitute(&constructor.minlog_type());
            reduced_type = reduced_type.substitute(&substituded_self, &reduced_algebra_type);
            reduced_type = reduced_type.remove_nulls().unwrap();
            
            if let Some(reduced_constructor_name) = constructor_mapping.get(constructor.to_constructor().unwrap().name()) {
                if let Some(reduced_constructor) = reduced_algebra.constructor(reduced_constructor_name) {
                    if reduced_constructor.minlog_type() != reduced_type {
                        panic!("Mapped constructor '{}' in reduced algebra '{}' does not match reduced type '{}' for constructor '{}' of algebra '{}'",
                            reduced_constructor_name, reduced_algebra.name, reduced_type.debug_string(), constructor.debug_string(), self.name);
                    }
                } else {
                    panic!("Mapped constructor '{}' not found in reduced algebra '{}'", reduced_constructor_name, reduced_algebra.name);
                }
            } else {
                panic!("No mapping provided for constructor '{}' of algebra '{}'", constructor.debug_string(), self.name);
            }
        }
        
        self.reductions.borrow_mut().insert(relevant_null_types, AlgebraReduction {
            reduced_algebra,
            constructor_mapping,
        });
    }
    
    pub fn generate_reduction(&self, null_types: IndexSet<Rc<MinlogType>>, name: String) {
        let relevant_null_types = null_types
            .intersection(
                &self.get_polarized_tvars(Polarity::Unknown, &mut IndexSet::new())
                    .into_iter().map(|ptv| ptv.value).collect::<IndexSet<_>>()
            ).cloned().collect::<Vec<_>>();
        
        if self.reductions.borrow().contains_key(&relevant_null_types) {
            panic!("Reduction for algebra '{}' with null types {:?} already exists",
                self.name, relevant_null_types.iter().map(|t| t.debug_string()).collect::<Vec<_>>()
            );
        }
        
        let self_type = AlgebraType::create(Rc::new(self.clone()), TypeSubstitution::make_empty());
        
        let reduced_algebra = Algebra::create(name);
        let reduced_algebra_type = AlgebraType::create(reduced_algebra.clone(), TypeSubstitution::make_empty());
        
            let subst = TypeSubstitution::from_pairs(
                relevant_null_types.iter().map(|t| (t.clone(), TypeConstant::create_null())).collect()
            );
        
        let substituded_self = subst.substitute(&self_type);
        
        for constructor in self.constructors.borrow().iter() {
            
            let mut reduced_type = subst.substitute(&constructor.minlog_type());
            reduced_type = reduced_type.substitute(&substituded_self, &reduced_algebra_type);
            reduced_type = reduced_type.remove_nulls().unwrap();
            
            let reduced_constructor = Constructor::create(
                constructor.to_constructor().unwrap().name().clone(),
                reduced_type
            );
            
            reduced_algebra.add_constructor(reduced_constructor);
        }
        
        reduced_algebra_type.to_algebra().unwrap().ensure_well_founded();
        
        let constructor_mapping = self.constructors.borrow().iter().map(|c| {
            (c.to_constructor().unwrap().name().clone(), c.to_constructor().unwrap().name().clone())
        }).collect::<IndexMap<_, _>>();
        
        self.reductions.borrow_mut().insert(relevant_null_types, AlgebraReduction {
            reduced_algebra,
            constructor_mapping,
        });
    }
    
    pub fn reduce(&self, null_types: &IndexSet<Rc<MinlogType>>) -> Option<AlgebraReduction> {
        let relevant_null_types = null_types
            .intersection(
                &self.get_polarized_tvars(Polarity::Unknown, &mut IndexSet::new())
                    .into_iter().map(|ptv| ptv.value).collect::<IndexSet<_>>()
            ).cloned().collect::<Vec<_>>();
            
        if relevant_null_types.is_empty() {
            return None;
        }
        
        if self.reductions.borrow().contains_key(&relevant_null_types) {
            Some(self.reductions.borrow().get(&relevant_null_types).unwrap().clone())
        } else {
            self.generate_reduction(
                relevant_null_types.iter().cloned().collect(),
                format!("{}Red{}", self.name, self.reductions.borrow().len())
            );
            
            Some(self.reductions.borrow().get(&relevant_null_types).unwrap().clone())
        }
    }
    
    pub fn get_polarized_tvars(&self, current: Polarity, visited: &mut IndexSet<MinlogType>) -> IndexSet<Polarized<Rc<MinlogType>>> {
        self.constructors.borrow().iter().flat_map(|constructor| {
            constructor.minlog_type().get_polarized_tvars(current, visited)
        }).collect()
    }
    
    pub fn get_polarized_algebras(&self, current: Polarity, visited: &mut IndexSet<MinlogType>) -> IndexSet<Polarized<Rc<MinlogType>>> {
        self.constructors.borrow().iter().flat_map(|constructor| {
            constructor.minlog_type().get_polarized_algebras(current, visited)
        }).collect()
    }
}

impl PrettyPrintable for Algebra {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        let tvars = self.get_polarized_tvars(Polarity::Unknown, &mut IndexSet::new())
            .into_iter().map(|p| p.value).collect::<IndexSet<_>>();
        
        let has_tvars = !tvars.is_empty();
        
        let name = if !has_tvars {
            PPElement::text(self.name.clone())
        } else {
            let tvars = PPElement::list(
                self.get_polarized_tvars(Polarity::Unknown, &mut IndexSet::new())
                    .iter().map(|pol| pol.value.to_pp_element(detail)).collect(),
                PPElement::break_elem(0, 4, false),
                PPElement::text(",".to_string()),
                PPElement::break_elem(1, 4, false),
                BreakType::Flexible,
            );
            
            PPElement::group(vec![
                PPElement::text(self.name.clone()),
                PPElement::text("<".to_string()),
                PPElement::break_elem(1, 4, false),
                tvars,
                PPElement::break_elem(1, 0, false),
                PPElement::text(">".to_string())
            ], BreakType::Consistent, 0)
        };
        
        if detail {
            let constructors = PPElement::list(
                self.constructors.borrow().iter().map(|c| c.to_pp_element(true)).collect(),
                PPElement::break_elem(0, 0, false),
                PPElement::text(";".to_string()),
                PPElement::break_elem(1, 4, true),
                BreakType::Flexible,
            );
            
            
            PPElement::group(vec![
                name,
                PPElement::text(" {".to_string()),
                PPElement::break_elem(1, 4, true),
                constructors,
                PPElement::break_elem(1, 0, true),
                PPElement::text("}".to_string())
            ], BreakType::Consistent, 0)
        } else {
            name
        }
    }
}

impl Hash for Algebra {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}