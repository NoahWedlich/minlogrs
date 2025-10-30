
use std::{rc::Rc, cell::RefCell, hash::{Hash, Hasher}, collections::HashSet};
use crate::utils::pretty_printer::*;

use crate::core::polarity::{Polarity, Polarized};

use crate::core::types::minlog_type::MinlogType;

use crate::core::terms::minlog_term::MinlogTerm;

#[derive(Clone, PartialEq, Eq)]
pub struct Algebra {
    name: String,
    constructors: RefCell<Vec<Rc<MinlogTerm>>>,
}

impl Algebra {
    pub fn create(name: String) -> Rc<Algebra> {
        Rc::new(Algebra { name, constructors: RefCell::new(vec![]) })
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
    
    pub fn get_polarized_tvars(&self, current: Polarity) -> HashSet<Polarized<Rc<MinlogType>>> {
        self.constructors.borrow().iter().flat_map(|constructor| {
            constructor.minlog_type().get_polarized_tvars(current)
        }).collect()
    }
    
    pub fn get_polarized_algebras(&self, current: Polarity) -> HashSet<Polarized<Rc<MinlogType>>> {
        self.constructors.borrow().iter().flat_map(|constructor| {
            constructor.minlog_type().get_polarized_algebras(current)
        }).collect()
    }
}

impl PrettyPrintable for Algebra {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        let tvars = self.get_polarized_tvars(Polarity::Unknown)
            .into_iter().map(|p| p.value).collect::<HashSet<_>>();
        
        let has_tvars = !tvars.is_empty();
        
        let name = if !has_tvars {
            PPElement::text(self.name.clone())
        } else {
            let tvars = PPElement::list(
                self.get_polarized_tvars(Polarity::Unknown).iter().map(|pol| pol.value.to_pp_element(detail)).collect(),
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