
use std::{rc::Rc, cell::RefCell};
use crate::utils::pretty_printer::*;

use crate::core::types::minlog_type::MinlogType;

use crate::core::terms::minlog_term::MinlogTerm;

#[derive(Clone, PartialEq, Eq)]
pub struct Algebra {
    name: String,
    constructors: RefCell<Vec<Rc<MinlogTerm>>>,
    type_variables: Vec<Rc<MinlogType>>,
}

impl Algebra {
    pub fn create(name: String, type_variables: Vec<Rc<MinlogType>>) -> Rc<Algebra> {
        Rc::new(Algebra { name, constructors: RefCell::new(vec![]), type_variables })
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
        
        for tvar in constructor.get_type_variables() {
            if !self.type_variables.contains(&tvar) {
                panic!("Constructor type contains type variable '{}' not in the algebra's type variables", tvar.debug_string());
            }
        }
        
        self.constructors.borrow_mut().push(constructor);
    }
    
    pub fn get_type_variables(&self) -> &Vec<Rc<MinlogType>> {
        &self.type_variables
    }
}

impl PrettyPrintable for Algebra {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        let name = if self.get_type_variables().is_empty() {
            PPElement::text(self.name.clone())
        } else {
            let tvars = PPElement::list(
                self.get_type_variables().iter().map(|tv| tv.to_pp_element(detail)).collect(),
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
                PPElement::break_elem(0, 4, false),
                PPElement::text(",".to_string()),
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