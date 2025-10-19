
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
        
        for tvar in constructor.minlog_type().get_type_variables() {
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
        let tvars = self.get_type_variables();
        
        let name = if tvars.is_empty() {
            PPElement::text(self.name.clone())
        } else {
            let mut tvarlist = vec![];
            for (i, tvar) in tvars.iter().enumerate() {
                tvarlist.push(
                    if i < tvars.len() - 1 {
                        PPElement::group(vec![
                            tvar.to_pp_element(detail),
                            PPElement::break_elem(0, 4, false),
                            PPElement::text(",".to_string())
                        ], BreakType::Flexible, 0)
                    } else {
                        tvar.to_pp_element(detail)
                    }
                );
                
                if i < tvars.len() - 1 {
                    tvarlist.push(PPElement::break_elem(1, 4, false));
                }
            }
            
            PPElement::group(vec![
                PPElement::text(self.name.clone()),
                PPElement::text("<".to_string()),
                PPElement::break_elem(1, 4, false),
                PPElement::group(tvarlist, BreakType::Flexible, 0),
                PPElement::break_elem(1, 0, false),
                PPElement::text(">".to_string())
            ], BreakType::Consistent, 0)
        };
        
        if detail {
            let mut constructors = vec![];
            for (i, c) in self.constructors.borrow().iter().enumerate() {
                constructors.push(
                    if i < self.constructors.borrow().len() - 1 {
                        PPElement::group(vec![
                            c.to_pp_element(true),
                            PPElement::text(";".to_string()),
                            PPElement::break_elem(1, 4, true)
                        ], BreakType::Flexible, 0)
                    } else {
                        PPElement::group(vec![
                            c.to_pp_element(true),
                            PPElement::text(";".to_string())
                        ], BreakType::Flexible, 0)
                    }
                );
            }
            
            
            PPElement::group(vec![
                name,
                PPElement::text(" {".to_string()),
                PPElement::break_elem(1, 4, true),
                PPElement::group(constructors, BreakType::Flexible, 0),
                PPElement::break_elem(1, 0, true),
                PPElement::text("}".to_string())
            ], BreakType::Consistent, 0)
        } else {
            name
        }
    }
}