
use std::rc::Rc;
use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};
use crate::core::types::minlog_type::{TypeBody, MinlogType};

pub struct StarType {
    types: Vec<Rc<MinlogType>>,
}

impl StarType {
    pub fn create(types: Vec<Rc<MinlogType>>) -> Rc<MinlogType> {
        Rc::new(MinlogType::Star(StarType { types }))
    }
    
    pub fn types(&self) -> &Vec<Rc<MinlogType>> {
        &self.types
    }
    
    pub fn type_at(&self, index: usize) -> Option<&Rc<MinlogType>> {
        self.types.get(index)
    }
}

impl TypeBody for StarType {
    fn is_object_type(&self) -> bool {
        self.types.iter().all(|t| t.is_object_type())
    }
    
    fn level(&self) -> usize {
        self.types.iter().map(|t| t.level()).max().unwrap_or(0)
    }
    
    fn inner_type_variables(&self) -> Vec<Rc<MinlogType>> {
        let mut inner = vec![];
        
        for t in &self.types {
            inner.extend(t.inner_type_variables());
        }
        
        inner
    }
    
    fn inner_algebra_types(&self) -> Vec<Rc<MinlogType>> {
        let mut inner = vec![];
        
        for t in &self.types {
            inner.extend(t.inner_algebra_types());
        }
        
        inner
    }
    
    fn remove_nulls(minlog_type: &StarType) -> Option<Rc<MinlogType>> {
        let mut new_types = vec![];
        
        for t in &minlog_type.types {
            if let Some(new_t) = MinlogType::remove_nulls(t) {
                new_types.push(new_t);
            }
        }
        
        if new_types.is_empty() {
            None
        } else {
            Some(StarType::create(new_types))
        }
    }
}

impl PrettyPrintable for StarType {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        let mut elements = vec![];
        
        for (i, arg_type) in self.types.iter().enumerate() {
            elements.push(
                if i > 0 {
                    PPElement::group(
                        vec![
                            PPElement::text("*".to_string()),
                            PPElement::break_elem(1, 4, false),
                            arg_type.to_enclosed_pp_element(detail),
                        ], BreakType::Flexible, 0)
                } else {
                    arg_type.to_enclosed_pp_element(detail)
                }
            );
            
            if i + 1 < self.types.len() {
                elements.push(PPElement::break_elem(1, 4, false));
            }
        }
        
        PPElement::group(elements, BreakType::Flexible, 0)
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        true
    }
    
    fn open_paren(&self) -> String {
        "(".to_string()
    }
    
    fn close_paren(&self) -> String {
        ")".to_string()
    }
}

impl PartialEq for StarType {
    fn eq(&self, other: &Self) -> bool {
        self.types == other.types
    }
}

impl Eq for StarType {}