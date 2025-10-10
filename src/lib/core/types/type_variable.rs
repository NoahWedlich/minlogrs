
use std::rc::Rc;
use crate::utils::pretty_printer::{PrettyPrintable, PPElement};
use crate::core::types::minlog_type::{TypeBody, MinlogType};

pub struct TypeVariable {
    name: String
}

impl TypeVariable {
    pub fn create(name: String) -> Rc<MinlogType> {
        Rc::new(MinlogType::Variable(TypeVariable { name }))
    }
    
    pub fn name(&self) -> &str {
        &self.name
    }
}

impl TypeBody for TypeVariable {
    fn is_object_type(&self) -> bool {
        true
    }
    
    fn remove_nulls(minlog_type: &TypeVariable) -> Option<Rc<MinlogType>> {
        Some(TypeVariable::create(minlog_type.name.clone()))
    }
}

impl PrettyPrintable for TypeVariable {
    fn to_pp_element(&self, _detail: bool) -> PPElement {
        PPElement::text(self.name.clone())
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}

impl PartialEq for TypeVariable {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for TypeVariable {}