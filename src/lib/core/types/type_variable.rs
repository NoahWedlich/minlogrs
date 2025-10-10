
use std::rc::Rc;
use crate::utils::pretty_printer::{PrettyPrintable, PrettyPrinter};
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
    fn pretty_print(&self, printer: &mut PrettyPrinter, _detail: bool) {
        let name = &self.name;
        crate::pretty_print!(printer, name);
    }
}

impl PartialEq for TypeVariable {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for TypeVariable {}