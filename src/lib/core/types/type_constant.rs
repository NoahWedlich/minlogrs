
use std::rc::Rc;
use crate::utils::pretty_printer::{PrettyPrintable, PrettyPrinter};
use crate::core::types::minlog_type::{TypeBody, MinlogType};

#[derive(Debug, PartialEq, Eq)]
pub enum TypeConstant {
    NullType,
    Atomic,
    Existential,
    Proposition,
}

impl TypeConstant {
    pub fn create_null() -> Rc<MinlogType> {
        Rc::new(MinlogType::NullType(TypeConstant::NullType))
    }
    
    pub fn create_atomic() -> Rc<MinlogType> {
        Rc::new(MinlogType::Atomic(TypeConstant::Atomic))
    }
    
    pub fn create_existential() -> Rc<MinlogType> {
        Rc::new(MinlogType::Existential(TypeConstant::Existential))
    }
    
    pub fn create_proposition() -> Rc<MinlogType> {
        Rc::new(MinlogType::Proposition(TypeConstant::Proposition))
    }
}

impl PrettyPrintable for TypeConstant {
    fn pretty_print(&self, printer: &mut PrettyPrinter, _detail: bool) {
        match self {
            TypeConstant::NullType => crate::pretty_print!(printer, "Null"),
            TypeConstant::Atomic => crate::pretty_print!(printer, "Atomic"),
            TypeConstant::Existential => crate::pretty_print!(printer, "Existential"),
            TypeConstant::Proposition => crate::pretty_print!(printer, "Proposition"),
        };
    }
}

impl TypeBody for TypeConstant {}