
use std::rc::Rc;
use crate::utils::pretty_printer::{PrettyPrintable, PPElement};
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
    fn to_pp_element(&self, _detail: bool) -> PPElement {
        match self {
            TypeConstant::NullType => PPElement::text("null".to_string()),
            TypeConstant::Atomic => PPElement::text("atomic".to_string()),
            TypeConstant::Existential => PPElement::text("existential".to_string()),
            TypeConstant::Proposition => PPElement::text("proposition".to_string()),
        }
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}

impl TypeBody for TypeConstant {
    fn remove_nulls(minlog_type: &TypeConstant) -> Option<Rc<MinlogType>> {
        match minlog_type {
            TypeConstant::NullType => None,
            TypeConstant::Atomic => Some(TypeConstant::create_atomic()),
            TypeConstant::Existential => Some(TypeConstant::create_existential()),
            TypeConstant::Proposition => Some(TypeConstant::create_proposition()),
        }
    }
}