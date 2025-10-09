
use std::{rc::Rc, fmt::Debug};
use crate::utils::pretty_printer::{PrettyPrintable, PrettyPrinter};

use crate::core::types::type_variable::TypeVariable;

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

crate::wrapper_enum! {
    
    @default { TypeConstant }
    pub trait TypeBody: PrettyPrintable {
        fn is_object_type(&Self) -> bool {
            false
        }
        
        fn arity(&Self) -> usize {
            0
        }
        
        fn level(&Self) -> usize {
            0
        }
        
        fn inner_type_variables(&Self) -> Vec<Rc<MinlogType>> {
            vec![]
        }
        
        fn inner_algebra_types(&Self) -> Vec<Rc<MinlogType>> {
            vec![]
        }
    }
    
    #[derive(PartialEq, Eq)]
    pub enum MinlogType {
        NullType(|null|),
        Atomic(|atomic|),
        Existential(|existential|),
        Proposition(|proposition|),
        Variable(||variable||),
        Algebra(||algebra||),
        Arrow(||arrow||),
        Star(||star||),
    }
    
    impl PrettyPrintable {
        fn pretty_print(&Self, printer: &mut PrettyPrinter, detail: bool);

        fn requires_parens(&Self, _detail: bool) -> bool;

        fn open_paren(&Self) -> String;

        fn close_paren(&Self) -> String;
    }
    
}

impl MinlogType {
    pub fn is_constant(&self) -> bool {
        matches!(self, MinlogType::NullType(_)
            | MinlogType::Atomic(_)
            | MinlogType::Existential(_)
            | MinlogType::Proposition(_))
    }
    
    pub fn is_ground_type(&self) -> bool {
        self.is_constant() || matches!(self, MinlogType::Variable(_) | MinlogType::Algebra(_))
    }
    
    pub fn get_type_variables(minlog_type: &Rc<MinlogType>) -> Vec<Rc<MinlogType>> {
        let mut inner = minlog_type.inner_type_variables();
        
        if minlog_type.is_variable() {
            inner.push(minlog_type.clone());
        }
        
        inner
    }
    
    pub fn contains_type_variable(minlog_type: &Rc<MinlogType>, var: &Rc<MinlogType>) -> bool {
        var.is_variable() && MinlogType::get_type_variables(minlog_type).contains(var)
    }
    
    pub fn get_algebra_types(minlog_type: &Rc<MinlogType>) -> Vec<Rc<MinlogType>> {
        let mut inner = minlog_type.inner_algebra_types();
        
        if minlog_type.is_algebra() {
            inner.push(minlog_type.clone());
        }
        
        inner
    }
    
    pub fn contains_algebra_type(minlog_type: &Rc<MinlogType>, alg: &Rc<MinlogType>) -> bool {
        alg.is_algebra() && MinlogType::get_algebra_types(minlog_type).contains(alg)
    }
}