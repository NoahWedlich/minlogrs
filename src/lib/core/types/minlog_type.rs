
use std::{rc::Rc, fmt::Debug};
use crate::utils::pretty_printer::{PrettyPrintable, PrettyPrinter};

#[derive(Debug, PartialEq, Eq)]
pub struct EmptyTypeBody {}

impl PrettyPrintable for EmptyTypeBody {
    fn pretty_print(&self, printer: &mut PrettyPrinter, _detail: bool) {
        crate::pretty_print!(printer, "()");
    }
}

impl TypeBody for EmptyTypeBody {}

crate::wrapper_enum! {
    
    @default { EmptyTypeBody }
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

        fn open_paren(&Self) -> &'static str;

        fn close_paren(&Self) -> &'static str;
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