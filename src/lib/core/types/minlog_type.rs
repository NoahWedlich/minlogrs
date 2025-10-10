
use std::rc::Rc;
use crate::utils::pretty_printer::{PrettyPrintable, PPElement};

use crate::core::types::type_constant::TypeConstant;
use crate::core::types::type_variable::TypeVariable;
use crate::core::types::arrow_type::ArrowType;
use crate::core::types::star_type::StarType;

crate::wrapper_enum! {
    
    @default { TypeConstant }
    pub trait TypeBody: PrettyPrintable {
        pub fn is_object_type(&Self) -> bool {
            false
        }
        
        pub fn arity(&Self) -> usize {
            0
        }
        
        pub fn level(&Self) -> usize {
            0
        }
        
        pub fn inner_type_variables(&Self) -> Vec<Rc<MinlogType>> {
            vec![]
        }
        
        pub fn inner_algebra_types(&Self) -> Vec<Rc<MinlogType>> {
            vec![]
        }
    }
    
    @pass_through {
        fn remove_nulls(minlog_type: &Self) -> Option<Rc<MinlogType>>;
    }
    
    #[derive(PartialEq, Eq)]
    pub enum MinlogType {
        NullType(|null|),
        Atomic(|atomic|),
        Existential(|existential|),
        Proposition(|proposition|),
        Variable(||variable|| TypeVariable),
        Algebra(||algebra||),
        Arrow(||arrow|| ArrowType),
        Star(||star|| StarType),
    }
    
    @forward {
        pub fn remove_nulls(minlog_type: &Rc<MinlogType>) -> Option<Rc<MinlogType>> {
            @match {minlog_type.as_ref()}
            @method {remove_nulls()}
        }
    }
    
    impl PrettyPrintable {
        fn to_pp_element(&Self, detail: bool) -> PPElement;

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