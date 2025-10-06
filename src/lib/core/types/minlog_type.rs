
use std::{cell::RefCell, rc::Rc, fmt::Debug};
use crate::utils::indirect_ref::{IRef, RefGroup, RefGroupable};

use crate::core::types::type_variable::TypeVariable;

crate::wrapper_enum! {
    
    @default { EmptyTypeBody }
    pub trait TypeBody: Debug {
        fn is_object_type(&Self) -> bool {
            false
        }
        
        fn arity(&Self) -> usize {
            0
        }
        
        fn level(&Self) -> usize {
            0
        }
        
        fn inner_type_variables(&Self) -> Vec<IRef<MinlogType>> {
            vec![]
        }
        
        fn inner_algebra_types(&Self) -> Vec<IRef<MinlogType>> {
            vec![]
        }
        
        fn add_group(&Self, _group: &RefGroup<MinlogType>) {}
        
        fn del_group(&Self, _group: &RefGroup<MinlogType>) {}
        
        fn copy_if_needed(&Self, _group: &Rc<RefCell<RefGroup<MinlogType>>>,
            _elements: &mut Vec<IRef<MinlogType>>) -> Option<Rc<MinlogType>> {
            None
        }
    }
    
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum MinlogType {
        NullType(),
        Atomic(),
        Existential(),
        Proposition(),
        Variable(|variable| TypeVariable),
        Algebra(|algebra|),
        Arrow(|arrow|),
        Star(|star|)
    }
    
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EmptyTypeBody;
impl TypeBody for EmptyTypeBody {}

impl MinlogType {
    pub fn is_null(&self) -> bool {
        matches!(self, MinlogType::NullType(_))
    }
    
    pub fn is_atomic(&self) -> bool {
        matches!(self, MinlogType::Atomic(_))
    }
    
    pub fn is_existential(&self) -> bool {
        matches!(self, MinlogType::Existential(_))
    }
    
    pub fn is_proposition(&self) -> bool {
        matches!(self, MinlogType::Proposition(_))
    }
    
    pub fn is_constant(&self) -> bool {
        matches!(self, MinlogType::NullType(_) | MinlogType::Atomic(_)
            | MinlogType::Existential(_) | MinlogType::Proposition(_))
    }
    
    pub fn is_variable(&self) -> bool {
        matches!(self, MinlogType::Variable(_))
    }
    
    pub fn is_algebra(&self) -> bool {
        matches!(self, MinlogType::Algebra(_))
    }
    
    pub fn is_arrow(&self) -> bool {
        matches!(self, MinlogType::Arrow(_))
    }
    
    pub fn is_star(&self) -> bool {
        matches!(self, MinlogType::Star(_))
    }
    
    pub fn is_ground_type(&self) -> bool {
        matches!(self, MinlogType::NullType(_)| MinlogType::Atomic(_)| MinlogType::Existential(_)
            | MinlogType::Proposition(_)| MinlogType::Variable(_)| MinlogType::Algebra(_))
    }
    
    pub fn get_type_variables(minlog_type: &IRef<MinlogType>) -> Vec<IRef<MinlogType>> {
        let mut inner = minlog_type.inner_type_variables();
        
        if minlog_type.is_variable() {
            inner.push(minlog_type.clone());
        }
        
        inner
    }

    pub fn contains_type_variable(&self, var: &IRef<MinlogType>) -> bool {
        var.is_variable() && MinlogType::get_type_variables(var).contains(var)
    }
    
    pub fn get_algebra_types(minlog_type: &IRef<MinlogType>) -> Vec<IRef<MinlogType>> {
        let mut inner = minlog_type.inner_algebra_types();
        
        if minlog_type.is_algebra() {
            inner.push(minlog_type.clone());
        }
        
        inner
    }

    pub fn contains_algebra_type(&self, alg: &IRef<MinlogType>) -> bool {
        alg.is_algebra() && MinlogType::get_algebra_types(alg).contains(alg)
    }
}

impl RefGroupable for MinlogType {
    fn add_group(&self, _group: &RefGroup<Self>) {
        self.add_group(_group);
    }
    
    fn del_group(&self, _group: &RefGroup<Self>) {
        self.del_group(_group);
    }
    
    fn copy_if_needed(&self, _group: &Rc<RefCell<RefGroup<Self>>>, _elements: &mut Vec<IRef<Self>>) -> Option<Rc<Self>> {
        self.copy_if_needed(_group, _elements)
    }
}