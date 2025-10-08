
use std::{cell::RefCell, rc::Rc, fmt::Debug, cmp::max};
use crate::core::types::minlog_type::{TypeBody, MinlogType};
use crate::utils::indirect_ref::{IRef, IRefGroup, RefGroup};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrowType {
    arguments: Vec<IRef<MinlogType>>,
    value: IRef<MinlogType>,
}

impl ArrowType {
    pub fn create(arguments: Vec<IRef<MinlogType>>, value: IRef<MinlogType>, group: &IRefGroup<MinlogType>) -> IRef<MinlogType> {
        let arrow = MinlogType::Arrow(ArrowType {
            arguments: arguments.iter().map(|arg| arg.copy_to(group)).collect(),
            value: value.copy_to(group),
        });

        ArrowType::collapse(&IRef::new(Rc::new(arrow), group.clone()))
    }
    
    pub fn collapse(minlog_type: &IRef<MinlogType>) -> IRef<MinlogType> {
        if minlog_type.is_decoupled() {
            panic!("Cannot collapse decoupled type.");
        }
        
        if minlog_type.is_arrow() && minlog_type.to_arrow().unwrap().value().is_arrow() {
            let mut arrow = minlog_type.to_arrow().unwrap();
            let mut arguments = arrow.arguments().clone();
            
            while arrow.value().is_arrow() {
                let inner_arrow = arrow.value().to_arrow().unwrap();
                arguments.extend(inner_arrow.arguments().clone());
                arrow = inner_arrow;
            }
            
            let new_arrow = ArrowType::create(arguments, arrow.value().clone(),
                &minlog_type.group().unwrap());
            minlog_type.redirect(new_arrow);
        }
        
        minlog_type.clone()
    }
    
    pub fn remove_nulls(minlog_type: &IRef<MinlogType>) -> IRef<MinlogType> {
        if minlog_type.is_decoupled() {
            panic!("Cannot remove nulls from decoupled type.");
        }
        
        if minlog_type.is_arrow() {
            ArrowType::collapse(minlog_type);
            
            let arrow = minlog_type.to_arrow().unwrap();
            let arguments: Vec<IRef<MinlogType>> = arrow.arguments().iter()
                .filter(|arg| {
                    !arg.is_null() && !(
                        arg.is_arrow() && arg.to_arrow().unwrap().value().is_null()
                    )
                }).cloned().collect();
                
            if arguments.len() != arrow.arguments().len() {
                let new_arrow = ArrowType::create(arguments, arrow.value().clone(),
                    &minlog_type.group().unwrap());
                minlog_type.redirect(new_arrow);
            }
        }

        minlog_type.clone()
    }
    
    pub fn argument(&self, index: usize) -> Option<&IRef<MinlogType>> {
        self.arguments.get(index)
    }
    
    pub fn arguments(&self) -> &Vec<IRef<MinlogType>> {
        &self.arguments
    }
    
    pub fn value(&self) -> &IRef<MinlogType> {
        &self.value
    }
}

impl TypeBody for ArrowType {
    fn is_object_type(&self) -> bool {
        self.arguments.iter().all(|arg| arg.is_object_type()) && self.value.is_object_type()
    }
    
    fn arity(&self) -> usize {
        self.arguments.len() + self.value.arity()
    }
    
    fn level(&self) -> usize {
        max(self.arguments.iter().map(|arg| arg.level()).max().unwrap_or(0) + 1, self.value.level())
    }
    
    fn inner_type_variables(&self) -> Vec<IRef<MinlogType>> {
        let mut inner = Vec::new();
        
        for arg in &self.arguments {
            inner.extend(arg.inner_type_variables());
        }
        
        inner.extend(self.value.inner_type_variables());
        inner
    }
    
    fn inner_algebra_types(&self) -> Vec<IRef<MinlogType>> {
        let mut inner = Vec::new();
        
        for arg in &self.arguments {
            inner.extend(arg.inner_algebra_types());
        }
        
        inner.extend(self.value.inner_algebra_types());
        inner
    }
    
    fn copy_if_needed(self: &Self, group: &Rc<RefCell<RefGroup<MinlogType>>>,
        elements: &mut Vec<IRef<MinlogType>>) -> Option<IRef<MinlogType>> {
            
        let mut changed = false;
        
        let copied_arguments: Vec<IRef<MinlogType>> = self.arguments.iter()
            .map(|arg| {
                if let Some(copied) = arg.shallow_copy_to(group) {
                    elements.push(copied.clone());
                    changed = true;
                    copied
                } else {
                    arg.clone()
                }
            }).collect();
            
        let copied_value = if let Some(copied) = self.value.shallow_copy_to(group) {
            elements.push(copied.clone());
            changed = true;
            copied
        } else {
            self.value.clone()
        };
        
        if changed {
            Some(ArrowType::create(copied_arguments, copied_value, &IRefGroup::from(group.clone())))
        } else {
            None
        }
    }
}