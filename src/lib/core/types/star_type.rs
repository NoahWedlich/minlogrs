
use std::{cell::RefCell, rc::Rc, fmt::Debug, cmp::max};
use crate::core::types::minlog_type::{TypeBody, MinlogType};
use crate::utils::indirect_ref::{IRef, IRefGroup, RefGroup};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StarType {
    left: IRef<MinlogType>,
    right: IRef<MinlogType>,
}

impl StarType {
    pub fn create(left: IRef<MinlogType>, right: IRef<MinlogType>, group: &IRefGroup<MinlogType>) -> IRef<MinlogType> {
        let star = MinlogType::Star(StarType {
            left,
            right,
        });

        IRef::new(Rc::new(star), group.clone())
    }
    
    pub fn left(&self) -> &IRef<MinlogType> {
        &self.left
    }
    
    pub fn right(&self) -> &IRef<MinlogType> {
        &self.right
    }
}

impl TypeBody for StarType {
    fn is_object_type(&self) -> bool {
        self.left.is_object_type() && self.right.is_object_type()
    }
    
    fn level(&self) -> usize {
        max(self.left.level(), self.right.level())
    }

    fn inner_type_variables(&self) -> Vec<IRef<MinlogType>> {
        let mut vars = self.left.inner_type_variables();
        vars.extend(self.right.inner_type_variables());
        vars
    }
    
    fn inner_algebra_types(&self) -> Vec<IRef<MinlogType>> {
        let mut algs = self.left.inner_algebra_types();
        algs.extend(self.right.inner_algebra_types());
        algs
    }
    
    fn copy_if_needed(self: &Self, group: &Rc<RefCell<RefGroup<MinlogType>>>,
        elements: &mut Vec<IRef<MinlogType>>) -> Option<IRef<MinlogType>> {
        
        let mut changed = false;
        
        let left_copied = if let Some(copied) = self.left.shallow_copy_to(group) {
            elements.push(copied.clone());
            changed = true;
            copied
        } else {
            self.left.clone()
        };
        
        let right_copied = if let Some(copied) = self.right.shallow_copy_to(group) {
            elements.push(copied.clone());
            changed = true;
            copied
        } else {
            self.right.clone()
        };
        
        if changed {
            Some(StarType::create(left_copied, right_copied, group))
        } else {
            None
        }
    }
}