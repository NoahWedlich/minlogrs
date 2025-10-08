
use std::{collections::HashMap, cell::RefCell, rc::Rc};
use crate::core::types::minlog_type::{TypeBody, MinlogType};
use crate::utils::indirect_ref::{IRef, IRefGroup, RefGroup};

#[derive(Debug)]
pub struct TypeVariable {
    name: String,
    group_map: RefCell<HashMap<usize, usize>>,
}

impl TypeVariable {
    pub fn create(name: &str, group: &IRefGroup<MinlogType>) -> IRef<MinlogType> {
        let variable = MinlogType::Variable(TypeVariable {
            name: name.to_string(),
            group_map: RefCell::new(HashMap::new()),
        });
        
        IRef::new(Rc::new(variable), group.clone())
    }
    
    pub fn name(&self) -> &str {
        &self.name
    }
    
    pub fn get_index(&self, group: &IRefGroup<MinlogType>) -> Option<usize> {
        let group_index = group.borrow().index();
        let index = self.group_map.borrow().get(&group_index).cloned();
        
        match index {
            Some(i) => {
                let element = group.borrow().get_from_index(i);

                match element {
                    Some(e)
                        if e.is_variable() && e.to_variable().unwrap() == self => Some(i),
                    _ => None,
                }
            }
            None => None,
        }
    }
}

impl TypeBody for TypeVariable {
    fn is_object_type(&self) -> bool {
        true
    }
    
    fn add_group(&self, wrapped: &MinlogType, group: &RefGroup<MinlogType>) {
        if !wrapped.is_variable() || wrapped.to_variable().unwrap() != self {
            panic!("Can only add group for self.");
        }
        
        let mut group_map = self.group_map.borrow_mut();
        group_map.insert(group.index(), group.get(wrapped).unwrap());
    }
    
    fn del_group(&self, wrapped: &MinlogType, group: &RefGroup<MinlogType>) {
        if !wrapped.is_variable() || wrapped.to_variable().unwrap() != self {
            panic!("Can only remove group for self.");
        }

        let mut group_map = self.group_map.borrow_mut();
        group_map.remove(&group.index());
    }
}

impl PartialEq for TypeVariable {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for TypeVariable {}