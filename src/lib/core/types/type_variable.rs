
use std::{collections::HashMap, cell::RefCell, rc::Rc};
use crate::core::types::minlog_type::{TypeBody, MinlogType};
use crate::utils::indirect_ref::{IRef, IRefGroup, RefGroup};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeVariable {
    name: String,
    group_index: RefCell<HashMap<usize, usize>>,
}

impl TypeVariable {
    pub fn create(name: &str, group: &IRefGroup<MinlogType>) -> IRef<MinlogType> {
        let variable = MinlogType::Variable(TypeVariable {
            name: name.to_string(),
            group_index: RefCell::new(HashMap::new()),
        });
        
        IRef::new(Rc::new(variable), group.clone())
    }
    
    pub fn name(&self) -> &str {
        &self.name
    }
}

impl TypeBody for TypeVariable {
    fn is_object_type(&self) -> bool {
        true
    }
    
    fn add_group(&self, group: &RefGroup<MinlogType>) {
        let mut group_index = self.group_index.borrow_mut();
        group_index.insert(group.index(), group.get(&MinlogType::Variable(self.clone())).unwrap());
    }
    
    fn del_group(&self, group: &RefGroup<MinlogType>) {
        let mut group_index = self.group_index.borrow_mut();
        group_index.remove(&group.index());
    }
}