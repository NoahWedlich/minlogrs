
use std::rc::Rc;
use crate::core::substitution::{MatchContext, Substitutable};
use crate::utils::pretty_printer::{PrettyPrintable, PPElement};
use crate::core::types::minlog_type::{TypeBody, MinlogType};
use crate::core::types::type_substitution::TypeMatchContext;

#[derive(Clone)]
pub struct TypeVariable {
    name: String
}

impl TypeVariable {
    pub fn create(name: String) -> Rc<MinlogType> {
        Rc::new(MinlogType::Variable(TypeVariable { name }))
    }
    
    pub fn name(&self) -> &str {
        &self.name
    }
}

impl TypeBody for TypeVariable {
    fn is_object_type(&self) -> bool {
        true
    }
    
    fn remove_nulls(&self) -> Option<Rc<MinlogType>> {
        Some(TypeVariable::create(self.name.clone()))
    }
    
    fn substitute(self: &Self, from: &Rc<MinlogType>, to: &Rc<MinlogType>) -> Rc<MinlogType> {
        if from.is_variable() && self == from.to_variable().unwrap() {
            to.clone()
        } else {
            TypeVariable::create(self.name.clone())
        }
    }
    
    fn first_conflict_with(self: &Self, other: &Rc<MinlogType>) -> Option<(Rc<MinlogType>, Rc<MinlogType>)> {
        if other.is_variable() && self == other.to_variable().unwrap() {
            None
        } else {
            Some((TypeVariable::create(self.name.clone()), other.clone()))
        }
    }
    
    fn match_with(self: &Self, ctx: &mut TypeMatchContext) -> Result<Option<(Rc<MinlogType>, Rc<MinlogType>)>, ()> {
        let pattern = ctx.next_pattern().unwrap();
        let instance = ctx.next_instance().unwrap();
        
        if pattern.valid_substitution(&instance) {
            Ok(Some((pattern.clone(), instance.clone())))
        } else {
            Err(())
        }
    }
}

impl PrettyPrintable for TypeVariable {
    fn to_pp_element(&self, _detail: bool) -> PPElement {
        PPElement::text(self.name.clone())
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}

impl PartialEq for TypeVariable {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for TypeVariable {}