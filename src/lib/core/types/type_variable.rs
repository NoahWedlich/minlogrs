
use crate::includes::{
    essential::*,
    utils::*,
    core::{
        types::*,
    }
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

    fn get_polarized_tvars(&self, current: Polarity, _visited: &mut IndexSet<MinlogType>) -> IndexSet<Polarized<Rc<MinlogType>>> {
        IndexSet::from([Polarized::new(current, Rc::new(MinlogType::Variable(self.clone())))])
    }

    fn substitute(&self, from: &Rc<MinlogType>, to: &Rc<MinlogType>) -> Rc<MinlogType> {
        if from.is_variable() && self == from.to_variable().unwrap() {
            to.clone()
        } else {
            TypeVariable::create(self.name.clone())
        }
    }
    
    fn first_conflict_with(&self, other: &Rc<MinlogType>) -> Option<(Rc<MinlogType>, Rc<MinlogType>)> {
        if other.is_variable() && self == other.to_variable().unwrap() {
            None
        } else {
            Some((TypeVariable::create(self.name.clone()), other.clone()))
        }
    }

    fn match_with(&self, instance: &Rc<MinlogType>) -> MatchOutput<Rc<MinlogType>> {
        if !instance.contains_type_variable(&Rc::new(MinlogType::Variable(self.clone()))) {
            MatchOutput::Substitution(Rc::new(MinlogType::Variable(self.clone())), instance.clone())
        } else {
            MatchOutput::FailedMatch
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