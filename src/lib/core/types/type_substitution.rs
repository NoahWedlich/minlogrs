
use crate::includes::{
    essential::*,
    core::{
        types::*,
    }
};

impl Substitutable for Rc<MinlogType> {
    fn substitute(&self, from: &Self, to: &Self) -> Self {
        self.as_ref().substitute(from, to)
    }
    
    fn first_conflict_with(&self, other: &Self) -> Option<(Self, Self)> {
        self.as_ref().first_conflict_with(other)
    }
    
    fn valid_substitution(&self, to: &Self) -> bool {
        self.is_variable() && !to.contains_type_variable(self)
    }
    
    fn match_with(&self, instance: &Self) -> MatchOutput<Self> {
        self.as_ref().match_with(instance)
    }
}

pub type TypeSubstitution = Substitution<Rc<MinlogType>>;