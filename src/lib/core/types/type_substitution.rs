
use std::rc::Rc;
use crate::core::substitution::{Substitutable, Substitution, MatchContext};
use crate::core::types::minlog_type::MinlogType;

impl Substitutable for Rc<MinlogType> {
    fn substitute(&self, from: &Self, to: &Self) -> Self {
        self.as_ref().substitute(from, to)
    }
    
    fn first_conflict_with(&self, other: &Self) -> Option<(Self, Self)> {
        self.as_ref().first_conflict_with(other)
    }
    
    fn valid_substitution(&self, to: &Self) -> bool {
        self.is_variable() && !MinlogType::contains_type_variable(to, self)
    }
    
    fn match_with(&self, ctx: &mut impl MatchContext<Self>) -> Result<Option<(Self, Self)>, ()> {
        self.as_ref().match_with(ctx)
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct TypeSubstitution {
    pairs: Vec<(Rc<MinlogType>, Rc<MinlogType>)>,
}

impl Substitution for TypeSubstitution {
    
    type ElementType = Rc<MinlogType>;
    
    fn make_empty() -> Self {
        Self {
            pairs: vec![],
        }
    }
    
    fn pairs(&self) -> &Vec<(Rc<MinlogType>, Rc<MinlogType>)> {
        &self.pairs
    }
    
    fn pairs_mut(&mut self) -> &mut Vec<(Rc<MinlogType>, Rc<MinlogType>)> {
        &mut self.pairs
    }
}