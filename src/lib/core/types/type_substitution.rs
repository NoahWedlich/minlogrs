
use std::rc::Rc;
use crate::core::substitution::{Substitutable, Substitution, MatchContext, MatchContextImpl, MatchOutput};
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
    
    fn match_with(&self, ctx: &mut impl MatchContext<Self>) -> MatchOutput<Self> {
        self.as_ref().match_with(ctx)
    }
}

pub type TypeSubstitution = Substitution<Rc<MinlogType>>;
pub type TypeMatchContext = MatchContextImpl<Rc<MinlogType>>;