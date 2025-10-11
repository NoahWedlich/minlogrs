
use std::rc::Rc;
use crate::utils::pretty_printer::PrettyPrintable;
use crate::core::substitution::{Substitutable, Substitution, MatchContext};
use crate::core::types::minlog_type::MinlogType;

#[derive(Clone, PartialEq, Eq)]
pub struct TypeSubstitution {
    pairs: Vec<(Rc<MinlogType>, Rc<MinlogType>)>,
}

impl TypeSubstitution {
    pub fn from_pairs(pairs: Vec<(Rc<MinlogType>, Rc<MinlogType>)>) -> Self {
        for (from, to) in pairs.clone() {
            if !from.valid_substitution(&to) {
                panic!("Invalid substitution from {} to {}", from.debug_string(), to.debug_string());
            }
        }

        Self { pairs }
    }
}

impl Substitution for TypeSubstitution {
    
    type ElementType = Rc<MinlogType>;
    type ContextType = TypeMatchContext;
    
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

#[derive(Clone, PartialEq, Eq)]
pub struct TypeMatchContext {
    patterns: Vec<Rc<MinlogType>>,
    instances: Vec<Rc<MinlogType>>,
    restricted: Vec<Rc<MinlogType>>,
}

impl MatchContext<Rc<MinlogType>> for TypeMatchContext {
    fn new(patterns: &mut Vec<Rc<MinlogType>>, instances: &mut Vec<Rc<MinlogType>>) -> Self {
        Self {
            patterns: patterns.drain(..).collect(),
            instances: instances.drain(..).collect(),
            restricted: vec![],
        }
    }
    
    fn extend(&mut self, pattern: &Rc<MinlogType>, instance: &Rc<MinlogType>) {
        self.patterns.push(pattern.clone());
        self.instances.push(instance.clone());
    }
    
    fn next_pattern(&mut self) -> Option<Rc<MinlogType>> {
        self.patterns.last().cloned()
    }
    
    fn next_instance(&mut self) -> Option<Rc<MinlogType>> {
        self.instances.last().cloned()
    }
    
    fn skip_current(&mut self) {
        self.patterns.pop();
        self.instances.pop();
    }
    
    fn restrict(&mut self, element: &Rc<MinlogType>) {
        if !element.is_variable() {
            panic!("Can only restrict type variables.");
        }
        
        self.restricted.push(element.clone());
    }
    
    fn is_restricted(&self, element: &Rc<MinlogType>) -> bool {
        if element.is_variable() {
            self.restricted.iter().any(|r| r.eq(element))
        } else {
            false
        }
    }
    
    fn substitute(&mut self, from: &Rc<MinlogType>, to: &Rc<MinlogType>) {
        for pattern in self.patterns.iter_mut() {
            *pattern = pattern.substitute(from, to);
        }
        
        for instance in self.instances.iter_mut() {
            *instance = instance.substitute(from, to);
        }
    }
}

impl Substitutable<TypeMatchContext> for Rc<MinlogType> {
    fn substitute(&self, from: &Self, to: &Self) -> Self {
        self.as_ref().substitute(from, to)
    }
    
    fn first_conflict_with(&self, other: &Self) -> Option<(Self, Self)> {
        self.as_ref().first_conflict_with(other)
    }
    
    fn valid_substitution(&self, to: &Self) -> bool {
        self.is_variable() && !MinlogType::contains_type_variable(to, self)
    }
    
    fn match_with(&self, ctx: &mut TypeMatchContext) -> Result<Option<(Self, Self)>, ()> {
        self.as_ref().match_with(ctx)
    }
}