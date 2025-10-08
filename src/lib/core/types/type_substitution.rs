
use crate::utils::indirect_ref::IRef;
use crate::core::substitution::{Substitutable, Substitution, MatchContext};
use crate::core::types::minlog_type::MinlogType;

impl Substitutable<TypeMatchContext> for IRef<MinlogType> {
    fn substitute(&self, from: &Self, to: &Self) -> Self {
        if !from.is_variable() {
            panic!("Can only substitute type variables.");
        }
        
        match self {
            IRef::Indirect(group, _) => {
                let var = from.to_variable().unwrap();
                let index = var.get_index(&group);
                
                if index.is_none() {
                    return self.clone();
                }
                
                let i_var = IRef::from_group(
                    group.clone(),
                    index.unwrap(),
                );
                
                i_var.redirect(to.clone());
                self.clone()
            }
            IRef::Direct(_) => {
                panic!("Cannot substitute in decoupled type.");
            }
        }
    }
    
    fn first_conflict_with(&self, _other: &Self) -> Option<(Self, Self)> {
        todo!()
    }
    
    fn valid_substitution(&self, to: &Self) -> bool {
        self.is_variable() && !MinlogType::contains_type_variable(to, self)
    }
    
    fn match_with(&self, _other: &Self, _ctx: &mut TypeMatchContext) -> Option<(&Self, &Self)> {
        todo!()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeSubstitution {
    pairs: Vec<(IRef<MinlogType>, IRef<MinlogType>)>,
}

impl Substitution<IRef<MinlogType>, TypeMatchContext> for TypeSubstitution {
    fn make_empty() -> Self {
        Self {
            pairs: vec![],
        }
    }
    
    fn pairs(&self) -> &Vec<(IRef<MinlogType>, IRef<MinlogType>)> {
        &self.pairs
    }
    
    fn pairs_mut(&mut self) -> &mut Vec<(IRef<MinlogType>, IRef<MinlogType>)> {
        &mut self.pairs
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeMatchContext {
    patterns: Vec<IRef<MinlogType>>,
    instances: Vec<IRef<MinlogType>>,
    restricted: Vec<IRef<MinlogType>>,
}

impl MatchContext<IRef<MinlogType>> for TypeMatchContext {
    fn new(patterns: &mut Vec<IRef<MinlogType>>, instances: &mut Vec<IRef<MinlogType>>) -> Self {
        Self {
            patterns: patterns.drain(..).collect(),
            instances: instances.drain(..).collect(),
            restricted: vec![],
        }
    }
    
    fn next_pattern(&mut self) -> Option<IRef<MinlogType>> {
        self.patterns.last().cloned()
    }
    
    fn next_instance(&mut self) -> Option<IRef<MinlogType>> {
        self.instances.last().cloned()
    }
    
    fn skip_current(&mut self) {
        self.patterns.pop();
        self.instances.pop();
    }
    
    fn restrict(&mut self, element: &IRef<MinlogType>) {
        if !element.is_variable() {
            panic!("Can only restrict type variables.");
        }
        
        self.restricted.push(element.clone());
    }
    
    fn is_restricted(&self, element: &IRef<MinlogType>) -> bool {
        if element.is_variable() {
            self.restricted.iter().any(|r| r.eq(element))
        } else {
            false
        }
    }
    
    fn substitute(&mut self, from: &IRef<MinlogType>, to: &IRef<MinlogType>) {
        for pattern in self.patterns.iter_mut() {
            *pattern = pattern.substitute(from, to);
        }
        
        for instance in self.instances.iter_mut() {
            *instance = instance.substitute(from, to);
        }
    }
}