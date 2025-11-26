
use indexmap::{IndexMap, IndexSet};
use std::rc::Rc;
use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};

use crate::core::substitution::MatchOutput;
use crate::core::polarity::{Polarity, Polarized};

use crate::core::types::minlog_type::{TypeBody, MinlogType};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct TupleType {
    types: Vec<Rc<MinlogType>>,
}

impl TupleType {
    pub fn create(types: Vec<Rc<MinlogType>>) -> Rc<MinlogType> {
        if types.len() == 1 {
            return types[0].clone();
        }
        
        Rc::new(MinlogType::Tuple(TupleType { types }))
    }
    
    pub fn create_unit() -> Rc<MinlogType> {
        TupleType::create(vec![])
    }
    
    pub fn types(&self) -> &Vec<Rc<MinlogType>> {
        &self.types
    }
    
    pub fn type_at(&self, index: usize) -> Option<&Rc<MinlogType>> {
        self.types.get(index)
    }
}

impl TypeBody for TupleType {
    fn is_object_type(&self) -> bool {
        self.types.iter().all(|t| t.is_object_type())
    }
    
    fn level(&self) -> usize {
        self.types.iter().map(|t| t.level()).max().unwrap_or(0)
    }

    fn get_polarized_tvars(&self, current: Polarity, visited: &mut IndexSet<MinlogType>) -> IndexSet<Polarized<Rc<MinlogType>>> {
        self.types.iter()
            .flat_map(|t| t.get_polarized_tvars(current, visited))
            .collect()
    }

    fn get_polarized_algebras(&self, current: Polarity, visited: &mut IndexSet<MinlogType>) -> IndexSet<Polarized<Rc<MinlogType>>> {
        self.types.iter()
            .flat_map(|t| t.get_polarized_algebras(current, visited))
            .collect()
    }
    
    fn remove_nulls(&self) -> Option<Rc<MinlogType>> {
        let mut new_types = vec![];
        
        for t in &self.types {
            if let Some(new_t) = MinlogType::remove_nulls(t) {
                new_types.push(new_t);
            }
        }
        
        if new_types.is_empty() {
            None
        } else {
            Some(TupleType::create(new_types))
        }
    }

    fn substitute(&self, from: &Rc<MinlogType>, to: &Rc<MinlogType>) -> Rc<MinlogType> {
        if from.is_tuple() && self == from.to_tuple().unwrap() {
            to.clone()
        } else {
            let new_types: Vec<Rc<MinlogType>> = self.types.iter()
                .map(|t| t.substitute(from, to))
                .collect();
            
            TupleType::create(new_types)
        }
    }
    
    fn first_conflict_with(&self, other: &Rc<MinlogType>) -> Option<(Rc<MinlogType>, Rc<MinlogType>)> {
        if !other.is_tuple() {
            return Some((TupleType::create(self.types.clone()), other.clone()));
        }
        
        let other_tuple = other.to_tuple().unwrap();
        
        if self.types.len() != other_tuple.types.len() {
            return Some((TupleType::create(self.types.clone()), other.clone()));
        }
        
        for (t1, t2) in self.types.iter().zip(other_tuple.types.iter()) {
            if let Some(conflict) = t1.first_conflict_with(t2) {
                return Some(conflict);
            }
        }
        
        None
    }

    fn match_with(&self, instance: &Rc<MinlogType>) -> MatchOutput<Rc<MinlogType>> {
        if !instance.is_tuple() {
            return MatchOutput::FailedMatch;
        }
        
        let instance_tuple = instance.to_tuple().unwrap();

        if self.types.len() != instance_tuple.types.len() {
            return MatchOutput::FailedMatch;
        }
        
        let conditions = self.types.iter().cloned()
            .zip(instance_tuple.types.iter().cloned())
            .filter(|(t1, t2)| t1 != t2)
            .collect::<IndexMap<_, _>>();
        
        MatchOutput::Matched(conditions)
    }
}

impl PrettyPrintable for TupleType {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        PPElement::group(vec![
            PPElement::text("(".to_string()),
            PPElement::break_elem(1, 4, false),
            PPElement::list(
                self.types.iter().map(|t| t.to_enclosed_pp_element(detail)).collect(),
                PPElement::break_elem(0, 0, false),
                PPElement::text(",".to_string()),
                PPElement::break_elem(1, 4, false),
                BreakType::Flexible
            ),
            PPElement::break_elem(1, 0, false),
            PPElement::text(")".to_string())
        ], BreakType::Consistent, 0)
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}