
use crate::includes::{
    essential::*,
    utils::*,
    core::{
        types::*,
    }
};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct PairType {
    left: Arc<MinlogType>,
    right: Arc<MinlogType>,
}

impl PairType {
    pub fn create(left: Arc<MinlogType>, right: Arc<MinlogType>) -> Arc<MinlogType> {
        Arc::new(MinlogType::Pair(PairType { left, right }))
    }
    
    pub fn create_nested(elements: Vec<Arc<MinlogType>>) -> Arc<MinlogType> {
        if elements.is_empty() {
            TypeConstant::create_unit()
        } else if elements.len() == 1 {
            elements[0].clone()
        } else {
            PairType::create(elements[0].clone(), PairType::create_nested(elements[1..].to_vec()))
        }
    }

    pub fn left(&self) -> &Arc<MinlogType> {
        &self.left
    }
    
    pub fn right(&self) -> &Arc<MinlogType> {
        &self.right
    }
    
    pub fn nested_types(&self) -> Vec<Arc<MinlogType>> {
        let mut types = vec![self.left.clone()];
        let mut current_right = self.right.clone();
        
        while let Some(pair) = current_right.to_pair() {
            types.push(pair.left.clone());
            current_right = pair.right.clone();
        }
        
        types.push(current_right);
        types
    }
}

impl TypeBody for PairType {
    fn is_object_type(&self) -> bool {
        self.left().is_object_type() && self.right().is_object_type()
    }
    
    fn level(&self) -> usize {
        self.left().level().max(self.right().level())
    }

    fn get_polarized_tvars(&self, current: Polarity, visited: &mut IndexSet<MinlogType>) -> IndexSet<Polarized<Arc<MinlogType>>> {
        self.left.get_polarized_tvars(current, visited)
            .into_iter()
            .chain(self.right.get_polarized_tvars(current, visited))
            .collect()
    }

    fn get_polarized_algebras(&self, current: Polarity, visited: &mut IndexSet<MinlogType>) -> IndexSet<Polarized<Arc<MinlogType>>> {
        self.left.get_polarized_algebras(current, visited)
            .into_iter()
            .chain(self.right.get_polarized_algebras(current, visited))
            .collect()
    }
    
    fn remove_nulls(&self) -> Option<Arc<MinlogType>> {
        let new_left = self.left.remove_nulls();
        let new_right = self.right.remove_nulls();
        
        match (new_left, new_right) {
            (Some(l), Some(r)) => Some(PairType::create(l, r)),
            (Some(l), None) => Some(l),
            (None, Some(r)) => Some(r),
            (None, None) => None,
        }
    }

    fn substitute(&self, from: &Arc<MinlogType>, to: &Arc<MinlogType>) -> Arc<MinlogType> {
        if from.is_pair() && self == from.to_pair().unwrap() {
            to.clone()
        } else {
            let new_left = self.left.substitute(from, to);
            let new_right = self.right.substitute(from, to);
            PairType::create(new_left, new_right)
        }
    }
    
    fn first_conflict_with(&self, other: &Arc<MinlogType>) -> Option<(Arc<MinlogType>, Arc<MinlogType>)> {
        if !other.is_pair() {
            return Some((PairType::create(self.left.clone(), self.right.clone()), other.clone()));
        }
        
        let other_pair = other.to_pair().unwrap();
        
        if let Some(conflict) = self.left.first_conflict_with(&other_pair.left) {
            return Some(conflict);
        }
        
        if let Some(conflict) = self.right.first_conflict_with(&other_pair.right) {
            return Some(conflict);
        }
        
        None
    }

    fn match_with(&self, instance: &Arc<MinlogType>) -> MatchOutput<Arc<MinlogType>> {
        if !instance.is_pair() {
            return MatchOutput::FailedMatch;
        }
        
        let instance_pair = instance.to_pair().unwrap();
        
        let conditions = IndexMap::from([
            (self.left.clone(), instance_pair.left.clone()),
            (self.right.clone(), instance_pair.right.clone()),
        ]);
        
        MatchOutput::Matched(conditions)
    }
}

impl PrettyPrintable for PairType {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        PPElement::group(vec![
            self.left.to_pp_element(detail),
            PPElement::break_elem(1, 4, false),
            PPElement::text("x".to_string()),
            PPElement::break_elem(1, 0, false),
            self.right.to_pp_element(detail),
        ], BreakType::Consistent, 0)
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}