
use crate::includes::{
    essential::*,
    utils::*,
    core::{
        types::*,
        terms::*,
    }
};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Tuple {
    elements: Vec<Rc<MinlogTerm>>,
    minlog_type: Rc<MinlogType>,
}

impl Tuple {
    pub fn create(elements: Vec<Rc<MinlogTerm>>) -> Rc<MinlogTerm> {
        let element_types: Vec<Rc<MinlogType>> = elements.iter().map(|e| e.minlog_type()).collect();
        let minlog_type = TupleType::create(element_types);
        Rc::new(MinlogTerm::Tuple(Tuple { elements, minlog_type }))
    }
    
    pub fn elements(&self) -> &Vec<Rc<MinlogTerm>> {
        &self.elements
    }
    
    pub fn element(&self, index: usize) -> Option<&Rc<MinlogTerm>> {
        self.elements.get(index)
    }
}

impl TermBody for Tuple {
    fn minlog_type(&self) -> Rc<MinlogType> {
        self.minlog_type.clone()
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> Rc<MinlogTerm> {
        if eta {
            if self.elements.iter().all(|e| e.is_projection()) {
                println!("Warning: Eta-reduction for tuples of projections not implemented yet.");
            }
            
            if self.elements.iter().all(|e| e.is_match_term()) {
                println!("Warning: Eta-reduction for tuples of match terms not implemented yet.");
            }
        }
        
        let new_elements = self.elements.iter().map(|e| e.normalize(eta, pi)).collect();
        Tuple::create(new_elements)
    }
    
    fn remove_nulls(&self) -> Option<Rc<MinlogTerm>> {
        let new_elements = self.elements.iter()
            .filter_map(|e| e.remove_nulls())
            .collect::<Vec<_>>();
        
        if new_elements.is_empty() {
            None
        } else {
            Some(Tuple::create(new_elements))
        }
    }
    
    fn length(&self) -> usize {
        1 + self.elements.iter().map(|e| e.length()).sum::<usize>()
    }
    
    fn depth(&self) -> usize {
        1 + self.elements.iter().map(|e| e.depth()).max().unwrap_or(0)
    }
    
    fn get_type_variables(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogType>> {
        self.minlog_type.get_type_variables(&mut IndexSet::new())
    }
    
    fn get_algebra_types(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogType>> {
        self.minlog_type.get_algebra_types(&mut IndexSet::new())
    }
    
    fn get_free_variables(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogTerm>> {
        if visited.contains(&MinlogTerm::Tuple(self.clone())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogTerm::Tuple(self.clone()));
            self.elements.iter().flat_map(|e| e.get_free_variables(visited)).collect()
        }
    }

    fn get_bound_variables(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogTerm>> {
        if visited.contains(&MinlogTerm::Tuple(self.clone())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogTerm::Tuple(self.clone()));
            self.elements.iter().flat_map(|e| e.get_bound_variables(visited)).collect()
        }
    }

    fn get_constructors(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogTerm>> {
        if visited.contains(&MinlogTerm::Tuple(self.clone())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogTerm::Tuple(self.clone()));
            self.elements.iter().flat_map(|e| e.get_constructors(visited)).collect()
        }
    }

    fn get_program_terms(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogTerm>> {
        if visited.contains(&MinlogTerm::Tuple(self.clone())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogTerm::Tuple(self.clone()));
            self.elements.iter().flat_map(|e| e.get_program_terms(visited)).collect()
        }
    }

    fn get_internal_constants(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogTerm>> {
        if visited.contains(&MinlogTerm::Tuple(self.clone())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogTerm::Tuple(self.clone()));
            self.elements.iter().flat_map(|e| e.get_internal_constants(visited)).collect()
        }
    }
    
    fn alpha_equivalent(&self, other: &Rc<MinlogTerm>,
        forward: &mut Vec<(TermVariable, TermVariable)>,
        backward: &mut Vec<(TermVariable, TermVariable)>) -> bool {
        
        if !other.is_tuple() {
            return false;
        }
        
        let other = other.to_tuple().unwrap();
        
        if self.elements.len() != other.elements.len() {
            return false;
        }
        
        for (e1, e2) in self.elements.iter().zip(other.elements.iter()) {
            if !e1.alpha_equivalent(e2, forward, backward) {
                return false;
            }
        }
        
        true
    }
    
    fn substitute(&self, from: &TermSubstEntry, to: &TermSubstEntry) -> Rc<MinlogTerm> {
        if let Some(tm) = from.to_term() && tm.is_tuple() && self == tm.to_tuple().unwrap() {
            to.to_term().unwrap()
        } else {
            let new_elements = self.elements.iter().map(|e| e.substitute(from, to)).collect();
            Tuple::create(new_elements)
        }
    }
    
    fn first_conflict_with(&self, other: &Rc<MinlogTerm>) -> Option<(TermSubstEntry, TermSubstEntry)> {
        if let Some(conflict) = self.minlog_type.first_conflict_with(&other.minlog_type()) {
            return Some((conflict.0.into(), conflict.1.into()));
        }
        
        if !other.is_tuple() {
            return Some((Rc::new(MinlogTerm::Tuple(self.clone())).into(), other.clone().into()));
        }
        
        let other_tup = other.to_tuple().unwrap();

        if self.elements.len() != other_tup.elements.len() {
            return Some((Rc::new(MinlogTerm::Tuple(self.clone())).into(), other.clone().into()));
        }

        for (e1, e2) in self.elements.iter().zip(other_tup.elements.iter()) {
            if let Some(conflict) = e1.first_conflict_with(e2) {
                return Some(conflict);
            }
        }
        
        None
    }

    fn match_with(&self, instance: &Rc<MinlogTerm>) -> MatchOutput<TermSubstEntry> {
        if !instance.is_tuple() {
            return MatchOutput::FailedMatch;
        }
        
        let tup_instance = instance.to_tuple().unwrap();
        
        if self.elements.len() != tup_instance.elements.len() {
            return MatchOutput::FailedMatch;
        }
        
        let conditions = self.elements.iter().zip(tup_instance.elements.iter())
            .filter_map(|(e1, e2)| {
                if e1 != e2 {
                    Some((e1.into(), e2.into()))
                } else {
                    None
                }
            })
            .collect::<IndexMap<_, _>>();
        
        MatchOutput::Matched(conditions)
    }
}

impl PrettyPrintable for Tuple {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        let elements = PPElement::list(
            self.elements.iter().map(|e| e.minlog_type().to_pp_element(detail)).collect(),
            PPElement::break_elem(0, 4, false),
            PPElement::text(",".to_string()),
            PPElement::break_elem(1, 4, false),
            BreakType::Flexible
        );
        
        PPElement::group(vec![
            PPElement::text("(".to_string()),
            PPElement::break_elem(1, 4, false),
            elements,
            PPElement::break_elem(1, 0, false),
            PPElement::text(")".to_string())
        ], BreakType::Consistent, 0)
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}