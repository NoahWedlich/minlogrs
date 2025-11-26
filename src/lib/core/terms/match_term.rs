
use crate::includes::{
    essential::*,
    utils::*,
    core::{
        types::*,
        terms::*,
    }
};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct MatchTerm {
    minlog_type: Rc<MinlogType>,
    branches: Vec<(Rc<MinlogTerm>, Rc<MinlogTerm>)>,
}

// TODO: Allow anonymous recursion
impl MatchTerm {
    pub fn create(minlog_type: Rc<MinlogType>, branches: Vec<(Rc<MinlogTerm>, Rc<MinlogTerm>)>) -> Rc<MinlogTerm> {
        if let Some(arr_type) = minlog_type.to_arrow() {
            let argument_type = arr_type.arguments()[0].clone();
            let return_type = ArrowType::create(
                arr_type.arguments()[1..].to_vec(),
                arr_type.value().clone(),
            );
            
            for (pattern, result) in branches.iter() {
                if pattern.minlog_type() != argument_type {
                    panic!("Match term branch pattern type does not match match term argument type");
                }
                
                if result.minlog_type() != return_type {
                    panic!("Match term branch result type does not match match term return type");
                }
            }
            
            Rc::new(MinlogTerm::MatchTerm(MatchTerm {
                minlog_type,
                branches,
            }))
        } else {
            panic!("Match term must have an arrow type");
        }
    }
    
    pub fn branches(&self) -> &Vec<(Rc<MinlogTerm>, Rc<MinlogTerm>)> {
        &self.branches
    }
    
    pub fn compute(&self, term: &Rc<MinlogTerm>) -> (Rc<MinlogTerm>, bool) {
        for (pattern, result) in self.branches.iter() {
            if let Some(subst) = TermSubstitution::match_with(&pattern.clone().into(), &term.clone().into()) {
                return (subst.substitute::<TermSubstEntry>(&result.clone().into()).to_term().unwrap(), true);
            }
        }
        
        (term.clone(), false)
    }
}

impl TermBody for MatchTerm {
    fn minlog_type(&self) -> Rc<MinlogType> {
        self.minlog_type.clone()
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> Rc<MinlogTerm> {
        let normalized_branches = self.branches.iter().map(|(pattern, result)| {
            (
                pattern.normalize(eta, pi),
                result.normalize(eta, pi)
            )
        }).collect::<Vec<_>>();
        
        MatchTerm::create(self.minlog_type.clone(), normalized_branches)
    }
    
    fn remove_nulls(&self) -> Option<Rc<MinlogTerm>> {
        let new_branches = self.branches.iter().filter_map(|(pattern, result)| {
            let new_pattern = pattern.remove_nulls();
            let new_result = result.remove_nulls();
            
            if new_pattern.is_none() || new_result.is_none() {
                return None;
            }
            
            Some((new_pattern.unwrap(), new_result.unwrap()))
        }).collect::<Vec<_>>();
        
        if new_branches.is_empty() {
            None
        } else {
            Some(MatchTerm::create(self.minlog_type.clone(), new_branches))
        }
    }
    
    fn length(&self) -> usize {
        1
    }
    
    fn depth(&self) -> usize {
        0
    }
    
    fn get_type_variables(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogType>> {
        if visited.contains(&MinlogTerm::MatchTerm(self.clone())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogTerm::MatchTerm(self.clone()));
            
            self.branches.iter()
                .flat_map(|(p, i)|
                    p.get_type_variables(visited)
                        .union(&i.get_type_variables(visited))
                        .cloned().collect::<IndexSet<_>>())
                .collect()
        }
    }
    
    fn get_algebra_types(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogType>> {
        if visited.contains(&MinlogTerm::MatchTerm(self.clone())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogTerm::MatchTerm(self.clone()));
            
            self.branches.iter()
                .flat_map(|(p, i)| {
                    p.get_algebra_types(visited)
                        .union(&i.get_algebra_types(visited))
                        .cloned().collect::<IndexSet<_>>()
                })
                .collect()
        }
    }
    
    fn get_free_variables(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogTerm>> {
        if visited.contains(&MinlogTerm::MatchTerm(self.clone())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogTerm::MatchTerm(self.clone()));
            
            self.branches.iter()
                .flat_map(|(p, i)| {
                    p.get_free_variables(visited)
                        .union(&i.get_free_variables(visited))
                        .cloned().collect::<IndexSet<_>>()
                })
                .collect()
        }
    }
    
    fn get_bound_variables(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogTerm>> {
        if visited.contains(&MinlogTerm::MatchTerm(self.clone())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogTerm::MatchTerm(self.clone()));
            
            self.branches.iter()
                .flat_map(|(p, i)| {
                    p.get_bound_variables(visited)
                        .union(&i.get_bound_variables(visited))
                        .cloned().collect::<IndexSet<_>>()
                })
                .collect()
        }
    }
    
    fn get_constructors(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogTerm>> {
        if visited.contains(&MinlogTerm::MatchTerm(self.clone())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogTerm::MatchTerm(self.clone()));
            
            self.branches.iter()
                .flat_map(|(p, i)| {
                    p.get_constructors(visited)
                        .union(&i.get_constructors(visited))
                        .cloned().collect::<IndexSet<_>>()
                })
                .collect()
        }
    }
    
    fn get_program_terms(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogTerm>> {
        if visited.contains(&MinlogTerm::MatchTerm(self.clone())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogTerm::MatchTerm(self.clone()));
            
            self.branches.iter()
                .flat_map(|(p, i)| {
                    p.get_program_terms(visited)
                        .union(&i.get_program_terms(visited))
                        .cloned().collect::<IndexSet<_>>()
                })
                .collect()
        }
    }
    
    fn get_internal_constants(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogTerm>> {
        if visited.contains(&MinlogTerm::MatchTerm(self.clone())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogTerm::MatchTerm(self.clone()));
            
            self.branches.iter()
                .flat_map(|(p, i)| {
                    p.get_internal_constants(visited)
                        .union(&i.get_internal_constants(visited))
                        .cloned().collect::<IndexSet<_>>()
                })
                .collect()
        }
    }
    
    fn alpha_equivalent(&self, other: &Rc<MinlogTerm>,
        forward: &mut Vec<(TermVariable, TermVariable)>,
        backward: &mut Vec<(TermVariable, TermVariable)>) -> bool
    {
        if let Some(other_match) = other.to_match_term() {
            let mut remaining_other_branches = other_match.branches.clone();
            for (pattern, instance) in self.branches.iter() {
                let mut found_equivalent = false;
                for (i, (other_pattern, other_instance)) in remaining_other_branches.iter().enumerate() {
                    if pattern.alpha_equivalent(other_pattern, forward, backward)
                        && instance.alpha_equivalent(other_instance, forward, backward)
                    {
                        found_equivalent = true;
                        remaining_other_branches.remove(i);
                        break;
                    }
                }
                
                if !found_equivalent {
                    return false;
                }
            }
            
            remaining_other_branches.is_empty()
        } else {
            false
        }
    }
    
    fn substitute(&self, from: &TermSubstEntry, to: &TermSubstEntry) -> Rc<MinlogTerm> {
        if let Some(tm) = from.to_term() && let Some(match_term) = tm.to_match_term() && self == match_term {
            to.to_term().unwrap()
        } else {
            let substituted_branches = self.branches.iter().map(|(pattern, instance)| {
                (
                    pattern.substitute(from, to),
                    instance.substitute(from, to)
                )
            }).collect::<Vec<_>>();
            
            MatchTerm::create(self.minlog_type.clone(), substituted_branches)
        }
    }
    
    fn first_conflict_with(&self, other: &Rc<MinlogTerm>) -> Option<(TermSubstEntry, TermSubstEntry)> {
        if let Some(other_match) = other.to_match_term() {
            if self.branches.len() != other_match.branches.len() {
                return Some((
                    TermSubstEntry::Term(Rc::new(MinlogTerm::MatchTerm(self.clone()))),
                    TermSubstEntry::Term(other.clone())
                ));
            }
            
            for ((pattern, instance), (other_pattern, other_instance))
                in self.branches.iter().zip(other_match.branches.iter())
            {
                if let Some(conflict) = pattern.first_conflict_with(other_pattern) {
                    return Some(conflict);
                } else if let Some(conflict) = instance.first_conflict_with(other_instance) {
                    return Some(conflict);
                }
            }
            
            None
        } else {
            Some((
                TermSubstEntry::Term(Rc::new(MinlogTerm::MatchTerm(self.clone()))),
                TermSubstEntry::Term(other.clone())
            ))
        }
    }
    
    fn match_with(&self, instance: &Rc<MinlogTerm>) -> MatchOutput<TermSubstEntry> {
        if !instance.is_match_term() {
            return MatchOutput::FailedMatch;
        }
        
        let match_instance = instance.to_match_term().unwrap();
        
        if self.branches.len() != match_instance.branches.len() {
            return MatchOutput::FailedMatch;
        }
        
        let conditions = self.branches.iter().zip(match_instance.branches.iter())
            .flat_map(|((p_pattern, i_pattern), (p_instance, i_instance))| {
                let mut cond = vec![];
                
                if p_pattern != p_instance {
                    cond.push((p_pattern.into(), p_instance.into()));
                }
                
                if i_pattern != i_instance {
                    cond.push((i_pattern.into(), i_instance.into()));
                }
                
                cond
            }
        ).collect::<IndexMap<_, _>>();
        
        MatchOutput::Matched(conditions)
    }
}

impl PrettyPrintable for MatchTerm {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        let name = if detail {
            PPElement::group(vec![
                PPElement::text("match:".to_string()),
                PPElement::break_elem(1, 4, false),
                self.minlog_type.to_pp_element(detail),
            ], BreakType::Flexible, 0)
        } else {
            PPElement::text("match".to_string())
        };
        
        let branches = PPElement::list(
            self.branches.iter().map(|(pattern, instance)| {
                PPElement::group(vec![
                    pattern.to_pp_element(detail),
                    PPElement::break_elem(1, 4, false),
                    PPElement::text("->".to_string()),
                    PPElement::break_elem(1, 4, false),
                    instance.to_pp_element(detail),
                ], BreakType::Flexible, 0)
            }).collect(),
            PPElement::break_elem(0, 0, false),
            PPElement::text(",".to_string()),
            PPElement::break_elem(1, 0, false),
            BreakType::Flexible,
        );
        
        PPElement::group(vec![
            name,
            PPElement::text(" {".to_string()),
            PPElement::break_elem(1, 4, false),
            branches,
            PPElement::break_elem(1, 0, false),
            PPElement::text("}".to_string()),
        ], BreakType::Consistent, 0)
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}