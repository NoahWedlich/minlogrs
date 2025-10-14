
use std::rc::Rc;
use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};

use crate::core::substitution::MatchContext;

use crate::core::types::minlog_type::MinlogType;
use crate::core::types::star_type::StarType;

use crate::core::terms::minlog_term::{TermBody, MinlogTerm, Totality};
use crate::core::terms::term_variable::TermVariable;

use crate::core::terms::term_substitution::TermSubstEntry;

#[derive(Clone, PartialEq, Eq)]
pub struct Tuple {
    elements: Vec<Rc<MinlogTerm>>,
    minlog_type: Rc<MinlogType>,
}

impl Tuple {
    pub fn create(elements: Vec<Rc<MinlogTerm>>) -> Rc<MinlogTerm> {
        let element_types: Vec<Rc<MinlogType>> = elements.iter().map(|e| e.minlog_type()).collect();
        let minlog_type = StarType::create(element_types);
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
            
            if self.elements.iter().all(|e| e.is_conditional()) {
                println!("Warning: Eta-reduction for tuples of conditionals not implemented yet.");
            }
        }
        
        let new_elements = self.elements.iter().map(|e| e.normalize(eta, pi)).collect();
        Tuple::create(new_elements)
    }
    
    fn length(&self) -> usize {
        1 + self.elements.iter().map(|e| e.length()).sum::<usize>()
    }
    
    fn depth(&self) -> usize {
        1 + self.elements.iter().map(|e| e.depth()).max().unwrap_or(0)
    }
    
    fn inner_free_variables(&self) -> Vec<Rc<MinlogTerm>> {
        let mut inner = vec![];
        
        for element in &self.elements {
            inner.extend(MinlogTerm::get_free_variables(element));
        }
        
        inner
    }
    
    fn inner_bound_variables(&self) -> Vec<Rc<MinlogTerm>> {
        let mut inner = vec![];
        
        for element in &self.elements {
            inner.extend(MinlogTerm::get_bound_variables(element));
        }
        
        inner
    }
    
    fn inner_constructors(&self) -> Vec<Rc<MinlogTerm>> {
        let mut inner = vec![];
        
        for element in &self.elements {
            inner.extend(MinlogTerm::get_constructors(element));
        }
        
        inner
    }
    
    fn inner_program_terms(&self) -> Vec<Rc<MinlogTerm>> {
        let mut inner = vec![];
        
        for element in &self.elements {
            inner.extend(MinlogTerm::get_program_terms(element));
        }
        
        inner
    }
    
    fn inner_internal_constants(&self) -> Vec<Rc<MinlogTerm>> {
        let mut inner = vec![];
        
        for element in &self.elements {
            inner.extend(MinlogTerm::get_internal_constants(element));
        }
        
        inner
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
    
    fn totality(&self, bound: &mut Vec<TermVariable>) -> Totality {
        if self.elements.iter().any(|e| e.totality(bound) == Totality::Partial) {
            Totality::Partial
        } else {
            Totality::Total
        }
    }
    
    fn substitute(&self, from: &TermSubstEntry, to: &TermSubstEntry) -> Rc<MinlogTerm> {
        let new_elements = self.elements.iter().map(|e| e.substitute(from, to)).collect();
        Tuple::create(new_elements)
    }
    
    fn first_conflict_with(&self, other: &Rc<MinlogTerm>) -> Option<(Rc<MinlogTerm>, Rc<MinlogTerm>)> {
        if !other.is_tuple() {
            return Some((Rc::new(MinlogTerm::Tuple(self.clone())), other.clone()));
        }
        
        let other_tup = other.to_tuple().unwrap();

        if self.elements.len() != other_tup.elements.len() {
            return Some((Rc::new(MinlogTerm::Tuple(self.clone())), other.clone()));
        }

        for (e1, e2) in self.elements.iter().zip(other_tup.elements.iter()) {
            if let Some(conflict) = e1.first_conflict_with(e2) {
                return Some(conflict);
            }
        }
        
        None
    }
    
    fn match_with(&self, ctx: &mut impl MatchContext<TermSubstEntry>) -> Result<Option<(TermSubstEntry,TermSubstEntry)>, ()> {
        let pattern = ctx.next_pattern().unwrap();
        let instance = ctx.next_instance().unwrap();
        
        match (pattern, instance) {
            (TermSubstEntry::Term(p), TermSubstEntry::Term(i)) => {
                if !p.is_tuple() || !i.is_tuple() {
                    return Err(());
                }
                
                if p.minlog_type() != i.minlog_type() {
                    ctx.extend(&TermSubstEntry::Type(p.minlog_type()), &TermSubstEntry::Type(i.minlog_type()));
                    ctx.extend(&TermSubstEntry::Term(p.clone()), &TermSubstEntry::Term(i.clone()));
                    return Ok(None);
                }
                
                let tup_pattern = p.to_tuple().unwrap();
                let tup_instance = i.to_tuple().unwrap();
                
                if tup_pattern.elements.len() != tup_instance.elements.len() {
                    return Err(());
                }
                
                for (e1, e2) in tup_pattern.elements.iter().zip(tup_instance.elements.iter()) {
                    ctx.extend(&TermSubstEntry::Term(e1.clone()), &TermSubstEntry::Term(e2.clone()));
                }
                Ok(None)
            },
            _ => Err(())
        }
        
        
    }
}

impl PrettyPrintable for Tuple {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        let mut elements = vec![];
        elements.push(PPElement::text("(".to_string()));
        elements.push(PPElement::break_elem(1, 4, false));
        
        for (i, element) in self.elements.iter().enumerate() {
            elements.push(
                if i < self.elements.len() - 1 {
                    PPElement::group(vec![
                        element.to_pp_element(detail),
                        PPElement::break_elem(0, 4, false),
                        PPElement::text(",".to_string()),
                    ], BreakType::Flexible, 0)
                } else {
                    element.to_pp_element(detail)
                }
            );
            
            elements.push(PPElement::break_elem(1, 4, false));
        }

        elements.push(PPElement::text(")".to_string()));
        
        PPElement::group(elements, BreakType::Consistent, 0)
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}