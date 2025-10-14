
use std::rc::Rc;
use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};

use crate::core::substitution::MatchContext;

use crate::core::types::minlog_type::MinlogType;

use crate::core::terms::minlog_term::{TermBody, MinlogTerm, Totality};
use crate::core::terms::term_variable::TermVariable;

use crate::core::terms::term_substitution::TermSubstEntry;

#[derive(Clone, PartialEq, Eq)]
pub struct Projection {
    term: Rc<MinlogTerm>,
    index: usize,
    minlog_type: Rc<MinlogType>,
}

impl Projection {
    pub fn create(term: Rc<MinlogTerm>, index: usize) -> Rc<MinlogTerm> {
        if !term.minlog_type().is_star() {
            panic!("Tried to create projection from non-tuple term.");
        }
        
        if index >= term.minlog_type().to_star().unwrap().types().len() {
            panic!("Tried to create projection with out-of-bounds index.");
        }
        
        let minlog_type = term.minlog_type().to_star().unwrap().type_at(index).unwrap().clone();
        Rc::new(MinlogTerm::Projection(Projection { term, index, minlog_type }))
    }
    
    pub fn term(&self) -> &Rc<MinlogTerm> {
        &self.term
    }
    
    pub fn index(&self) -> usize {
        self.index
    }
}

impl TermBody for Projection {
    fn minlog_type(&self) -> Rc<MinlogType> {
        self.minlog_type.clone()
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> Rc<MinlogTerm> {
        if self.term.is_tuple() {
            let tuple = self.term.to_tuple().unwrap();
            if self.index < tuple.elements().len() {
                return tuple.element(self.index).unwrap().normalize(eta, pi);
            } else {
                panic!("Projection index out of bounds in normalization.");
            }
        }
        
        let new_term = self.term.normalize(eta, pi);
        Projection::create(new_term, self.index)
    }
    
    fn length(&self) -> usize {
        1 + self.term.length()
    }
    
    fn depth(&self) -> usize {
        1 + self.term.depth()
    }
    
    fn inner_free_variables(&self) -> Vec<Rc<MinlogTerm>> {
        MinlogTerm::get_free_variables(&self.term)
    }
    
    fn inner_bound_variables(&self) -> Vec<Rc<MinlogTerm>> {
        MinlogTerm::get_bound_variables(&self.term)
    }
    
    fn inner_constructors(&self) -> Vec<Rc<MinlogTerm>> {
        MinlogTerm::get_constructors(&self.term)
    }
    
    fn inner_program_terms(&self) -> Vec<Rc<MinlogTerm>> {
        MinlogTerm::get_program_terms(&self.term)
    }
    
    fn inner_internal_constants(&self) -> Vec<Rc<MinlogTerm>> {
        MinlogTerm::get_internal_constants(&self.term)
    }
    
    fn alpha_equivalent(&self, other: &Rc<MinlogTerm>,
        forward: &mut Vec<(TermVariable, TermVariable)>,
        backward: &mut Vec<(TermVariable, TermVariable)>) -> bool {
        
        if !other.is_projection() {
            return false;
        }
        
        let other = other.to_projection().unwrap();
        
        self.index == other.index && self.term.alpha_equivalent(&other.term, forward, backward)
    }
    
    fn totality(&self, bound: &mut Vec<TermVariable>) -> Totality {
        self.term.totality(bound)
    }
    
    fn substitute(&self, from: &TermSubstEntry, to: &TermSubstEntry) -> Rc<MinlogTerm> {
        let new_term = self.term.substitute(from, to);
        Projection::create(new_term, self.index)
    }
    
    fn first_conflict_with(&self, other: &Rc<MinlogTerm>) -> Option<(Rc<MinlogTerm>, Rc<MinlogTerm>)> {
        if !other.is_projection() {
            return Some((Rc::new(MinlogTerm::Projection(self.clone())), other.clone()));
        }
        
        let other_proj = other.to_projection().unwrap();
        
        if self.index != other_proj.index {
            return Some((Rc::new(MinlogTerm::Projection(self.clone())), other.clone()));
        }
        
        self.term.first_conflict_with(&other_proj.term)
    }
    
    fn match_with(&self, ctx: &mut impl MatchContext<TermSubstEntry>) -> Result<Option<(TermSubstEntry, TermSubstEntry)>, ()> {
        let pattern = ctx.next_pattern().unwrap();
        let instance = ctx.next_instance().unwrap();
        
        match (pattern, instance) {
            (TermSubstEntry::Term(p), TermSubstEntry::Term(i)) => {
                if !p.is_projection() || !i.is_projection() {
                    return Err(());
                }
                
                let proj_pattern = p.to_projection().unwrap();
                let proj_instance = i.to_projection().unwrap();
                
                if proj_pattern.index != proj_instance.index {
                    return Err(());
                }
                
                ctx.extend(&TermSubstEntry::Term(proj_pattern.term.clone()), &TermSubstEntry::Term(proj_instance.term.clone()));
                Ok(None)
            },
            _ => Err(())
        }
    }
}

impl PrettyPrintable for Projection {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        PPElement::group(vec![
            self.term.to_enclosed_pp_element(detail),
            PPElement::break_elem(0, 0, false),
            PPElement::text("_".to_string()),
            PPElement::break_elem(0, 0, false),
            PPElement::text(self.index.to_string()),
        ], BreakType::Flexible, 0)
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        true
    }
    
    fn open_paren(&self) -> String {
        "(".to_string()
    }
    
    fn close_paren(&self) -> String {
        ")".to_string()
    }
}