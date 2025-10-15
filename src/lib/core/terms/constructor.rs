
use std::rc::Rc;
use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};

use crate::core::substitution::MatchContext;

use crate::core::types::minlog_type::MinlogType;

use crate::core::terms::minlog_term::{TermBody, MinlogTerm, Totality};
use crate::core::terms::term_variable::TermVariable;

use crate::core::terms::term_substitution::TermSubstEntry;

#[derive(Clone, PartialEq, Eq)]
pub struct Constructor {
    name: String,
    minlog_type: Rc<MinlogType>,
}

impl Constructor {
    pub fn create(name: String, minlog_type: Rc<MinlogType>) -> Rc<MinlogTerm> {
        if name.is_empty() {
            panic!("Constructor name cannot be empty");
        }
        
        if !minlog_type.is_algebra() && (!minlog_type.is_arrow() ||
            !minlog_type.to_arrow().unwrap().value().is_algebra()) {
            panic!("Constructor type must be an algebra type or an arrow type ending in an algebra type");
        }
        
        Rc::new(MinlogTerm::Constructor(Constructor {
            name,
            minlog_type,
        }))
    }
    
    pub fn name(&self) -> &String {
        &self.name
    }
}

impl TermBody for Constructor {
    fn minlog_type(&self) -> Rc<MinlogType> {
        self.minlog_type.clone()
    }
    
    fn normalize(&self, _eta: bool, _pi: bool) -> Rc<MinlogTerm> {
        Constructor::create(self.name.clone(), self.minlog_type.clone())
    }
    
    fn length(&self) -> usize {
        1
    }
    
    fn depth(&self) -> usize {
        0
    }
    
    fn constructor_pattern(&self) -> bool {
        self.minlog_type.is_algebra()
    }
    
    fn alpha_equivalent(&self, other: &Rc<MinlogTerm>,
        _forward: &mut Vec<(TermVariable, TermVariable)>,
        _backward: &mut Vec<(TermVariable, TermVariable)>) -> bool {
        
        if let Some(other_body) = other.to_constructor() {
            self.name == other_body.name && self.minlog_type == other_body.minlog_type
        } else {
            false
        }
    }
    
    fn totality(&self, _bound: &mut Vec<TermVariable>) -> Totality {
        Totality::Total
    }
    
    fn substitute(&self, from: &TermSubstEntry, to: &TermSubstEntry) -> Rc<MinlogTerm> {
        match (from, to) {
            (TermSubstEntry::Type(from_t), TermSubstEntry::Type(to_t)) => {
                Constructor::create(self.name.clone(), self.minlog_type.substitute(from_t, to_t))
            },
            (TermSubstEntry::Term(_), TermSubstEntry::Term(_)) => {
                Constructor::create(self.name.clone(), self.minlog_type.clone())
            },
            _ => {
                panic!("Tried to substitute between incompatible TermSubstEntry types");
            }
        }
    }
    
    fn first_conflict_with(&self, other: &Rc<MinlogTerm>) -> Option<(Rc<MinlogTerm>, Rc<MinlogTerm>)> {
        if !other.is_constructor() {
            return Some((Rc::new(MinlogTerm::Constructor(self.clone())), other.clone()));
        }
        
        let other_constr = other.to_constructor().unwrap();
        
        if self.name != other_constr.name || self.minlog_type != other_constr.minlog_type {
            return Some((Rc::new(MinlogTerm::Constructor(self.clone())), other.clone()));
        }
        None
    }
    
    fn match_with(&self, ctx: &mut impl MatchContext<TermSubstEntry>) -> Result<Option<(TermSubstEntry, TermSubstEntry)>, ()> {
        let pattern = ctx.next_pattern().unwrap();
        let instance = ctx.next_instance().unwrap();
        
        match (pattern, instance) {
            (TermSubstEntry::Term(p), TermSubstEntry::Term(i)) => {
                if !p.is_constructor() || !i.is_constructor() {
                    return Err(());
                }
                
                let constr_pattern = p.to_constructor().unwrap();
                let constr_instance = i.to_constructor().unwrap();
                
                if constr_pattern.name != constr_instance.name {
                    return Err(());
                }
                
                if constr_pattern.minlog_type != constr_instance.minlog_type {
                    ctx.extend(&TermSubstEntry::Type(constr_pattern.minlog_type.clone()), &TermSubstEntry::Type(constr_instance.minlog_type.clone()));
                }
                Ok(None)
            },
            _ => Err(())
        }
    }
}

impl PrettyPrintable for Constructor {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        if detail {
            PPElement::group(vec![
                PPElement::text(self.name.clone()),
                PPElement::text(":".to_string()),
                PPElement::break_elem(1, 4, false),
                self.minlog_type.to_enclosed_pp_element(true),
            ], BreakType::Flexible, 0)
        } else {
            PPElement::text(self.name.clone())
        }
    }
    
    fn requires_parens(&self, detail: bool) -> bool {
        detail
    }
    
    fn open_paren(&self) -> String {
        "(".to_string()
    }
    
    fn close_paren(&self) -> String {
        ")".to_string()
    }
}