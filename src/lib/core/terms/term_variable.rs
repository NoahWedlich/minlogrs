
use std::rc::Rc;
use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};
use crate::core::types::minlog_type::MinlogType;
use crate::core::terms::minlog_term::{TermBody, MinlogTerm, Totality};

#[derive(Clone)]
pub struct TermVariable {
    name: String,
    minlog_type: Rc<MinlogType>,
    totality: Totality,
    index: usize,
}

impl TermVariable {
    pub fn create(name: String, minlog_type: Rc<MinlogType>, totality: Totality) -> Rc<MinlogTerm> {
        Rc::new(MinlogTerm::Variable(TermVariable { name, minlog_type, totality, index: 0 }))
    }
    
    pub fn unshadow(var: &Rc<MinlogTerm>) -> Rc<MinlogTerm> {
        if let Some(tv) = var.to_variable() {
            Rc::new(MinlogTerm::Variable(TermVariable {
                name: tv.name.clone(),
                minlog_type: Rc::clone(&tv.minlog_type),
                totality: tv.totality.clone(),
                index: tv.index + 1,
            }))
        } else {
            panic!("Called TermVariable::unshadow on a non-variable term");
        }
    }
    
    pub fn name(&self) -> &str {
        &self.name
    }
    
    pub fn index(&self) -> usize {
        self.index
    }
    
    pub fn set_totality(&mut self, totality: Totality) {
        self.totality = totality;
    }
}

impl TermBody for TermVariable {
    fn minlog_type(&self) -> Rc<MinlogType> {
        self.minlog_type.clone()
    }
    
    fn normalize(self: &Self, _eta: bool, _pi: bool) -> Rc<MinlogTerm> {
        Rc::new(MinlogTerm::Variable(TermVariable {
            name: self.name.clone(),
            minlog_type: Rc::clone(&self.minlog_type),
            totality: self.totality.clone(),
            index: self.index,
        }))
    }
    
    fn length(self: &Self) -> usize {
        1
    }
    
    fn constructor_pattern(self: &Self) -> bool {
        true
    }
    
    fn alpha_equivalent(self: &Self, other: &Rc<MinlogTerm>,
        forward: &mut Vec<(TermVariable, TermVariable)>,
        backward: &mut Vec<(TermVariable, TermVariable)>) -> bool {
        
        if !other.is_variable() {
            return false;
        }
        
        let other = other.to_variable().unwrap();
        
        let forward_pair = forward.iter().find(|(v1, _)| v1 == self);
        let backward_pair = backward.iter().find(|(v2, _)| v2 == other);
        
        match (forward_pair, backward_pair) {
            (Some((f1, f2)), Some((b2, b1))) => f1 == b1 && f2 == b2,
            (None, None) => self == other,
            _ => false,
        }
    }
    
    fn totality(&self, bound: &mut Vec<TermVariable>) -> Totality {
        if bound.contains(self) {
            Totality::Total
        } else {
            self.totality.clone()
        }
    }
}

impl PrettyPrintable for TermVariable {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        if detail {
            PPElement::group(vec![
                PPElement::text(self.name.clone()),
                if self.index > 0 {
                    PPElement::text(format!("_{}", self.index))
                } else {
                    PPElement::break_elem(0, 0, false)
                },
                PPElement::text(":".to_string()),
                PPElement::break_elem(1, 0, false),
                self.minlog_type.to_pp_element(false)
            ], BreakType::Flexible, 0)
        } else {
            PPElement::text(if self.index > 0 {
                format!("{}_{}", self.name, self.index)
            } else {
                self.name.clone()
            })
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

impl PartialEq for TermVariable {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.index == other.index && self.minlog_type == other.minlog_type
    }
}

impl Eq for TermVariable {}