
use indexmap::{IndexMap, IndexSet};
use std::rc::Rc;

use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};

use crate::core::substitution::MatchOutput;

use crate::core::types::minlog_type::MinlogType;

use crate::core::terms::minlog_term::{TermBody, MinlogTerm};

use crate::core::terms::term_substitution::TermSubstEntry;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct TermVariable {
    name: String,
    minlog_type: Rc<MinlogType>,
    index: usize,
}

impl TermVariable {
    pub fn create(name: String, minlog_type: Rc<MinlogType>) -> Rc<MinlogTerm> {
        Rc::new(MinlogTerm::Variable(TermVariable { name, minlog_type, index: 0 }))
    }
    
    pub fn unshadow(var: &Rc<MinlogTerm>) -> Rc<MinlogTerm> {
        if let Some(tv) = var.to_variable() {
            Rc::new(MinlogTerm::Variable(TermVariable {
                name: tv.name.clone(),
                minlog_type: Rc::clone(&tv.minlog_type),
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
}

impl TermBody for TermVariable {
    fn minlog_type(&self) -> Rc<MinlogType> {
        self.minlog_type.clone()
    }
    
    fn normalize(&self, _eta: bool, _pi: bool) -> Rc<MinlogTerm> {
        Rc::new(MinlogTerm::Variable(self.clone()))
    }
    
    fn remove_nulls(&self) -> Option<Rc<MinlogTerm>> {
        self.minlog_type.remove_nulls().map(|new_type| {
            Rc::new(MinlogTerm::Variable(TermVariable {
                name: self.name.clone(),
                minlog_type: new_type,
                index: self.index,
            }))
        })
    }
    
    fn length(&self) -> usize {
        1
    }
    
    fn constructor_pattern(&self) -> bool {
        true
    }

    fn get_type_variables(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogType>> {
        self.minlog_type.get_type_variables(&mut IndexSet::new())
    }
    
    fn get_algebra_types(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogType>> {
        self.minlog_type.get_algebra_types(&mut IndexSet::new())
    }
    
    fn get_free_variables(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogTerm>> {
        IndexSet::from([Rc::new(MinlogTerm::Variable(self.clone()))])
    }
    
    fn alpha_equivalent(&self, other: &Rc<MinlogTerm>,
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

    fn substitute(&self, from: &TermSubstEntry, to: &TermSubstEntry) -> Rc<MinlogTerm> {
        match from {
            TermSubstEntry::Type(from_t) => {
                Rc::new(MinlogTerm::Variable(TermVariable {
                    name: self.name.clone(),
                    minlog_type: self.minlog_type.substitute(from_t, &to.to_type().unwrap()),
                    index: self.index,
                }))
            },
            TermSubstEntry::Term(from_tm) => {
                if from_tm.is_variable() && self == from_tm.to_variable().unwrap() {
                    to.to_term().unwrap()
                } else {
                    Rc::new(MinlogTerm::Variable(self.clone()))
                }
            }
        }
    }
    
    fn first_conflict_with(&self, other: &Rc<MinlogTerm>) -> Option<(TermSubstEntry, TermSubstEntry)> {
        if let Some(conflict) = self.minlog_type.first_conflict_with(&other.minlog_type()) {
            return Some((conflict.0.into(), conflict.1.into()));
        }
        
        if other.is_variable() && self == other.to_variable().unwrap() {
            None
        } else {
            Some((Rc::new(MinlogTerm::Variable(self.clone())).into(), Rc::clone(other).into()))
        }
    }
    
    fn match_with(&self, instance: &Rc<MinlogTerm>) -> MatchOutput<TermSubstEntry> {
        if self.minlog_type() != instance.minlog_type() {
            MatchOutput::Matched(IndexMap::from([
                (Rc::new(MinlogTerm::Variable(self.clone())).into(), instance.clone().into()),
                (self.minlog_type().into(), instance.minlog_type().into())
            ]))
        } else {
            MatchOutput::Substitution(Rc::new(MinlogTerm::Variable(self.clone())).into(), instance.clone().into())
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