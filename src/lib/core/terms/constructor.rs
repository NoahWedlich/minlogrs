
use indexmap::{IndexMap, IndexSet};
use std::rc::Rc;
use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};

use crate::core::substitution::MatchOutput;

use crate::core::types::minlog_type::MinlogType;

use crate::core::terms::minlog_term::{TermBody, MinlogTerm};
use crate::core::terms::term_variable::TermVariable;

use crate::core::terms::term_substitution::TermSubstEntry;

#[derive(Clone, PartialEq, Eq, Hash)]
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
    
    fn remove_nulls(&self) -> Option<Rc<MinlogTerm>> {
        let algebra_type = if let Some(algebra) = self.minlog_type.to_algebra() {
            algebra
        } else if let Some(arrow) = self.minlog_type.to_arrow() {
            arrow.value().to_algebra()?
        } else {
            return None;
        };
        
        if let Some(reduction) = algebra_type.get_reduction() {
            let new_name = reduction.constructor_mapping.get(&self.name).unwrap().clone();
            let new_type = self.minlog_type.remove_nulls().unwrap();
            Some(Constructor::create(new_name, new_type))
        } else {
            Some(Constructor::create(self.name.clone(), self.minlog_type.clone()))
        }
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
    
    fn get_type_variables(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogType>> {
        self.minlog_type.get_type_variables(&mut IndexSet::new())
    }
    
    fn get_algebra_types(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogType>> {
        self.minlog_type.get_algebra_types(&mut IndexSet::new())
    }
    
    fn get_constructors(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogTerm>> {
        IndexSet::from([Rc::new(MinlogTerm::Constructor(self.clone()))])
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
    
    fn substitute(&self, from: &TermSubstEntry, to: &TermSubstEntry) -> Rc<MinlogTerm> {
        match from {
            TermSubstEntry::Type(from_t) => {
                let new_type = self.minlog_type.substitute(from_t, &to.to_type().unwrap());
                Constructor::create(self.name.clone(), new_type)
            },
            TermSubstEntry::Term(from_tm) => {
                if from_tm.is_constructor() && self == from_tm.to_constructor().unwrap() {
                    to.to_term().unwrap()
                } else {
                    Constructor::create(self.name.clone(), self.minlog_type.clone())
                }
            }
        }
    }
    
    fn first_conflict_with(&self, other: &Rc<MinlogTerm>) -> Option<(TermSubstEntry, TermSubstEntry)> {
        if let Some(conflict) = self.minlog_type.first_conflict_with(&other.minlog_type()) {
            return Some((conflict.0.into(), conflict.1.into()));
        }
        
        if !other.is_constructor() {
            return Some((Rc::new(MinlogTerm::Constructor(self.clone())).into(), other.clone().into()));
        }
        
        let other_constr = other.to_constructor().unwrap();
        
        if self.name != other_constr.name || self.minlog_type != other_constr.minlog_type {
            return Some((Rc::new(MinlogTerm::Constructor(self.clone())).into(), other.clone().into()));
        }
        None
    }
    
    fn match_with(&self, instance: &Rc<MinlogTerm>) -> MatchOutput<TermSubstEntry> {
        if !instance.is_constructor() {
            return MatchOutput::FailedMatch;
        }
        
        let constr_instance = instance.to_constructor().unwrap();
        
        if self.name != constr_instance.name {
            return MatchOutput::FailedMatch;
        }
        
        let conditions = if self.minlog_type != constr_instance.minlog_type {
            IndexMap::from([(self.minlog_type.clone().into(), constr_instance.minlog_type.clone().into())])
        } else {
            IndexMap::new()
        };
        
        MatchOutput::Matched(conditions)
    }
}

impl PrettyPrintable for Constructor {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        if detail {
            PPElement::group(vec![
                PPElement::text(self.name.clone()),
                PPElement::text(":".to_string()),
                PPElement::break_elem(1, 4, false),
                self.minlog_type.to_pp_element(true),
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