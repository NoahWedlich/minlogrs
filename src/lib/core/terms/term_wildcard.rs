
use std::{rc::Rc, collections::HashSet};

use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};

use crate::core::substitution::{MatchContext, MatchOutput};

use crate::core::types::minlog_type::MinlogType;

use crate::core::terms::minlog_term::{TermBody, MinlogTerm, Totality};
use crate::core::terms::term_variable::TermVariable;

use crate::core::terms::term_substitution::TermSubstEntry;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct TermWildcard {
    minlog_type: Rc<MinlogType>,
}

impl TermWildcard {
    pub fn create(minlog_type: Rc<MinlogType>) -> Rc<MinlogTerm> {
        Rc::new(MinlogTerm::Wildcard(TermWildcard { minlog_type }))
    }
}

impl TermBody for TermWildcard {
    fn minlog_type(&self) -> Rc<MinlogType> {
        self.minlog_type.clone()
    }
    
    fn normalize(&self, _eta: bool, _pi: bool) -> Rc<MinlogTerm> {
        Rc::new(MinlogTerm::Wildcard(self.clone()))
    }
    
    fn length(&self) -> usize {
        1
    }
    
    fn constructor_pattern(&self) -> bool {
        true
    }

    fn get_type_variables(&self, _visited: &mut HashSet<MinlogTerm>) -> HashSet<Rc<MinlogType>> {
        self.minlog_type.get_type_variables(&mut HashSet::new())
    }
    
    fn get_algebra_types(&self, _visited: &mut HashSet<MinlogTerm>) -> HashSet<Rc<MinlogType>> {
        self.minlog_type.get_algebra_types(&mut HashSet::new())
    }
    
    fn alpha_equivalent(&self, other: &Rc<MinlogTerm>,
        _forward: &mut Vec<(TermVariable, TermVariable)>,
        _backward: &mut Vec<(TermVariable, TermVariable)>) -> bool {
        
        other.is_wildcard() && self.minlog_type == other.minlog_type()
    }
    
    fn totality(&self, _bound: &mut HashSet<TermVariable>) -> Totality {
        Totality::Partial
    }

    fn substitute(&self, from: &TermSubstEntry, to: &TermSubstEntry) -> Rc<MinlogTerm> {
        match from {
            TermSubstEntry::Type(from_t) => {
                Rc::new(MinlogTerm::Wildcard(TermWildcard {
                    minlog_type: self.minlog_type.substitute(from_t, &to.to_type().unwrap()),
                }))
            },
            _ => {
                Rc::new(MinlogTerm::Wildcard(self.clone()))
            }
        }
    }
    
    fn first_conflict_with(&self, other: &Rc<MinlogTerm>) -> Option<(TermSubstEntry, TermSubstEntry)> {
        if let Some(conflict) = self.minlog_type.first_conflict_with(&other.minlog_type()) {
            return Some((conflict.0.into(), conflict.1.into()));
        }
        
        None
    }
    
    fn match_with(&self, ctx: &mut impl MatchContext<TermSubstEntry>) -> MatchOutput<TermSubstEntry> {
        let pattern = ctx.next_pattern().unwrap();
        let instance = ctx.next_instance().unwrap();
        
        match (pattern, instance) {
            (TermSubstEntry::Term(p), TermSubstEntry::Term(i)) => {
                if p.minlog_type() != i.minlog_type() {
                    ctx.extend(&TermSubstEntry::Type(p.minlog_type()), &TermSubstEntry::Type(i.minlog_type()));
                }

                MatchOutput::Matched
            },
            _ => MatchOutput::FailedMatch,
        }
    }
}

impl PrettyPrintable for TermWildcard {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        if detail {
            PPElement::group(vec![
                PPElement::text("_:".to_string()),
                PPElement::break_elem(1, 0, false),
                self.minlog_type.to_pp_element(false)
            ], BreakType::Flexible, 0)
        } else {
            PPElement::text("_".to_string())
        }
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}