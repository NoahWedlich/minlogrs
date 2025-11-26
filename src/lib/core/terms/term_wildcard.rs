
use crate::includes::{
    essential::*,
    utils::*,
    core::{
        types::*,
        terms::*,
    }
};

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
    
    fn remove_nulls(&self) -> Option<Rc<MinlogTerm>> {
        self.minlog_type.remove_nulls().map(|new_type| {
            TermWildcard::create(new_type)
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
    
    fn alpha_equivalent(&self, other: &Rc<MinlogTerm>,
        _forward: &mut Vec<(TermVariable, TermVariable)>,
        _backward: &mut Vec<(TermVariable, TermVariable)>) -> bool {
        
        other.is_wildcard() && self.minlog_type == other.minlog_type()
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
    
    fn match_with(&self, instance: &Rc<MinlogTerm>) -> MatchOutput<TermSubstEntry> {
        let conditions = if self.minlog_type() != instance.minlog_type() {
            IndexMap::from([(self.minlog_type.clone().into(), instance.minlog_type().clone().into())])
        } else {
            IndexMap::new()
        };

        MatchOutput::Matched(conditions)
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