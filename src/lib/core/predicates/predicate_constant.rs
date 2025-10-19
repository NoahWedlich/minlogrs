
use std::rc::Rc;

use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};

use crate::core::substitution::{MatchContext, MatchOutput};

use crate::core::types::minlog_type::MinlogType;

use crate::core::predicates::minlog_predicate::{MinlogPredicate, PredicateBody};

use crate::core::predicates::predicate_substitution::PredSubstEntry;

#[derive(Clone, PartialEq, Eq)]
pub struct PredicateConstant {
    name: String,
    arity: Vec<Rc<MinlogType>>,
}

impl PredicateConstant {
    pub fn create(name: String, arity: Vec<Rc<MinlogType>>) -> Rc<MinlogPredicate> {
        Rc::new(MinlogPredicate::Constant(PredicateConstant { name, arity }))
    }
    
    pub fn name(&self) -> &str {
        &self.name
    }
}

impl PredicateBody for PredicateConstant {
    fn arity(&self) -> Vec<Rc<MinlogType>> {
        self.arity.clone()
    }
    
    fn substitute(&self, from: &PredSubstEntry, to: &PredSubstEntry) -> Rc<MinlogPredicate> {
        match (from, to) {
            (PredSubstEntry::Type(from_t), PredSubstEntry::Type(to_t)) => {
                let new_arity = self.arity.iter()
                    .map(|t| t.substitute(from_t, to_t))
                    .collect();
                
                PredicateConstant::create(self.name.clone(), new_arity)
            },
            (PredSubstEntry::Term(_), PredSubstEntry::Term(_)) => {
                Rc::new(MinlogPredicate::Constant(self.clone()))
            },
            (PredSubstEntry::Predicate(_), PredSubstEntry::Predicate(_)) => {
                Rc::new(MinlogPredicate::Constant(self.clone()))
            },
            _ => {
                panic!("Tried to substitute between incompatible PredSubstEntry types");
            }
        }
    }
    
    fn first_conflict_with(&self, other: &Rc<MinlogPredicate>) -> Option<(PredSubstEntry, PredSubstEntry)> {
        if let Some(conflict) = self.arity.iter().find_map(|t| {
            other.arity().iter().find_map(|ot| t.first_conflict_with(ot))
        }) {
            return Some((conflict.0.into(), conflict.1.into()));
        }
        
        if other.is_constant() && self == other.to_constant().unwrap() {
            None
        } else {
            Some((Rc::new(MinlogPredicate::Constant(self.clone())).into(), other.clone().into()))
        }
    }
    
    fn match_with(&self, ctx: &mut impl MatchContext<PredSubstEntry>) -> MatchOutput<PredSubstEntry> {
        let pattern = ctx.next_pattern().unwrap();
        let instance = ctx.next_instance().unwrap();
        
        match (pattern, instance) {
            (PredSubstEntry::Predicate(p), PredSubstEntry::Predicate(i)) => {
                if p.arity() != i.arity() {
                    return MatchOutput::FailedMatch;
                }
                
                for (pt, it) in p.arity().iter().zip(i.arity().iter()) {
                    if pt != it {
                        ctx.extend(&PredSubstEntry::Type(pt.clone()), &PredSubstEntry::Type(it.clone()));
                    }
                }
                
                MatchOutput::Matched
            },
            _ => MatchOutput::FailedMatch,
        }
    }
}

impl PrettyPrintable for PredicateConstant {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        if detail {
            let mut types = vec![];
            
            for (i, t) in self.arity.iter().enumerate() {
                types.push(
                    if i < self.arity.len() - 1 {
                        PPElement::group(vec![
                            t.to_pp_element(true),
                            PPElement::break_elem(0, 0, false),
                            PPElement::text(",".to_string()),
                        ], BreakType::Flexible, 0)
                    } else {
                        t.to_pp_element(true)
                    }
                );
                
                if i < self.arity.len() - 1 {
                    types.push(PPElement::break_elem(1, 0, false));
                }
            }
            
            PPElement::group(vec![
                PPElement::text(self.name.clone()),
                PPElement::break_elem(1, 4, false),
                PPElement::group(vec![
                    PPElement::text("[".to_string()),
                    PPElement::break_elem(1, 4, false),
                    PPElement::group(types, BreakType::Flexible, 0),
                    PPElement::break_elem(1, 0, false),
                    PPElement::text("]".to_string()),
                ], BreakType::Consistent, 0),
            ], BreakType::Flexible, 0)
        } else {
            PPElement::text(self.name.clone())
        }
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}