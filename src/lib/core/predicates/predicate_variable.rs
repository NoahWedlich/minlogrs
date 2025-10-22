
use std::{rc::Rc, collections::HashSet};

use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};

use crate::core::substitution::{MatchContext, MatchOutput};
use crate::core::polarity::{Polarity, Polarized};

use crate::core::types::minlog_type::MinlogType;
use crate::core::types::type_variable::TypeVariable;

use crate::core::predicates::minlog_predicate::{MinlogPredicate, PredicateBody, PredicateDegree};

use crate::core::predicates::predicate_substitution::PredSubstEntry;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct PredicateVariable {
    name: String,
    arity: Vec<Rc<MinlogType>>,
    degree: PredicateDegree,
    index: usize,
}

impl PredicateVariable {
    pub fn create(name: String, arity: Vec<Rc<MinlogType>>, degree: PredicateDegree) -> Rc<MinlogPredicate> {
        Rc::new(MinlogPredicate::Variable(PredicateVariable { name, arity, degree, index: 0 }))
    }
    
    pub fn unshadow(pred: &Rc<MinlogPredicate>) -> Rc<MinlogPredicate> {
        if let Some(pv) = pred.to_variable() {
            Rc::new(MinlogPredicate::Variable(PredicateVariable {
                name: pv.name.clone(),
                arity: pv.arity.clone(),
                degree: pv.degree.clone(),
                index: pv.index + 1,
            }))
        } else {
            panic!("Called PredicateVariable::unshadow on a non-variable predicate");
        }
    }
    
    pub fn name(&self) -> &str {
        &self.name
    }
    
    pub fn index(&self) -> usize {
        self.index
    }
    
    pub fn set_degree(&mut self, degree: PredicateDegree) {
        self.degree = degree;
    }
}

impl PredicateBody for PredicateVariable {
    fn arity(&self) -> Vec<Rc<MinlogType>> {
        self.arity.clone()
    }
    
    fn degree(&self) -> PredicateDegree {
        self.degree.clone()
    }
    
    fn extracted_type(&self) -> Rc<MinlogType> {
        let name = if self.index > 0 {
            format!("{}_{}^et", self.name, self.index)
        } else {
            format!("{}^et", self.name)
        };
        
        TypeVariable::create(name)
    }
    
    fn get_type_variables(&self) -> HashSet<Rc<MinlogType>> {
        self.arity.iter().flat_map(|t| t.get_type_variables()).collect()
    }
    
    fn get_algebra_types(&self) -> HashSet<Rc<MinlogType>> {
        self.arity.iter().flat_map(|t| t.get_algebra_types()).collect()
    }
    
    fn get_polarized_pred_vars(&self, current: Polarity) -> HashSet<Polarized<Rc<MinlogPredicate>>> {
        HashSet::from([Polarized {
            polarity: current,
            value: Rc::new(MinlogPredicate::Variable(self.clone())),
        }])
    }
    
    fn substitute(&self, from: &PredSubstEntry, to: &PredSubstEntry) -> Rc<MinlogPredicate> {
        match (from, to) {
            (PredSubstEntry::Type(from_t), PredSubstEntry::Type(to_t)) => {
                let new_arity = self.arity.iter()
                    .map(|t| t.substitute(from_t, to_t))
                    .collect();
                
                Rc::new(MinlogPredicate::Variable(PredicateVariable {
                    name: self.name.clone(),
                    arity: new_arity,
                    degree: self.degree.clone(),
                    index: self.index,
                }))
            },
            (PredSubstEntry::Term(_), PredSubstEntry::Term(_)) => {
                Rc::new(MinlogPredicate::Variable(self.clone()))
            },
            (PredSubstEntry::Predicate(from_p), PredSubstEntry::Predicate(to_p)) => {
                if from_p.is_variable() && self == from_p.to_variable().unwrap() {
                    to_p.clone()
                } else {
                    Rc::new(MinlogPredicate::Variable(self.clone()))
                }
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
        
        if other.is_variable() && self == other.to_variable().unwrap() {
            None
        } else {
            Some((Rc::new(MinlogPredicate::Variable(self.clone())).into(), other.clone().into()))
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
                
                match (p.degree(), i.degree()) {
                    (PredicateDegree{ positive_content: false, .. }, PredicateDegree{ positive_content: true, .. }) |
                    (PredicateDegree{ negative_content: false, .. }, PredicateDegree{ negative_content: true, .. }) => {
                        return MatchOutput::FailedMatch;
                    },
                    _ => {}
                }
                
                MatchOutput::Substitution(p.into(), i.into())
            },
            _ => MatchOutput::FailedMatch,
        }
    }
}

impl PrettyPrintable for PredicateVariable {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        if detail {
            let types = PPElement::list(
                self.arity.iter().map(|t| t.to_pp_element(true)).collect(),
                PPElement::break_elem(0, 0, false),
                PPElement::text(",".to_string()),
                PPElement::break_elem(1, 0, false),
                BreakType::Flexible,
            );
            
            PPElement::group(vec![
                PPElement::text(if self.index > 0 {
                    format!("{}_{}", self.name, self.index)
                } else {
                    self.name.clone()
                }),
                PPElement::break_elem(1, 4, false),
                PPElement::group(vec![
                    PPElement::text("[".to_string()),
                    PPElement::break_elem(1, 4, false),
                    types,
                    PPElement::break_elem(1, 0, false),
                    PPElement::text("]".to_string()),
                ], BreakType::Consistent, 0),
            ], BreakType::Flexible, 0)
        } else {
            PPElement::text(if self.index > 0 {
                format!("{}_{}", self.name, self.index)
            } else {
                self.name.clone()
            })
        }
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}