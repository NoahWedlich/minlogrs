
use indexmap::{IndexMap, IndexSet};
use std::rc::Rc;

use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};

use crate::core::substitution::MatchOutput;
use crate::core::polarity::{Polarity, Polarized};

use crate::core::types::minlog_type::MinlogType;
use crate::core::types::type_constant::TypeConstant;
use crate::core::types::type_variable::TypeVariable;

use crate::core::terms::term_substitution::TermSubstitution;

use crate::core::predicates::minlog_predicate::{MinlogPredicate, PredicateBody};

use crate::core::predicates::predicate_substitution::PredSubstEntry;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct PredicateVariable {
    name: String,
    arity: Rc<MinlogType>,
    index: usize,
}

impl PredicateVariable {
    pub fn create(name: String, arity: Rc<MinlogType>) -> Rc<MinlogPredicate> {
        Rc::new(MinlogPredicate::Variable(PredicateVariable { name, arity, index: 0 }))
    }
    
    pub fn unshadow(pred: &Rc<MinlogPredicate>) -> Rc<MinlogPredicate> {
        if let Some(pv) = pred.to_variable() {
            Rc::new(MinlogPredicate::Variable(PredicateVariable {
                name: pv.name.clone(),
                arity: pv.arity.clone(),
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
}

impl PredicateBody for PredicateVariable {
    fn arity(&self) -> Rc<MinlogType> {
        self.arity.clone()
    }
    
    fn normalize(&self, _eta: bool, _pi: bool) -> Rc<MinlogPredicate> {
        Rc::new(MinlogPredicate::Variable(self.clone()))
    }
    
    fn extracted_type_pattern(&self) -> Rc<MinlogType> {
        let name = if self.index > 0 {
            format!("{}_{}^et", self.name, self.index)
        } else {
            format!("{}^et", self.name)
        };
        
        TypeVariable::create(name)
    }
    
    fn extracted_type(&self) -> Rc<MinlogType> {
        TypeConstant::create_null()
    }
    
    fn et_pattern_to_et(&self) -> TermSubstitution {
        let pattern = self.extracted_type_pattern();
        let et = self.extracted_type();
        
        TermSubstitution::from_pairs(vec![(pattern.into(), et.into())])
    }
    
    fn get_type_variables(&self, _visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Rc<MinlogType>> {
        self.arity().get_type_variables(&mut IndexSet::new())
    }
    
    fn get_algebra_types(&self, _visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Rc<MinlogType>> {
        self.arity.get_algebra_types(&mut IndexSet::new())
    }
    
    fn get_polarized_pred_vars(&self, current: Polarity, _visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Polarized<Rc<MinlogPredicate>>> {
        IndexSet::from([Polarized {
            polarity: current,
            value: Rc::new(MinlogPredicate::Variable(self.clone())),
        }])
    }
    
    fn substitute(&self, from: &PredSubstEntry, to: &PredSubstEntry) -> Rc<MinlogPredicate> {
        match from {
            PredSubstEntry::Type(from_t) => {
                let new_arity = self.arity.substitute(from_t, &to.to_type().unwrap());
                Rc::new(MinlogPredicate::Variable(PredicateVariable {
                    name: self.name.clone(),
                    arity: new_arity,
                    index: self.index,
                }))
            },
            PredSubstEntry::Predicate(from_p) => {
                if from_p.is_variable() && self == from_p.to_variable().unwrap() {
                    to.to_predicate().unwrap()
                } else {
                    Rc::new(MinlogPredicate::Variable(self.clone()))
                }
            },
            _ => {
                Rc::new(MinlogPredicate::Variable(self.clone()))
            }
        }
    }
    
    fn first_conflict_with(&self, other: &Rc<MinlogPredicate>) -> Option<(PredSubstEntry, PredSubstEntry)> {
        if let Some(conflict) = self.arity.first_conflict_with(&other.arity()) {
            return Some((conflict.0.into(), conflict.1.into()));
        }
        
        if other.is_variable() && self == other.to_variable().unwrap() {
            None
        } else {
            Some((Rc::new(MinlogPredicate::Variable(self.clone())).into(), other.clone().into()))
        }
    }
    
    fn match_with(&self, instance: &Rc<MinlogPredicate>) -> MatchOutput<PredSubstEntry> {
        if self.arity() != instance.arity() {
            MatchOutput::Matched(IndexMap::from([
                (Rc::new(MinlogPredicate::Variable(self.clone())).into(), instance.clone().into()),
                (self.arity().into(), instance.arity().into()),
            ]))
        } else {
            MatchOutput::Substitution(
                Rc::new(MinlogPredicate::Variable(self.clone())).into(),
                instance.clone().into(),
            )
        }
    }
}

impl PrettyPrintable for PredicateVariable {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        if detail && !self.arity.is_unit() {
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
                    self.arity.to_enclosed_pp_element(detail),
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