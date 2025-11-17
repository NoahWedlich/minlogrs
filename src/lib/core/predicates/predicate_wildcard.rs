
use indexmap::IndexSet;
use std::rc::Rc;

use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};

use crate::core::substitution::{MatchContext, MatchOutput};

use crate::core::types::minlog_type::MinlogType;
use crate::core::types::type_constant::TypeConstant;

use crate::core::terms::term_substitution::TermSubstitution;

use crate::core::predicates::minlog_predicate::{MinlogPredicate, PredicateBody};

use crate::core::predicates::predicate_substitution::PredSubstEntry;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct PredicateWildcard {
    arity: Rc<MinlogType>,
}

impl PredicateWildcard {
    pub fn create(arity: Rc<MinlogType>) -> Rc<MinlogPredicate> {
        Rc::new(MinlogPredicate::Wildcard(PredicateWildcard { arity }))
    }
}

impl PredicateBody for PredicateWildcard {
    fn arity(&self) -> Rc<MinlogType> {
        self.arity.clone()
    }
    
    fn normalize(&self, _eta: bool, _pi: bool) -> Rc<MinlogPredicate> {
        Rc::new(MinlogPredicate::Wildcard(self.clone()))
    }
    
    fn extracted_type_pattern(&self) -> Rc<MinlogType> {
        TypeConstant::create_null()
    }
    
    fn extracted_type(&self) -> Rc<MinlogType> {
        TypeConstant::create_null()
    }
    
    fn et_pattern_to_et(&self) -> TermSubstitution {
        TermSubstitution::make_empty()
    }
    
    fn get_type_variables(&self, _visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Rc<MinlogType>> {
        self.arity().get_type_variables(&mut IndexSet::new())
    }
    
    fn get_algebra_types(&self, _visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Rc<MinlogType>> {
        self.arity.get_algebra_types(&mut IndexSet::new())
    }
    
    fn substitute(&self, from: &PredSubstEntry, to: &PredSubstEntry) -> Rc<MinlogPredicate> {
        match from {
            PredSubstEntry::Type(from_t) => {
                PredicateWildcard::create(self.arity.substitute(from_t, &to.to_type().unwrap()))
            },
            _ => Rc::new(MinlogPredicate::Wildcard(self.clone())),
        }
    }
    
    fn first_conflict_with(&self, other: &Rc<MinlogPredicate>) -> Option<(PredSubstEntry, PredSubstEntry)> {
        if let Some(conflict) = self.arity.first_conflict_with(&other.arity()) {
            return Some((conflict.0.into(), conflict.1.into()));
        }
        
        None
    }
    
    fn match_with(&self, ctx: &mut impl MatchContext<PredSubstEntry>) -> MatchOutput<PredSubstEntry> {
        let pattern = ctx.next_pattern().unwrap();
        let instance = ctx.next_instance().unwrap();
        
        match (pattern, instance) {
            (PredSubstEntry::Predicate(p), PredSubstEntry::Predicate(i)) => {
                if p.arity() != i.arity() {
                    ctx.extend(&p.arity().clone().into(), &i.arity().clone().into());
                }
                
                MatchOutput::Matched
            },
            _ => MatchOutput::FailedMatch,
        }
    }
}

impl PrettyPrintable for PredicateWildcard {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        if detail && !self.arity.is_unit() {
            PPElement::group(vec![
                PPElement::text("_".to_string()),
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
            PPElement::text("_".to_string())
        }
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}