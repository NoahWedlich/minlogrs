
use crate::includes::{
    essential::*,
    utils::*,
    core::{
        types::*,
        terms::*,
        predicates::*,
    }
};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct PredicateWildcard {
    arity: Arc<MinlogType>,
}

impl PredicateWildcard {
    pub fn create(arity: Arc<MinlogType>) -> Arc<MinlogPredicate> {
        Arc::new(MinlogPredicate::Wildcard(PredicateWildcard { arity }))
    }
}

impl PredicateBody for PredicateWildcard {
    fn arity(&self) -> Arc<MinlogType> {
        self.arity.clone()
    }
    
    fn normalize(&self, _eta: bool, _pi: bool) -> Arc<MinlogPredicate> {
        Arc::new(MinlogPredicate::Wildcard(self.clone()))
    }
    
    fn extracted_type_pattern(&self) -> Arc<MinlogType> {
        TypeConstant::create_null()
    }
    
    fn extracted_type(&self) -> Arc<MinlogType> {
        TypeConstant::create_null()
    }
    
    fn et_pattern_to_et(&self) -> TermSubstitution {
        TermSubstitution::make_empty()
    }
    
    fn get_type_variables(&self, _visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Arc<MinlogType>> {
        self.arity().get_type_variables(&mut IndexSet::new())
    }
    
    fn get_algebra_types(&self, _visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Arc<MinlogType>> {
        self.arity.get_algebra_types(&mut IndexSet::new())
    }
    
    fn substitute(&self, from: &PredSubstEntry, to: &PredSubstEntry) -> Arc<MinlogPredicate> {
        match from {
            PredSubstEntry::Type(from_t) => {
                PredicateWildcard::create(self.arity.substitute(from_t, &to.to_type().unwrap()))
            },
            _ => Arc::new(MinlogPredicate::Wildcard(self.clone())),
        }
    }
    
    fn first_conflict_with(&self, other: &Arc<MinlogPredicate>) -> Option<(PredSubstEntry, PredSubstEntry)> {
        if let Some(conflict) = self.arity.first_conflict_with(&other.arity()) {
            return Some((conflict.0.into(), conflict.1.into()));
        }
        
        None
    }
    
    fn match_with(&self, instance: &Arc<MinlogPredicate>) -> MatchOutput<PredSubstEntry> {
        let conditions = if self.arity() != instance.arity() {
            IndexMap::from([(self.arity().into(), instance.arity().into())])
        } else {
            IndexMap::new()
        };
        
        MatchOutput::Matched(conditions)
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