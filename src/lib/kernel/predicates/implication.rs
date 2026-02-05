
use crate::includes::{
    essential::*,
    utils::*,
    kernel::{
        types::*,
        terms::*,
        predicates::*,
    }
};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Implication {
    premise: Arc<MinlogPredicate>,
    conclusion: Arc<MinlogPredicate>,
}

impl Implication {
    pub fn create(premise: Arc<MinlogPredicate>, conclusion: Arc<MinlogPredicate>) -> Arc<MinlogPredicate> {
        if premise.arity() != conclusion.arity() {
            panic!("Premise and conclusion of an Implication must have the same arity");
        }
        
        Arc::new(MinlogPredicate::Implication(Implication { premise, conclusion }))
    }
    
    pub fn create_nested(premises: Vec<Arc<MinlogPredicate>>, conclusion: Arc<MinlogPredicate>) -> Arc<MinlogPredicate> {
        let mut result = conclusion;
        
        for premise in premises.iter().rev() {
            result = Implication::create(premise.clone(), result);
        }
        
        result
    }
    
    pub fn premise(&self) -> &Arc<MinlogPredicate> {
        &self.premise
    }
    
    pub fn all_premises(&self) -> Vec<Arc<MinlogPredicate>> {
        let mut current = self;
        let mut premises = vec![current.premise.clone()];
        
        while let Some(next_implication) = current.conclusion.to_implication() {
            premises.push(next_implication.premise.clone());
            current = next_implication;
        }
        
        premises
    }
    
    pub fn premise_at(&self, index: usize) -> Option<&Arc<MinlogPredicate>> {
        if index == 0 {
            Some(&self.premise)
        } else if self.conclusion.is_implication() {
            self.conclusion.to_implication().unwrap().premise_at(index - 1)
        } else {
            None
        }
    }
    
    pub fn conclusion(&self) -> &Arc<MinlogPredicate> {
        &self.conclusion
    }
    
    pub fn final_conclusion(&self) -> &Arc<MinlogPredicate> {
        if let Some(next_implication) = self.conclusion.to_implication() {
            next_implication.final_conclusion()
        } else {
            &self.conclusion
        }
    }
}

impl PredicateBody for Implication {
    fn arity(&self) -> Arc<MinlogType> {
        self.conclusion.arity()
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> Arc<MinlogPredicate> {
        let normalized_premise = self.premise.normalize(eta, pi);
        let normalized_conclusion = self.conclusion.normalize(eta, pi);
        
        Implication::create(normalized_premise, normalized_conclusion)
    }
    
    fn depth(&self) -> usize {
        1 + max(self.premise.depth(), self.conclusion.depth())
    }
    
    fn extracted_type_pattern(&self) -> Arc<MinlogType> {
        let premise_type = self.premise.extracted_type_pattern();
        let conclusion_type = self.conclusion.extracted_type_pattern();
        
        ArrowType::create(premise_type, conclusion_type)
            .remove_nulls().unwrap_or(TypeConstant::create_null())
    }
    
    fn extracted_type(&self) -> Arc<MinlogType> {
        let premise_type = self.premise.extracted_type();
        let conclusion_type = self.conclusion.extracted_type();
        
        ArrowType::create(premise_type, conclusion_type)
            .remove_nulls().unwrap_or(TypeConstant::create_null())
    }
    
    fn et_pattern_to_et(&self) -> TermSubstitution {
        let mut subst = self.conclusion.et_pattern_to_et();
        subst.compose(&self.premise.et_pattern_to_et());
        
        subst
    }
    
    fn get_type_variables(&self, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Arc<MinlogType>> {
        self.conclusion.get_type_variables(visited).union(
            &self.premise.get_type_variables(visited)
        ).cloned().collect()
    }
    
    fn get_algebra_types(&self, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Arc<MinlogType>> {
        self.conclusion.get_algebra_types(visited).union(
            &self.premise.get_algebra_types(visited)
        ).cloned().collect()
    }
    
    fn get_free_variables(&self, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<MinlogTerm> {
        self.conclusion.get_free_variables(visited).union(
            &self.premise.get_free_variables(visited)
        ).cloned().collect()
    }
    
    fn get_bound_variables(&self, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<MinlogTerm> {
        self.conclusion.get_bound_variables(visited).union(
            &self.premise.get_bound_variables(visited)
        ).cloned().collect()
    }

    fn get_polarized_pred_vars(&self, current: Polarity, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Polarized<Arc<MinlogPredicate>>> {
        self.conclusion.get_polarized_pred_vars(current, visited).union(
            &self.premise.get_polarized_pred_vars(current.invert(), visited)
        ).cloned().collect()
    }

    fn get_polarized_comp_terms(&self, current: Polarity, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Polarized<Arc<MinlogPredicate>>> {
        self.conclusion.get_polarized_comp_terms(current, visited).union(
            &self.premise.get_polarized_comp_terms(current.invert(), visited)
        ).cloned().collect()
    }

    fn get_polarized_inductive_preds(&self, current: Polarity, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Polarized<Arc<MinlogPredicate>>> {
        self.conclusion.get_polarized_inductive_preds(current, visited).union(
            &self.premise.get_polarized_inductive_preds(current.invert(), visited)
        ).cloned().collect()
    }

    fn get_polarized_prime_formulas(&self, current: Polarity, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Polarized<Arc<MinlogPredicate>>> {
        self.conclusion.get_polarized_prime_formulas(current, visited).union(
            &self.premise.get_polarized_prime_formulas(current.invert(), visited)
        ).cloned().collect()
    }
    
    fn substitute(&self, from: &PredSubstEntry, to: &PredSubstEntry) -> Arc<MinlogPredicate> {
        if let Some(pred) = from.to_predicate() && pred.is_implication() && self == pred.to_implication().unwrap() {
            to.to_predicate().unwrap()
        } else {
            let new_premise = self.premise.substitute(from, to);
            let new_conclusion = self.conclusion.substitute(from, to);
            
            Implication::create(new_premise, new_conclusion)
        }
    }
    
    fn first_conflict_with(&self, other: &Arc<MinlogPredicate>) -> Option<(PredSubstEntry, PredSubstEntry)> {
        if let MinlogPredicate::Implication(other_implication) = other.as_ref() {
            if let Some(conflict) = self.premise.first_conflict_with(&other_implication.premise) {
                return Some(conflict);
            }
            
            self.conclusion.first_conflict_with(&other_implication.conclusion)
        } else {
            Some((Arc::new(MinlogPredicate::Implication(self.clone())).into(), other.clone().into()))
        }
    }
    
    fn match_with(&self, instance: &Arc<MinlogPredicate>) -> MatchOutput<PredSubstEntry> {
        if !instance.is_implication() {
            return MatchOutput::FailedMatch;
        }
        
        let imp_instance = instance.to_implication().unwrap();
        
        let conditions = IndexMap::from([
            (self.premise.clone().into(), imp_instance.premise.clone().into()),
            (self.conclusion.clone().into(), imp_instance.conclusion.clone().into()),
        ]);
        
        MatchOutput::Matched(conditions)
    }
}

impl PrettyPrintable for Implication {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        PPElement::list(
            vec![
                self.premise.to_pp_element(detail),
                self.conclusion.to_pp_element(detail),
            ],
            PPElement::break_elem(1, 4, false),
            PPElement::text("=>".to_string()),
            PPElement::break_elem(1, 4, false),
            BreakType::Flexible
        )
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        true
    }
    
    fn open_paren(&self) -> String {
        "(".to_string()
    }
    
    fn close_paren(&self) -> String {
        ")".to_string()
    }
}