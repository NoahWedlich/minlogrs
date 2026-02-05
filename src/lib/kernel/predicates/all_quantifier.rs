
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
pub struct AllQuantifier {
    var: MinlogTerm,
    body: Arc<MinlogPredicate>,
}

impl AllQuantifier {
    pub fn create(var: MinlogTerm, body: Arc<MinlogPredicate>) -> Arc<MinlogPredicate> {
        if !var.is_variable() {
            panic!("AllQuantifier can only quantify over variable terms");
        }
        
        if body.contains_bound_variable(&var) {
            panic!("AllQuantifier variable {} already bound in body {}",
                var.debug_string(),
                body.debug_string()
            );
        }
        
        Arc::new(MinlogPredicate::AllQuantifier(AllQuantifier { var, body }))
    }
    
    pub fn create_nested(vars: Vec<MinlogTerm>, body: Arc<MinlogPredicate>) -> Arc<MinlogPredicate> {
        let mut current_body = body;
        
        for var in vars.into_iter().rev() {
            current_body = AllQuantifier::create(var, current_body);
        }
        
        current_body
    }
    
    pub fn closure(minlog_formula: &Arc<MinlogPredicate>) -> Arc<MinlogPredicate> {
        let vars = minlog_formula.get_free_variables(&mut IndexSet::new())
            .into_iter().collect();
        
        AllQuantifier::create_nested(vars, minlog_formula.clone())
    }
    
    pub fn var(&self) -> &MinlogTerm {
        &self.var
    }
    
    pub fn all_vars(&self) -> Vec<MinlogTerm> {
        let mut current = self;
        let mut vars = vec![current.var.clone()];
        
        while let MinlogPredicate::AllQuantifier(next_aq) = current.body.as_ref() {
            vars.push(next_aq.var.clone());
            current = next_aq;
        }
        
        vars
    }
    
    pub fn var_at(&self, index: usize) -> Option<&MinlogTerm> {
        if index == 0 {
            Some(&self.var)
        } else if self.body.is_all_quantifier() {
            self.body.to_all_quantifier().unwrap().var_at(index - 1)
        } else {
            None
        }
    }
    
    pub fn body(&self) -> &Arc<MinlogPredicate> {
        &self.body
    }
    
    pub fn final_body(&self) -> &Arc<MinlogPredicate> {
        if let MinlogPredicate::AllQuantifier(next_aq) = self.body.as_ref() {
            next_aq.final_body()
        } else {
            &self.body
        }
    }
}

impl PredicateBody for AllQuantifier {
    fn arity(&self) -> Arc<MinlogType> {
        self.body.arity()
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> Arc<MinlogPredicate> {
        let normalized_body = self.body.normalize(eta, pi);
        AllQuantifier::create(self.var.clone(), normalized_body)
    }
    
    fn depth(&self) -> usize {
        1 + self.body.depth()
    }
    
    fn extracted_type_pattern(&self) -> Arc<MinlogType> {
        self.body.extracted_type_pattern()
    }
    
    fn extracted_type(&self) -> Arc<MinlogType> {
        self.body.extracted_type()
    }
    
    fn et_pattern_to_et(&self) -> TermSubstitution {
        self.body.et_pattern_to_et()
    }
    
    fn get_type_variables(&self, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Arc<MinlogType>> {
        self.body.get_type_variables(visited)
            .union(&self.var.get_type_variables(&mut IndexSet::new()))
            .cloned().collect()
    }
    
    fn get_algebra_types(&self, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Arc<MinlogType>> {
        self.body.get_algebra_types(visited)
            .union(&self.var.get_algebra_types(&mut IndexSet::new()))
            .cloned().collect()
    }
    
    fn get_free_variables(&self, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<MinlogTerm> {
        self.body.get_free_variables(visited).into_iter()
            .filter(|v| v != &self.var)
            .collect()
    }
    
    fn get_bound_variables(&self, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<MinlogTerm> {
        self.body.get_bound_variables(visited).into_iter()
            .chain(std::iter::once(self.var.clone()))
            .collect()
    }
    
    fn get_polarized_pred_vars(&self, current: Polarity, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Polarized<Arc<MinlogPredicate>>> {
        self.body.get_polarized_pred_vars(current, visited)
    }
    
    fn get_polarized_comp_terms(&self, current: Polarity, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Polarized<Arc<MinlogPredicate>>> {
        self.body.get_polarized_comp_terms(current, visited)
    }
    
    fn get_polarized_inductive_preds(&self, current: Polarity, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Polarized<Arc<MinlogPredicate>>> {
        self.body.get_polarized_inductive_preds(current, visited)
    }
    
    fn get_polarized_prime_formulas(&self, current: Polarity, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Polarized<Arc<MinlogPredicate>>> {
        self.body.get_polarized_prime_formulas(current, visited)
    }
    
    fn substitute(&self, from: &PredSubstEntry, to: &PredSubstEntry) -> Arc<MinlogPredicate> {
        if let Some(tm) = from.to_term() && tm == self.var {
            Arc::new(MinlogPredicate::AllQuantifier(self.clone()))
        } else if let Some(pred) = from.to_predicate() && pred.is_all_quantifier() && self == pred.to_all_quantifier().unwrap() {
            to.to_predicate().unwrap()
        } else {
            let new_var = if let Some(tse) = from.to_term_subst_entry() {
                self.var.substitute(&tse, &to.to_term_subst_entry().unwrap())
            } else {
                self.var.clone()
            };
            
            let new_body = self.body.substitute(from, to);
            
            AllQuantifier::create(new_var, new_body)
        }
    }
    
    fn first_conflict_with(&self, other: &Arc<MinlogPredicate>) -> Option<(PredSubstEntry, PredSubstEntry)> {
        if let MinlogPredicate::AllQuantifier(other_aq) = other.as_ref() {
            if self.var.minlog_type() != other_aq.var.minlog_type() {
                return Some((self.var.minlog_type().into(), other_aq.var.minlog_type().into()));
            }
            
            let other_body = if self.var == other_aq.var {
                other_aq.body()
            } else {
                &other_aq.body.substitute(
                    &other_aq.var.clone().into(),
                    &self.var.clone().into()
                )
            };
            
            self.body.first_conflict_with(other_body)
        } else {
            Some((Arc::new(MinlogPredicate::AllQuantifier(self.clone())).into(), other.clone().into()))
        }
    }
    
    fn match_with(&self, instance: &Arc<MinlogPredicate>) -> MatchOutput<PredSubstEntry> {
        if !instance.is_all_quantifier() {
            return MatchOutput::FailedMatch;
        }
        
        let aq_instance = instance.to_all_quantifier().unwrap();
        
        let conditions = IndexMap::from([
            (self.var.clone().into(), aq_instance.var.clone().into()),
            (self.body.clone().into(), aq_instance.body.clone().into()),
        ]);
        
        MatchOutput::Matched(conditions)
    }
}

impl PrettyPrintable for AllQuantifier {
    fn to_pp_element(&self, detail: bool) -> PPElement {        
        PPElement::group(vec![
            PPElement::text("all".to_string()),
            PPElement::break_elem(1, 4, false),
            self.var.to_pp_element(detail),
            PPElement::break_elem(0, 4, false),
            PPElement::text(":".to_string()),
            PPElement::break_elem(1, 4, false),
            self.body.to_pp_element(detail),
        ], BreakType::Flexible, 0)
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