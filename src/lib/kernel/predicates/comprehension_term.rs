
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
pub struct ComprehensionTerm {
    var: MinlogTerm,
    body: Arc<MinlogPredicate>,
    arity: Arc<MinlogType>,
}

impl ComprehensionTerm {
    pub fn create(var: MinlogTerm, body: Arc<MinlogPredicate>) -> Arc<MinlogPredicate> {
        if !var.is_variable() {
            panic!("Tried to create comprehension term with non-variable bound term");
        }
        
        let unpacked_arity = body.unpacked_arity();
        let mut var_types = vec![var.minlog_type().clone()];
        var_types.extend(unpacked_arity);
        
        let arity = PairType::create_nested(var_types);
        Arc::new(MinlogPredicate::Comprehension(ComprehensionTerm { var, body, arity }))
    }
    
    pub fn create_nested(vars: Vec<MinlogTerm>, body: Arc<MinlogPredicate>) -> Arc<MinlogPredicate> {
        let mut current = body;
        
        for var in vars.into_iter().rev() {
            current = ComprehensionTerm::create(var, current);
        }
        
        current
    }
    
    pub fn closure(minlog_formula: &Arc<MinlogPredicate>) -> Arc<MinlogPredicate> {
        let vars = minlog_formula.get_free_variables(&mut IndexSet::new())
            .into_iter().collect();
        
        ComprehensionTerm::create_nested(vars, minlog_formula.clone())
    }
    
    pub fn var(&self) -> &MinlogTerm {
        &self.var
    }
    
    pub fn all_vars(&self) -> Vec<MinlogTerm> {
        let mut current = self;
        let mut vars = vec![current.var.clone()];
        
        while let MinlogPredicate::Comprehension(cterm) = current.body.as_ref() {
            vars.push(cterm.var.clone());
            current = cterm;
        }
        
        vars
    }
    
    pub fn var_at(&self, index: usize) -> Option<&MinlogTerm> {
        if index == 0 {
            Some(&self.var)
        } else if let MinlogPredicate::Comprehension(cterm) = self.body.as_ref() {
            cterm.var_at(index - 1)
        } else {
            None
        }
    }
    
    pub fn body(&self) -> &Arc<MinlogPredicate> {
        &self.body
    }
    
    pub fn final_body(&self) -> &Arc<MinlogPredicate> {
        if let MinlogPredicate::Comprehension(cterm) = self.body.as_ref() {
            cterm.final_body()
        } else {
            &self.body
        }
    }
}

impl PredicateBody for ComprehensionTerm {
    fn arity(&self) -> Arc<MinlogType> {
        self.arity.clone()
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> Arc<MinlogPredicate> {
        let new_body = self.body.normalize(eta, pi);
        
        if eta && !new_body.contains_free_variable(&self.var) {
            new_body
        } else {
            ComprehensionTerm::create(self.var.clone(), new_body)
        }
    }
    
    fn depth(&self) -> usize {
        self.body.depth() + 1
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
        self.body.get_type_variables(visited).union(
            &self.var.get_type_variables(&mut IndexSet::new())
        ).cloned().collect()
    }

    fn get_algebra_types(&self, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Arc<MinlogType>> {
        self.body.get_algebra_types(visited).union(
            &self.var.get_algebra_types(&mut IndexSet::new())
        ).cloned().collect()
    }

    fn get_free_variables(&self, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<MinlogTerm> {
        self.body.get_free_variables(visited).difference(
            &IndexSet::from([self.var.clone()])
        ).cloned().collect()
    }
    
    fn get_bound_variables(&self, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<MinlogTerm> {
        self.body.get_bound_variables(visited).union(
            &IndexSet::from([self.var.clone()])
        ).cloned().collect()
    }
    
    fn get_polarized_pred_vars(&self, current: Polarity, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Polarized<Arc<MinlogPredicate>>> {
        self.body.get_polarized_pred_vars(current, visited)
    }

    fn get_polarized_comp_terms(&self, current: Polarity, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Polarized<Arc<MinlogPredicate>>> {
        let mut result = self.body.get_polarized_comp_terms(current, visited);
        result.insert(Polarized::new(current, Arc::new(MinlogPredicate::Comprehension(self.clone()))));
        result
    }
    
    fn get_polarized_inductive_preds(&self, current: Polarity, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Polarized<Arc<MinlogPredicate>>> {
        self.body.get_polarized_inductive_preds(current, visited)
    }
    
    fn get_polarized_prime_formulas(&self, current: Polarity, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Polarized<Arc<MinlogPredicate>>> {
        self.body.get_polarized_prime_formulas(current, visited)
    }
    
    fn substitute(&self, from: &PredSubstEntry, to: &PredSubstEntry) -> Arc<MinlogPredicate> {
        match from {
            PredSubstEntry::Type(_) => {
                let new_var = self.var.substitute(&from.to_term_subst_entry().unwrap(), &to.to_term_subst_entry().unwrap());
                let new_body = self.body.substitute(from, to);
                
                ComprehensionTerm::create(new_var, new_body)
            },
            PredSubstEntry::Term(from_tm) => {
                if from_tm.is_variable() && self.var == *from_tm {
                    Arc::new(MinlogPredicate::Comprehension(self.clone()))
                } else {
                    let new_var = self.var.substitute(&from.to_term_subst_entry().unwrap(), &to.to_term_subst_entry().unwrap());
                    let new_body = self.body.substitute(from, to);
                    
                    ComprehensionTerm::create(new_var, new_body)
                }
            },
            PredSubstEntry::Predicate(from_p) => {
                if from_p.is_comprehension_term() && self == from_p.to_comprehension_term().unwrap() {
                    to.to_predicate().unwrap()
                } else {
                    let new_body = self.body.substitute(from, to);
                    ComprehensionTerm::create(self.var.clone(), new_body)
                }
            },
        }
    }
    
    fn first_conflict_with(&self, other: &Arc<MinlogPredicate>) -> Option<(PredSubstEntry, PredSubstEntry)> {
        if let Some(conflict) = self.arity.first_conflict_with(&other.arity()) {
            return Some((conflict.0.into(), conflict.1.into()));
        }
        
        if let Some(other_cterm) = other.to_comprehension_term() {
            if self.var.minlog_type() != other_cterm.var.minlog_type() {
                return Some((self.var.minlog_type().clone().into(), other_cterm.var.minlog_type().clone().into()));
            }
            
            let other_body = if self.var == other_cterm.var {
                other_cterm.body()
            } else {
                &other_cterm.body.substitute(
                    &other_cterm.var.clone().into(),
                    &self.var.clone().into()
                )
            };
            
            self.body.first_conflict_with(other_body)
        } else {
            Some((Arc::new(MinlogPredicate::Comprehension(self.clone())).into(), other.clone().into()))
        }
    }
    
    fn match_with(&self, instance: &Arc<MinlogPredicate>) -> MatchOutput<PredSubstEntry> {
        if !instance.is_comprehension_term() {
            return MatchOutput::FailedMatch;
        }
        
        let cterm_instance = instance.to_comprehension_term().unwrap();
        
        let conditions = IndexMap::from([
            (self.var.clone().into(), cterm_instance.var.clone().into()),
            (self.body.clone().into(), cterm_instance.body.clone().into()),
        ]);
        
        MatchOutput::Matched(conditions)
    }
}

impl PrettyPrintable for ComprehensionTerm {
    fn to_pp_element(&self, detail: bool) -> PPElement {        
        PPElement::group(vec![
            PPElement::text("{".to_string()),
            PPElement::break_elem(1, 4, false),
            self.var.to_pp_element(detail),
            PPElement::break_elem(1, 0, false),
            PPElement::text("|".to_string()),
            PPElement::break_elem(1, 4, false),
            self.body.to_pp_element(detail),
            PPElement::break_elem(1, 0, false),
            PPElement::text("}".to_string()),
        ], BreakType::Consistent, 0)
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}