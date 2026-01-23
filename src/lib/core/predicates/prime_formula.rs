
use crate::includes::{
    essential::*,
    utils::*,
    core::{
        types::*,
        terms::*,
        predicates::*,
    }
};

#[derive(PartialEq, Eq, Clone, Hash)]
pub struct PrimeFormula {
    body: Rc<MinlogPredicate>,
    argument: MinlogTerm,
    arity: Rc<MinlogType>,
}

impl PrimeFormula {
    pub fn create(body: Rc<MinlogPredicate>, argument: MinlogTerm) -> Rc<MinlogPredicate> {
        if argument.is_tuple() && argument.to_tuple().unwrap().elements().is_empty() {
            return body;
        }
        
        let unpacked_arity = body.unpacked_arity();
        
        if unpacked_arity.is_empty() {
            panic!("Tried to create PrimeFormula with no argument types in predicate arity");
        }
        
        if argument.minlog_type() != unpacked_arity[0] {
            panic!("Argument type does not match predicate arity type, expected {}, got {}",
                unpacked_arity[0].debug_string(),
                argument.minlog_type().debug_string()
            );
        }
        
        let arity = if unpacked_arity.len() == 1 {
            TupleType::create_unit()
        } else {
            TupleType::create(unpacked_arity[1..].to_vec())
        };

        Rc::new(MinlogPredicate::Prime(PrimeFormula { body, argument, arity }))
    }
    
    pub fn create_nested(body: Rc<MinlogPredicate>, arguments: Vec<MinlogTerm>) -> Rc<MinlogPredicate> {
        let mut current_body = body;
        
        for arg in arguments.into_iter() {
            current_body = PrimeFormula::create(current_body, arg);
        }
        
        current_body
    }
    
    pub fn argument(&self) -> &MinlogTerm {
        &self.argument
    }
    
    pub fn all_arguments(&self) -> Vec<MinlogTerm> {
        let mut current = self;
        let mut args = vec![current.argument.clone()];
        
        while let Some(next_prime) = current.body.to_prime() {
            args.push(next_prime.argument.clone());
            current = next_prime;
        }
        
        args.into_iter().rev().collect()
    }
    
    pub fn argument_at(&self, index: usize) -> Option<&MinlogTerm> {
        if index == 0 {
            Some(&self.argument)
        } else if self.body.is_prime() {
            self.body.to_prime().unwrap().argument_at(index - 1)
        } else {
            None
        }
    }
    
    pub fn body(&self) -> &Rc<MinlogPredicate> {
        &self.body
    }
    
    pub fn final_body(&self) -> &Rc<MinlogPredicate> {
        if let Some(next_prime) = self.body.to_prime() {
            next_prime.final_body()
        } else {
            &self.body
        }
    }
}

impl PredicateBody for PrimeFormula {
    fn arity(&self) -> Rc<MinlogType> {
        self.arity.clone()
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> Rc<MinlogPredicate> {
        if pi && (self.argument.is_tuple() || self.argument.is_match_term()) {
            println!("Warning: Pi-normalization of Prime Formulas is not implemented yet.");
        }
        
        if self.body.is_comprehension_term() {
            let cterm = self.body.to_comprehension_term().unwrap();
            
            let var = cterm.var();
            
            if self.argument.minlog_type() != var.minlog_type() {
                panic!("Type mismatch during beta-normalization of Prime Formula.");
            }
            
            if self.argument.contains_free_variable(var) {
                panic!("Tried to apply term that contains the bound variable during beta-normalization of Prime Formula.");
            }
            
            cterm.body().substitute(
                &var.clone().into(),
                &self.argument.clone().into()
            ).normalize(eta, pi)
        } else {
            let new_body = self.body.normalize(eta, pi);
            let new_arg = self.argument.normalize(eta, pi);
            
            let result = PrimeFormula::create(new_body.clone(), new_arg);
            
            if new_body.is_comprehension_term() {
                result.normalize(eta, pi)
            } else {
                result
            }
        }
    }
    
    fn depth(&self) -> usize {
        1 + max(self.argument.depth(), self.body.depth())
    }
    
    fn extracted_type_pattern(&self) -> Rc<MinlogType> {
        self.body.extracted_type_pattern()
    }
    
    fn extracted_type(&self) -> Rc<MinlogType> {
        self.body.extracted_type()
    }
    
    fn et_pattern_to_et(&self) -> TermSubstitution {
        self.body.et_pattern_to_et()
    }
    
    fn get_type_variables(&self, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Rc<MinlogType>> {
        self.body.get_type_variables(visited)
            .union(&self.argument.get_type_variables(&mut IndexSet::new()))
            .cloned().collect()
    }
    
    fn get_algebra_types(&self, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Rc<MinlogType>> {
        self.body.get_algebra_types(visited)
            .union(&self.argument.get_algebra_types(&mut IndexSet::new()))
            .cloned().collect()
    }
    
    fn get_free_variables(&self, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<MinlogTerm> {
        self.body.get_free_variables(visited)
            .union(&self.argument.get_free_variables(&mut IndexSet::new()))
            .cloned().collect()
    }
    
    fn get_bound_variables(&self, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<MinlogTerm> {
        self.body.get_bound_variables(visited)
            .union(&self.argument.get_bound_variables(&mut IndexSet::new()))
            .cloned().collect()
    }
    
    fn get_polarized_pred_vars(&self, _current: Polarity, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Polarized<Rc<MinlogPredicate>>> {
        self.body.get_polarized_pred_vars(_current, visited)
    }
    
    fn get_polarized_comp_terms(&self, _current: Polarity, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Polarized<Rc<MinlogPredicate>>> {
        self.body.get_polarized_comp_terms(_current, visited)
    }
    
    fn get_polarized_inductive_preds(&self, _current: Polarity, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Polarized<Rc<MinlogPredicate>>> {
        self.body.get_polarized_inductive_preds(_current, visited)
    }
    
    fn get_polarized_prime_formulas(&self, current: Polarity, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Polarized<Rc<MinlogPredicate>>> {
        let mut primes = self.body.get_polarized_prime_formulas(current, visited);
        primes.insert(Polarized::new(current, Rc::new(MinlogPredicate::Prime(self.clone()))));
        primes
    }
    
    fn substitute(&self, from: &PredSubstEntry, to: &PredSubstEntry) -> Rc<MinlogPredicate> {
        if let Some(tse) = from.to_term_subst_entry() {
            let new_arg = self.argument.substitute(&tse, &to.to_term_subst_entry().unwrap());
            let new_body = self.body.substitute(from, to);
            
            PrimeFormula::create(new_body, new_arg)
        } else if let Some(pred) = from.to_predicate() && pred.is_prime() && self == pred.to_prime().unwrap() {
            to.to_predicate().unwrap()
        } else {
            let new_body = self.body.substitute(from, to);
            PrimeFormula::create(new_body, self.argument.clone())
        }
    }
    
    fn first_conflict_with(&self, other: &Rc<MinlogPredicate>) -> Option<(PredSubstEntry, PredSubstEntry)> {
        if let Some(other_prime) = other.to_prime() {
            if let Some(conflict) = self.argument.first_conflict_with(&other_prime.argument) {
                return Some((conflict.0.into(), conflict.1.into()));
            }
            
            self.body.first_conflict_with(&other_prime.body)
        } else {
            Some((Rc::new(MinlogPredicate::Prime(self.clone())).into(), other.clone().into()))
        }
    }
    
    fn match_with(&self, instance: &Rc<MinlogPredicate>) -> MatchOutput<PredSubstEntry> {
        if !instance.is_prime() {
            return MatchOutput::FailedMatch;
        }
        
        let prime_instance = instance.to_prime().unwrap();
        
        let conditions = IndexMap::from([
            (self.argument.clone().into(), prime_instance.argument.clone().into()),
            (self.body.clone().into(), prime_instance.body.clone().into()),
        ]);
        
        MatchOutput::Matched(conditions)
    }
}

impl PrettyPrintable for PrimeFormula {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        PPElement::group(vec![
            self.body.to_pp_element(detail),
            PPElement::text(" (".to_string()),
            PPElement::break_elem(1, 4, false),
            self.argument.to_pp_element(detail),
            PPElement::break_elem(1, 0, false),
            PPElement::text(")".to_string()),
        ], BreakType::Consistent, 0)
    }
    
    fn requires_parens(&self, detail: bool) -> bool {
        self.body.requires_parens(detail)
    }
    
    fn open_paren(&self) -> String {
        "(".to_string()
    }
    
    fn close_paren(&self) -> String {
        ")".to_string()
    }
}