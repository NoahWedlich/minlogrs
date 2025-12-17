
use crate::includes::{
    essential::*,
    utils::*,
    core::{
        types::*,
        terms::*,
        predicates::*,
    }
};

wrapper_enum::wrapper_enum! {
    pub fwd bnd trait PredicateBody: PrettyPrintable + Clone + PartialEq + Eq + Hash {
        pub fwd fn arity(&self) -> Rc<MinlogType>
        
        pub fwd fn normalize(&self, eta: bool, pi: bool) -> Rc<MinlogPredicate>
        
        pub fwd fn depth(&self) -> usize {
            0
        }
        
        pub fwd fn extracted_type_pattern(&self) -> Rc<MinlogType>
        pub fwd fn extracted_type(&self) -> Rc<MinlogType>
        pub fwd fn et_pattern_to_et(&self) -> TermSubstitution
        
        pub fwd fn get_type_variables(&self, _visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Rc<MinlogType>> {
            IndexSet::new()
        }
        
        pub fwd fn get_algebra_types(&self, _visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Rc<MinlogType>> {
            IndexSet::new()
        }

        pub fwd fn get_free_variables(&self, _visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<MinlogTerm> {
            IndexSet::new()
        }
        
        pub fwd fn get_bound_variables(&self, _visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<MinlogTerm> {
            IndexSet::new()
        }
        
        pub fwd fn get_polarized_pred_vars(&self, _current: Polarity, _visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Polarized<Rc<MinlogPredicate>>> {
            IndexSet::new()
        }
        
        pub fwd fn get_polarized_comp_terms(&self, _current: Polarity, _visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Polarized<Rc<MinlogPredicate>>> {
            IndexSet::new()
        }
        
        pub fwd fn get_polarized_inductive_preds(&self, _current: Polarity, _visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Polarized<Rc<MinlogPredicate>>> {
            IndexSet::new()
        }
        
        pub fwd fn get_polarized_prime_formulas(&self, _current: Polarity, _visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Polarized<Rc<MinlogPredicate>>> {
            IndexSet::new()
        }
        
        pub fwd fn substitute(&self, from: &PredSubstEntry, to: &PredSubstEntry) -> Rc<MinlogPredicate>
        
        pub fwd fn first_conflict_with(&self, other: &Rc<MinlogPredicate>) -> Option<(PredSubstEntry, PredSubstEntry)>
        
        pub fwd fn match_with(&self, ctx: &Rc<MinlogPredicate>) -> MatchOutput<PredSubstEntry>
    }
    
    #[derive(PartialEq, Eq, Hash)]
    pub enum MinlogPredicate {
        Wildcard(wildcard: PredicateWildcard),
        Variable(variable: PredicateVariable),
        Comprehension(comprehension_term: ComprehensionTerm),
        InductivePredicate(inductive_predicate: InductivePredicate),
        Prime(prime: PrimeFormula),
        Implication(implication: Implication),
        AllQuantifier(all_quantifier: AllQuantifier),
    }
    
    ext bnd trait PrettyPrintable {
        fwd fn to_pp_element(&self, detail: bool) -> PPElement

        fwd fn requires_parens(&self, detail: bool) -> bool

        fwd fn open_paren(&self) -> String

        fwd fn close_paren(&self) -> String
    }
}

impl MinlogPredicate {
    pub fn unpacked_arity(&self) -> Vec<Rc<MinlogType>> {
        if let Some(tuple_type) = self.arity().to_tuple() {
            tuple_type.types().clone()
        } else {
            vec![self.arity().clone()]
        }
    }
    
    pub fn is_formula(&self) -> bool {
        self.unpacked_arity().is_empty()
    }
    
    pub fn to_cterm(pred: &Rc<MinlogPredicate>) -> Rc<MinlogPredicate> {
        let arity = pred.unpacked_arity();
        let vars = arity.iter().enumerate().map(|(i, t)| {
            TermVariable::create(format!("T{}", i), t.clone())
        }).collect::<Vec<_>>();
        let prime_formula = PrimeFormula::create(pred.clone(), vars.clone());
        ComprehensionTerm::create(vars, prime_formula)
    }
    
    pub fn get_predicate_variables(&self, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Rc<MinlogPredicate>> {
        self.get_polarized_pred_vars(Polarity::Unknown, visited)
            .into_iter().map(|p| p.value).collect()
    }
    
    pub fn get_comprehension_terms(&self, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Rc<MinlogPredicate>> {
        self.get_polarized_comp_terms(Polarity::Unknown, visited)
            .into_iter().map(|p| p.value).collect()
    }
    
    pub fn get_inductive_predicates(&self, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Rc<MinlogPredicate>> {
        self.get_polarized_inductive_preds(Polarity::Unknown, visited)
            .into_iter().map(|p| p.value).collect()
    }

    pub fn get_prime_formulas(&self, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Rc<MinlogPredicate>> {
        self.get_polarized_prime_formulas(Polarity::Unknown, visited)
            .into_iter().map(|p| p.value).collect()
    }

    pub fn contains_type_variable(&self, var: &Rc<MinlogType>) -> bool {
        var.is_variable() && self.get_type_variables(&mut IndexSet::new()).contains(var)
    }
    
    pub fn contains_algebra_type(&self, alg: &Rc<MinlogType>) -> bool {
        alg.is_algebra() && self.get_algebra_types(&mut IndexSet::new()).contains(alg)
    }
    
    pub fn contains_free_variable(&self, var: &MinlogTerm) -> bool {
        var.is_variable() && self.get_free_variables(&mut IndexSet::new()).contains(var)
    }
    
    pub fn contains_bound_variable(&self, var: &MinlogTerm) -> bool {
        var.is_variable() && self.get_bound_variables(&mut IndexSet::new()).contains(var)
    }
    
    pub fn contains_predicate_variable(&self, pvar: &Rc<MinlogPredicate>) -> bool {
        pvar.is_variable() && self.get_predicate_variables(&mut IndexSet::new()).contains(pvar)
    }
    
    pub fn contains_comprehension_term(&self, cterm: &Rc<MinlogPredicate>) -> bool {
        cterm.is_comprehension_term() && self.get_comprehension_terms(&mut IndexSet::new()).contains(cterm)
    }
    
    pub fn contains_inductive_predicate(&self, ipred: &Rc<MinlogPredicate>) -> bool {
        ipred.is_inductive_predicate() && self.get_inductive_predicates(&mut IndexSet::new()).contains(ipred)
    }
    
    pub fn contains_prime_formula(&self, pform: &Rc<MinlogPredicate>) -> bool {
        pform.is_prime() && self.get_prime_formulas(&mut IndexSet::new()).contains(pform)
    }
}