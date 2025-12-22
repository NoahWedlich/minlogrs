
use crate::includes::{
    essential::*,
    utils::*,
    core::{
        types::*,
        terms::*,
        predicates::*,
    }
};

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct PrimeFormula {
    body: Rc<MinlogPredicate>,
    arguments: Vec<MinlogTerm>,
    arity: Rc<MinlogType>,
}

impl PrimeFormula {
    pub fn create(body: Rc<MinlogPredicate>, arguments: Vec<MinlogTerm>) -> Rc<MinlogPredicate> {
        let arguments = arguments.into_iter()
            .filter(|v| !v.is_tuple() || !v.to_tuple().unwrap().elements().is_empty())
            .collect::<Vec<MinlogTerm>>();
        
        if arguments.is_empty() {
            return body;
        }
        
        let unpacked_arity = body.unpacked_arity();

        if unpacked_arity.len() < arguments.len() {
            panic!("Tried to create PrimeFormula with too many arguments");
        }
        
        for (i, arg) in arguments.iter().enumerate() {
            if arg.minlog_type() != unpacked_arity[i] {
                panic!("Argument type does not match predicate arity type");
            }
        }
        
        let arity = if unpacked_arity.len() == arguments.len() {
            TupleType::create_unit()
        } else {
            TupleType::create(unpacked_arity[arguments.len()..].to_vec())
        };

        PrimeFormula::collapse(&Rc::new(MinlogPredicate::Prime(PrimeFormula { body, arguments, arity })))
    }
    
    pub fn collapse(prime: &Rc<MinlogPredicate>) -> Rc<MinlogPredicate> {
        if !prime.is_prime() || !prime.to_prime().unwrap().body.is_prime() {
            prime.clone()
        } else {
            let mut prime = prime.to_prime().unwrap().clone();
            let mut args = prime.arguments.clone();
            
            while prime.body.is_prime() {
                let inner_prime = prime.body.to_prime().unwrap();
                args.extend(inner_prime.arguments.iter().cloned());
                prime.body = inner_prime.body.clone();
            }
            
            PrimeFormula::create(prime.body.clone(), args)
        }
    }
    
    pub fn body(&self) -> &Rc<MinlogPredicate> {
        &self.body
    }
    
    pub fn argument_count(&self) -> usize {
        self.arguments.len()
    }
    
    pub fn arguments(&self) -> &Vec<MinlogTerm> {
        &self.arguments
    }
    
    pub fn argument(&self, index: usize) -> Option<&MinlogTerm> {
        self.arguments.get(index)
    }
}

impl PredicateBody for PrimeFormula {
    fn arity(&self) -> Rc<MinlogType> {
        self.arity.clone()
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> Rc<MinlogPredicate> {
        if self.arguments.is_empty() {
            return self.body.normalize(eta, pi);
        }
        
        if pi {
            for arg in &self.arguments {
                if arg.is_tuple() || arg.is_match_term() {
                    println!("Warning: Pi-normalization of Prime Formulas is not implemented yet.");
                    break;
                }
            }
        }
        
        if self.body.is_comprehension_term() {
            let cterm = self.body.to_comprehension_term().unwrap();
            
            let applicable_arguments = min(self.arguments.len(), cterm.vars().len());
            let mut subst = PredicateSubstitution::make_empty();
            
            for i in 0..applicable_arguments {
                let var = &cterm.vars()[i];
                let arg = &self.arguments[i];
                
                if var.minlog_type() != arg.minlog_type() {
                    panic!("Type mismatch during beta-normalization of Prime Formula.");
                }
                
                if arg.contains_free_variable(var) {
                    panic!("Tried to apply term that contains the bound variable during beta-normalization of Prime Formula.");
                }
                
                subst.extend((var.clone().into(), arg.clone().into()));
            }
            
            let new_body = subst.substitute::<PredSubstEntry>(&cterm.body().into()).to_predicate().unwrap();
            
            let remaining_vars = cterm.vars()[applicable_arguments..].to_vec();
            let inner_pred = if remaining_vars.is_empty() {
                new_body.normalize(eta, pi)
            } else {
                ComprehensionTerm::create(remaining_vars, new_body).normalize(eta, pi)
            };
            
            let remaining_args = self.arguments[applicable_arguments..].to_vec();
            
            if remaining_args.is_empty() {
                inner_pred
            } else {
                PrimeFormula::create(inner_pred, remaining_args).normalize(eta, pi)
            }
        } else {
            let new_body = self.body.normalize(eta, pi);
            let new_arguments = self.arguments.iter()
                .map(|arg| arg.normalize(eta, pi))
                .collect();
            
            PrimeFormula::create(new_body, new_arguments)
        }
    }
    
    fn depth(&self) -> usize {
        1 + self.arguments.iter().map(|arg| arg.depth())
            .chain(std::iter::once(self.body.depth()))
            .max().unwrap_or(0)
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
        self.arguments.iter()
            .flat_map(|arg| arg.get_type_variables(&mut IndexSet::new()))
            .chain(self.body.get_type_variables(visited))
            .collect()
    }
    
    fn get_algebra_types(&self, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Rc<MinlogType>> {
        self.arguments.iter()
            .flat_map(|arg| arg.get_algebra_types(&mut IndexSet::new()))
            .chain(self.body.get_algebra_types(visited))
            .collect()
    }
    
    fn get_free_variables(&self, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<MinlogTerm> {
        self.arguments.iter()
            .flat_map(|arg| arg.get_free_variables(&mut IndexSet::new()))
            .chain(self.body.get_free_variables(visited))
            .collect()
    }
    
    fn get_bound_variables(&self, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<MinlogTerm> {
        self.arguments.iter()
            .flat_map(|arg| arg.get_bound_variables(&mut IndexSet::new()))
            .chain(self.body.get_bound_variables(visited))
            .collect()
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
            let new_arguments = self.arguments.iter()
                .map(|arg| arg.substitute(&tse, &to.to_term_subst_entry().unwrap()))
                .collect();
            let new_body = self.body.substitute(from, to);
            PrimeFormula::create(new_body, new_arguments)
        } else if let Some(pred) = from.to_predicate() && pred.is_prime() && self == pred.to_prime().unwrap() {
            to.to_predicate().unwrap()
        } else {
            let new_body = self.body.substitute(from, to);
            PrimeFormula::create(new_body, self.arguments.clone())
        }
    }
    
    fn first_conflict_with(&self, other: &Rc<MinlogPredicate>) -> Option<(PredSubstEntry, PredSubstEntry)> {
        if let Some(other_prime) = other.to_prime() {
            if self.arguments.len() != other_prime.arguments.len() {
                return Some((Rc::new(MinlogPredicate::Prime(self.clone())).into(), other.clone().into()));
            }
            
            for (arg_self, arg_other) in self.arguments.iter().zip(other_prime.arguments.iter()) {
                if let Some(conflict) = arg_self.first_conflict_with(arg_other) {
                    return Some((conflict.0.into(), conflict.1.into()));
                }
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
        
        if self.arguments.len() != prime_instance.arguments.len() {
            return MatchOutput::FailedMatch;
        }
        
        let mut conditions = self.arguments.iter().zip(prime_instance.arguments.iter())
            .filter_map(|(arg_pattern, arg_instance)| {
                if arg_pattern != arg_instance {
                    Some((arg_pattern.into(), arg_instance.into()))
                } else {
                    None
                }
            })
            .collect::<IndexMap<_, _>>();
        
        if self.body != prime_instance.body {
            conditions.insert(self.body.clone().into(), prime_instance.body.clone().into());
        }
        
        MatchOutput::Matched(conditions)
    }
}

impl PrettyPrintable for PrimeFormula {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        if self.arguments.is_empty() {
            return self.body.to_pp_element(detail);
        }
        
        let arguments = PPElement::list(
            self.arguments.iter().map(|arg| arg.to_pp_element(detail)).collect(),
            PPElement::break_elem(0, 0, false),
            PPElement::text(",".to_string()),
            PPElement::break_elem(1, 0, false),
            BreakType::Flexible,
        );
        
        PPElement::group(vec![
            self.body.to_pp_element(detail),
            PPElement::text(" (".to_string()),
            PPElement::break_elem(1, 4, false),
            arguments,
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