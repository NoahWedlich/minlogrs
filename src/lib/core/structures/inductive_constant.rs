
use std::{rc::Rc, cell::RefCell, hash::{Hash, Hasher}, collections::HashSet};

use crate::utils::pretty_printer::*;

use crate::core::polarity::{Polarity, Polarized};

use crate::core::structures::algebra::Algebra;

use crate::core::types::minlog_type::MinlogType;

use crate::core::terms::minlog_term::MinlogTerm;
use crate::core::terms::constructor::Constructor;

use crate::core::predicates::minlog_predicate::MinlogPredicate;

use crate::core::formulas::minlog_formula::MinlogFormula;

#[derive(Clone, PartialEq, Eq)]
pub struct InductiveConstant {
    name: String,
    arity: Vec<Rc<MinlogType>>,
    clauses: RefCell<Vec<(String, Rc<MinlogFormula>)>>,
    algebra: RefCell<Option<Rc<Algebra>>>,
}

impl InductiveConstant {
    pub fn create(name: String, arity: Vec<Rc<MinlogType>>) -> Rc<InductiveConstant> {
        Rc::new(InductiveConstant {
            name,
            arity,
            clauses: RefCell::new(vec![]),
            algebra: RefCell::new(None),
        })
    }
    
    pub fn name(&self) -> &String {
        &self.name
    }
    
    pub fn arity(&self) -> &Vec<Rc<MinlogType>> {
        &self.arity
    }
    
    pub fn add_clause(&self, clause_name: String, clause_body: Rc<MinlogFormula>) {
        if self.clauses.borrow().iter().any(|(n, _)| n == &clause_name) {
            panic!("Clause with name '{}' already exists in inductive constant '{}'", clause_name, self.name);
        }

        let ipreds = clause_body.get_polarized_inductive_preds(Polarity::StrictlyPositive);
        if !ipreds.iter().any(|p| p.polarity.is_strictly_positive()
            && p.value.to_inductive_predicate().unwrap().definition().as_ref() == self) {
            panic!("Clause body does not contain strictly positive occurrence of inductive constant '{}'", self.name);
        }
        
        self.clauses.borrow_mut().push((clause_name, clause_body));
    }
    
    pub fn clauses(&self) -> Vec<(String, Rc<MinlogFormula>)> {
        self.clauses.borrow().clone()
    }
    
    pub fn make_computational(&self, algebra: Rc<Algebra>, existing: bool) {
        *self.algebra.borrow_mut() = Some(algebra.clone());
        for (name, body) in self.clauses.borrow().iter() {
            let et_type = body.extracted_type();
            
            if existing {
                if let Some(existing) = algebra.constructor(name) {
                    if existing.minlog_type() != et_type {
                        panic!("Existing constructor '{}' has type '{}', but extracted type is '{}'",
                            name, existing.minlog_type().debug_string(), et_type.debug_string());
                    }
                } else if algebra.constructors().iter().filter(|c| c.minlog_type() == et_type).count() > 1 {
                    panic!("Multiple constructors with extracted type '{}' exist in the algebra, but none named '{}'",
                        et_type.debug_string(), name);
                } else if algebra.constructors().iter().all(|c| c.minlog_type() != et_type) {
                    panic!("No constructor with extracted type '{}' exists in the algebra for clause '{}'",
                        et_type.debug_string(), name);
                }

            } else {
                let name = format!("{}^et", name);
                algebra.add_constructor(Constructor::create(name, et_type));
            }
        }
        
        *self.algebra.borrow_mut() = Some(algebra);
    }
    
    pub fn is_computational(&self) -> bool {
        self.algebra.borrow().is_some()
    }
    
    pub fn get_algebra(&self) -> Option<Rc<Algebra>> {
        self.algebra.borrow().clone()
    }
    
    pub fn get_type_variables(&self) -> HashSet<Rc<MinlogType>> {
        self.clauses.borrow().iter().flat_map(|(_, body)| body.get_type_variables()).collect()
    }
    
    pub fn get_free_variables(&self) -> HashSet<Rc<MinlogTerm>> {
        self.clauses.borrow().iter().flat_map(|(_, body)| body.get_free_variables()).collect()
    }
    
    pub fn get_bound_variables(&self) -> HashSet<Rc<MinlogTerm>> {
        self.clauses.borrow().iter().flat_map(|(_, body)| body.get_bound_variables()).collect()
    }
    
    pub fn get_polarized_pred_vars(&self, current: Polarity) -> HashSet<Polarized<Rc<MinlogPredicate>>> {
        self.clauses.borrow().iter().flat_map(|(_, body)| body.get_polarized_pred_vars(current)).collect()
    }

    pub fn get_polarized_comp_terms(&self, current: Polarity) -> HashSet<Polarized<Rc<MinlogPredicate>>> {
        self.clauses.borrow().iter().flat_map(|(_, body)| body.get_polarized_comp_terms(current)).collect()
    }

    pub fn get_polarized_inductive_preds(&self, current: Polarity) -> HashSet<Polarized<Rc<MinlogPredicate>>> {
        self.clauses.borrow().iter().flat_map(|(_, body)| body.get_polarized_inductive_preds(current)).collect()
    }
    
    pub fn get_polarized_prime_formulas(&self, current: Polarity) -> HashSet<Polarized<Rc<MinlogFormula>>> {
        self.clauses.borrow().iter().flat_map(|(_, body)| body.get_polarized_prime_formulas(current)).collect()
    }
}

impl PrettyPrintable for InductiveConstant {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        let tvars = self.get_type_variables();
        let tmvars = self.get_free_variables();
        let pvars = self.get_polarized_pred_vars(Polarity::Unknown)
            .into_iter().map(|p| p.value).collect::<HashSet<_>>();
        
        let has_tvars = !tvars.is_empty();
        let has_tmvars = !tmvars.is_empty();
        let has_pvars = !pvars.is_empty();
        
        let name = if !has_tvars && !has_tmvars && !has_pvars {
            PPElement::text(self.name.clone())
        } else {
            let tvars = PPElement::list(
                tvars.into_iter().map(|tv| tv.to_pp_element(false)).collect(),
                PPElement::break_elem(0, 0, false),
                PPElement::text(",".to_string()),
                PPElement::break_elem(1, 0, true),
                BreakType::Flexible,
            );
            
            let tmvars = PPElement::list(
                tmvars.into_iter().map(|tv| tv.to_pp_element(false)).collect(),
                PPElement::break_elem(0, 0, false),
                PPElement::text(",".to_string()),
                PPElement::break_elem(1, 0, true),
                BreakType::Flexible,
            );
            
            let pvars = PPElement::list(
                pvars.into_iter().map(|pv| pv.to_pp_element(false)).collect(),
                PPElement::break_elem(0, 0, false),
                PPElement::text(",".to_string()),
                PPElement::break_elem(1, 0, true),
                BreakType::Flexible,
            );
            
            let mut variables = vec![];
            if has_tvars {
                variables.push(tvars);
                if has_tmvars || has_pvars {
                    variables.push(PPElement::break_elem(4, 0, false));
                }
            }
            
            if has_tmvars {
                variables.push(tmvars);
                if has_pvars {
                    variables.push(PPElement::break_elem(4, 0, false));
                }
            }
            
            if has_pvars {
                variables.push(pvars);
            }
            
            PPElement::group(vec![
                PPElement::text(self.name.clone()),
                PPElement::text("<".to_string()),
                PPElement::break_elem(1, 4, false),
                PPElement::group(variables, BreakType::Consistent, 0),
                PPElement::break_elem(1, 0, false),
                PPElement::text(">".to_string())
            ], BreakType::Consistent, 0)
        };
        
        if detail {
            let clauses = PPElement::list(
                self.clauses.borrow().iter().map(|(_, body)| body.to_pp_element(true)).collect(),
                PPElement::break_elem(0, 0, false),
                PPElement::text(";".to_string()),
                PPElement::break_elem(1, 0, true),
                BreakType::Flexible,
            );
            
            
            PPElement::group(vec![
                name,
                PPElement::text(" {".to_string()),
                PPElement::break_elem(1, 4, true),
                clauses,
                PPElement::break_elem(1, 0, true),
                PPElement::text("}".to_string())
            ], BreakType::Consistent, 0)
        } else {
            name
        }
    }
}

impl Hash for InductiveConstant {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.arity.hash(state);
    }
}