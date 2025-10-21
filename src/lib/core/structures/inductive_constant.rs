
use std::{rc::Rc, cell::RefCell};

use crate::utils::pretty_printer::*;

use crate::core::polarity::Polarity;

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
    type_variables: Vec<Rc<MinlogType>>,
    term_variables: Vec<Rc<MinlogTerm>>,
    pred_variables: Vec<Rc<MinlogPredicate>>,
}

impl InductiveConstant {
    pub fn create(name: String, arity: Vec<Rc<MinlogType>>, type_variables: Vec<Rc<MinlogType>>,
        term_variables: Vec<Rc<MinlogTerm>>, pred_variables: Vec<Rc<MinlogPredicate>>) -> Rc<InductiveConstant> {
        Rc::new(InductiveConstant {
            name,
            arity,
            clauses: RefCell::new(vec![]),
            algebra: RefCell::new(None),
            type_variables,
            term_variables,
            pred_variables,
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

        for tvar in clause_body.get_type_variables() {
            if !self.type_variables.contains(&tvar) {
                panic!("Clause body contains type variable '{}' not in the inductive constant's type variables", tvar.debug_string());
            }
        }
        
        for term_var in clause_body.get_free_variables() {
            let var_type = term_var.minlog_type();
            if !self.term_variables.contains(&term_var) {
                panic!("Clause body contains free term variable '{}' of type '{}' not in the inductive constant's term variables",
                    term_var.debug_string(), var_type.debug_string());
            }
        }
        
        for pred_var in clause_body.get_predicate_variables() {
            let var_arity = pred_var.arity();
            if !self.pred_variables.contains(&pred_var) {
                panic!("Clause body contains predicate variable '{}' of arity '{:?}' not in the inductive constant's predicate variables",
                    pred_var.debug_string(), var_arity.iter().map(|t| t.debug_string()).collect::<Vec<String>>());
            }
        }
        
        self.clauses.borrow_mut().push((clause_name, clause_body));
    }
    
    pub fn clauses(&self) -> Vec<(String, Rc<MinlogFormula>)> {
        self.clauses.borrow().clone()
    }
    
    pub fn make_computational(&self, algebra: Rc<Algebra>, existing: bool) {
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
    
    pub fn get_type_variables(&self) -> &Vec<Rc<MinlogType>> {
        &self.type_variables
    }
    
    pub fn get_term_variables(&self) -> &Vec<Rc<MinlogTerm>> {
        &self.term_variables
    }
    
    pub fn get_predicate_variables(&self) -> &Vec<Rc<MinlogPredicate>> {
        &self.pred_variables
    }
}

impl PrettyPrintable for InductiveConstant {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        let tvars = PPElement::list(
            self.get_type_variables().iter().map(|tv| tv.to_pp_element(detail)).collect(),
            PPElement::break_elem(0, 4, false),
            PPElement::text(",".to_string()),
            PPElement::break_elem(1, 4, true),
            BreakType::Flexible,
        );
        
        let tmvars = PPElement::list(
            self.get_term_variables().iter().map(|tv| tv.to_pp_element(detail)).collect(),
            PPElement::break_elem(0, 4, false),
            PPElement::text(",".to_string()),
            PPElement::break_elem(1, 4, true),
            BreakType::Flexible,
        );
        
        let pvars = PPElement::list(
            self.get_predicate_variables().iter().map(|pv| pv.to_pp_element(detail)).collect(),
            PPElement::break_elem(0, 4, false),
            PPElement::text(",".to_string()),
            PPElement::break_elem(1, 4, true),
            BreakType::Flexible,
        );
        
        let has_tvars = !self.get_type_variables().is_empty();
        let has_tmvars = !self.get_term_variables().is_empty();
        let has_pvars = !self.get_predicate_variables().is_empty();
        
        let mut variables = vec![];
        if has_tvars {
            variables.push(tvars);
            variables.push(PPElement::break_elem(1, 0, false));
            if has_tmvars || has_pvars {
                variables.push(PPElement::text("-------".to_string()));
            }
        }
        
        if has_tmvars {
            variables.push(tmvars);
            variables.push(PPElement::break_elem(1, 0, false));
            if has_pvars {
                variables.push(PPElement::text("-------".to_string()));
            }
        }
        
        if has_pvars {
            variables.push(pvars);
            variables.push(PPElement::break_elem(1, 0, false));
        }
        
        let name = if !has_tvars && !has_tmvars && !has_pvars {
            PPElement::text(self.name.clone())
        } else {
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