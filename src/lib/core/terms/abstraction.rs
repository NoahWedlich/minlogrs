
use std::rc::Rc;
use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};
use crate::core::types::minlog_type::MinlogType;
use crate::core::types::arrow_type::ArrowType;
use crate::core::terms::minlog_term::{TermBody, MinlogTerm, Totality};
use crate::core::terms::term_variable::TermVariable;

#[derive(Clone)]
pub struct Abstraction {
    vars: Vec<Rc<MinlogTerm>>,
    kernel: Rc<MinlogTerm>,
    minlog_type: Rc<MinlogType>,
}

impl Abstraction {
    pub fn create(vars: Vec<Rc<MinlogTerm>>, kernel: Rc<MinlogTerm>) -> Rc<MinlogTerm> {
        if vars.is_empty() {
            return kernel;
        }
        
        if vars.iter().any(|v| !v.is_variable()) {
            panic!("Tried to create an Abstraction with a non-variable term");
        }
        
        for (i, var) in vars.iter().enumerate() {
            for j in (i + 1)..vars.len() {
                if var == &vars[j] {
                    panic!("Tried to create an Abstraction with duplicate variables");
                }
            }
        }
        
        let var_types: Vec<Rc<MinlogType>> = vars.iter().map(|v| v.minlog_type()).collect();
        Abstraction::collapse(&Rc::new(MinlogTerm::Abstraction(Abstraction {
            minlog_type: ArrowType::create(var_types, kernel.minlog_type()),
            vars,
            kernel,
        })))
    }
    
    pub fn collapse(minlog_term: &Rc<MinlogTerm>) -> Rc<MinlogTerm> {
        if !minlog_term.is_abstraction() || !minlog_term.to_abstraction().unwrap().kernel.is_abstraction() {
            return Rc::clone(minlog_term);
        } else {
            let mut abstraction = minlog_term.to_abstraction().unwrap();
            let mut vars = abstraction.vars().clone();
            
            while abstraction.kernel().is_abstraction() {
                let next_abstraction = abstraction.kernel().to_abstraction().unwrap();
                vars.extend(next_abstraction.vars().clone());
                abstraction = next_abstraction;
            }
            
            return Abstraction::create(vars, Rc::clone(&abstraction.kernel()));
        }
    }
    
    pub fn closure(minlog_term: &Rc<MinlogTerm>) -> Rc<MinlogTerm> {
        let mut vars = vec![];
        for var in MinlogTerm::get_free_variables(minlog_term) {
            if var.is_variable() && !vars.contains(&var) {
                vars.push(var);
            }
        }
        
        Abstraction::create(vars, minlog_term.clone())
    }
    
    pub fn arity(&self) -> usize {
        self.vars.len()
    }
    
    pub fn vars(&self) -> &Vec<Rc<MinlogTerm>> {
        &self.vars
    }
    
    pub fn var(&self, index: usize) -> Option<&Rc<MinlogTerm>> {
        self.vars.get(index)
    }
    
    pub fn kernel(&self) -> &Rc<MinlogTerm> {
        &self.kernel
    }
}

impl TermBody for Abstraction {
    fn minlog_type(&self) -> Rc<MinlogType> {
        self.minlog_type.clone()
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> Rc<MinlogTerm> {
        let kernel = self.kernel.normalize(eta, pi);
        
        let mut vars = vec![];
        
        if eta {
            let free_vars = MinlogTerm::get_free_variables(&kernel);
            for var in &self.vars {
                if free_vars.contains(var) {
                    vars.push(var.clone());
                }
            }
        } else {
            vars = self.vars.clone();
        }

        if vars.is_empty() {
            kernel
        } else {
            Abstraction::create(vars, kernel)
        }
    }
    
    fn length(&self) -> usize {
        1 + self.kernel.length()
    }
    
    fn depth(&self) -> usize {
        1 + self.kernel.depth()
    }
    
    fn inner_free_variables(&self) -> Vec<Rc<MinlogTerm>> {
        let mut inner = MinlogTerm::get_free_variables(&self.kernel);
        
        inner.retain(|v| !self.vars.contains(v));
        
        inner
    }
    
    fn inner_bound_variables(&self) -> Vec<Rc<MinlogTerm>> {
        let mut inner = MinlogTerm::get_bound_variables(&self.kernel);
        
        for var in &self.vars {
            if !inner.contains(var) {
                inner.push(var.clone());
            }
        }
        
        inner
    }
    
    fn inner_constructors(&self) -> Vec<Rc<MinlogTerm>> {
        MinlogTerm::get_constructors(&self.kernel)
    }
    
    fn inner_program_terms(&self) -> Vec<Rc<MinlogTerm>> {
        MinlogTerm::get_program_terms(&self.kernel)
    }
    
    fn inner_internal_constants(&self) -> Vec<Rc<MinlogTerm>> {
        MinlogTerm::get_internal_constants(&self.kernel)
    }
    
    fn alpha_equivalent(&self, other: &Rc<MinlogTerm>,
        forward: &mut Vec<(TermVariable, TermVariable)>,
        backward: &mut Vec<(TermVariable, TermVariable)>) -> bool {
        
        if !other.is_abstraction() {
            return false;
        }
        
        let other = other.to_abstraction().unwrap();
        
        if self.vars.len() != other.vars.len() {
            return false;
        }
        
        for (v1, v2) in self.vars.iter().zip(other.vars.iter()) {
            if v1.minlog_type() != v2.minlog_type() {
                return false;
            }
            
            forward.push((v1.to_variable().unwrap().clone(), v2.to_variable().unwrap().clone()));
            backward.push((v2.to_variable().unwrap().clone(), v1.to_variable().unwrap().clone()));
        }
        
        self.kernel.alpha_equivalent(&other.kernel, forward, backward)
    }
    
    fn totality(&self, bound: &mut Vec<TermVariable>) -> Totality {
        bound.extend(self.vars.iter().filter_map(|v| v.to_variable().cloned()));
        self.kernel.totality(bound)
    }
}

impl PrettyPrintable for Abstraction {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        if self.vars.is_empty() {
            return self.kernel.to_pp_element(detail);
        }
        
        let mut variables = vec![];
        
        for (i, var) in self.vars.iter().enumerate() {
            variables.push(
                if i > 0 {
                    PPElement::group(vec![
                        PPElement::text(",".to_string()),
                        PPElement::break_elem(1, 4, false),
                        var.to_pp_element(detail)
                    ], BreakType::Flexible, 0)
                } else {
                    var.to_pp_element(detail)
                }
            );
            
            variables.push(PPElement::break_elem(0, 4, false));
        }
        
        let mut elements = vec![];
        
        elements.push(
            PPElement::group(vec![
                PPElement::text("(".to_string()),
                PPElement::break_elem(1, 4, false),
                PPElement::group(variables, BreakType::Flexible, 0),
                PPElement::break_elem(1, 0, false),
                PPElement::text(")".to_string())
            ], BreakType::Consistent, 0)
        );
        
        elements.push(PPElement::break_elem(1, 4, false));
        elements.push(PPElement::text("->".to_string()));
        elements.push(PPElement::break_elem(1, 4, false));
        
        elements.push(self.kernel.to_enclosed_pp_element(detail));
        
        PPElement::group(elements, BreakType::Flexible, 0)
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

impl PartialEq for Abstraction {
    fn eq(&self, other: &Self) -> bool {
        self.vars == other.vars && self.kernel == other.kernel
    }
}

impl Eq for Abstraction {}