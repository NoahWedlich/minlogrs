
use crate::includes::{
    essential::*,
    utils::*,
    core::{
        terms::*,
        proofs::*,
    }
};

#[derive(Clone, PartialEq, Eq)]
pub struct ProofContext {
    pub assumptions: IndexSet<Rc<MinlogProof>>,
    pub variables: IndexSet<Rc<MinlogTerm>>,
}

impl ProofContext {
    pub fn new() -> Self {
        ProofContext {
            assumptions: IndexSet::new(),
            variables: IndexSet::new(),
        }
    }
    
    pub fn is_empty(&self) -> bool {
        self.assumptions.is_empty() && self.variables.is_empty()
    }
    
    pub fn add_assumption(&mut self, assumption: Rc<MinlogProof>) {
        if !assumption.is_assumption() {
            panic!("Tried to add non-assumption to proof context.");
        }
        
        self.assumptions.insert(assumption);
    }
    
    pub fn add_variable(&mut self, variable: Rc<MinlogTerm>) {
        if !variable.is_variable() {
            panic!("Tried to add non-variable term to proof context.");
        }
        
        self.variables.insert(variable);
    }
    
    pub fn get_assumptions(&self) -> &IndexSet<Rc<MinlogProof>> {
        &self.assumptions
    }
    
    pub fn get_variables(&self) -> &IndexSet<Rc<MinlogTerm>> {
        &self.variables
    }
    
    pub fn contains_assumption(&self, assumption: &Rc<MinlogProof>) -> bool {
        self.assumptions.contains(assumption)
    }
    
    pub fn contains_variable(&self, variable: &Rc<MinlogTerm>) -> bool {
        self.variables.contains(variable)
    }
    
    pub fn remove_assumption(&mut self, assumption: &Rc<MinlogProof>) {
        self.assumptions.shift_remove(assumption);
    }
    
    pub fn remove_variable(&mut self, variable: &Rc<MinlogTerm>) {
        self.variables.shift_remove(variable);
    }
    
    pub fn clear(&mut self) {
        self.assumptions.clear();
        self.variables.clear();
    }
}

impl Default for ProofContext {
    fn default() -> Self {
        Self::new()
    }
}

impl PrettyPrintable for ProofContext {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        if self.is_empty() {
            PPElement::text("∅".to_string())
        } else {
            PPElement::list(
                self.assumptions.iter().map(|a| a.to_pp_element(detail))
                    .chain(self.variables.iter().map(|v| v.to_pp_element(detail)))
                    .collect(),
                PPElement::break_elem(4, 0, false),
                PPElement::break_elem(0, 0, false),
                PPElement::break_elem(0, 0, false),
                BreakType::Flexible
            )
        }
        
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}