
use std::{rc::Rc, collections::HashSet};

use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};
use crate::utils::proof_tree_display::{ProofTreeDisplayable, ProofTreeNode};

use crate::core::types::minlog_type::MinlogType;
use crate::core::terms::minlog_term::MinlogTerm;
use crate::core::formulas::minlog_formula::MinlogFormula;
use crate::core::predicates::minlog_predicate::MinlogPredicate;

use crate::core::proofs::minlog_proof::{MinlogProof, ProofBody};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Assumption {
    name: String,
    formula: Rc<MinlogFormula>,
    index: usize,
}

impl Assumption {
    pub fn create(name: String, formula: Rc<MinlogFormula>) -> Rc<MinlogProof> {
        Rc::new(MinlogProof::Assumption(Assumption { name, formula, index: 0 }))
    }
    
    pub fn unshadow(proof: &Rc<MinlogProof>) -> Rc<MinlogProof> {
        if let Some(assump) = proof.to_assumption() {
            Rc::new(MinlogProof::Assumption(Assumption {
                name: assump.name.clone(),
                formula: assump.formula.clone(),
                index: assump.index + 1,
            }))
        } else {
            panic!("Called Assumption::unshadow on a non-assumption proof");
        }
    }
    
    pub fn name(&self) -> &str {
        &self.name
    }
    
    pub fn index(&self) -> usize {
        self.index
    }
}

impl ProofBody for Assumption {
    fn proved_formula(&self) -> Rc<MinlogFormula> {
        self.formula.clone()
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> Rc<MinlogProof> {
        Rc::new(MinlogProof::Assumption(Assumption {
            name: self.name.clone(),
            formula: self.formula.normalize(eta, pi),
            index: self.index,
        }))
    }
    
    fn get_type_variables(&self) -> HashSet<Rc<MinlogType>> {
        self.formula.get_type_variables()
    }
    
    fn get_algebra_types(&self) -> HashSet<Rc<MinlogType>> {
        self.formula.get_algebra_types()
    }
    
    fn get_free_variables(&self) -> HashSet<Rc<MinlogTerm>> {
        self.formula.get_free_variables()
    }
    
    fn get_bound_variables(&self) -> HashSet<Rc<MinlogTerm>> {
        self.formula.get_bound_variables()
    }
    
    fn get_predicate_variables(&self) -> HashSet<Rc<MinlogPredicate>> {
        self.formula.get_predicate_variables()
    }
    
    fn get_comprehension_terms(&self) -> HashSet<Rc<MinlogPredicate>> {
        self.formula.get_comprehension_terms()
    }
    
    fn get_inductive_predicates(&self) -> HashSet<Rc<MinlogPredicate>> {
        self.formula.get_inductive_predicates()
    }
    
    fn get_prime_formulas(&self) -> HashSet<Rc<MinlogFormula>> {
        self.formula.get_prime_formulas()
    }
    
    fn get_assumptions(&self) -> HashSet<Rc<MinlogProof>> {
        HashSet::from([Rc::new(MinlogProof::Assumption(self.clone()))])
    }
}

impl PrettyPrintable for Assumption {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        PPElement::group(vec![
            if self.index > 0 {
                PPElement::text(format!("{}_{}", self.name, self.index))
            } else {
                PPElement::text(self.name.clone())
            },
            PPElement::break_elem(0, 0, false),
            PPElement::text(":".to_string()),
            PPElement::break_elem(0, 1, false),
            self.formula.to_pp_element(detail),
        ], BreakType::Flexible, 0)
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}

impl ProofTreeDisplayable for Assumption {
    fn to_proof_tree_node(&self) -> ProofTreeNode {
        ProofTreeNode::new_leaf(self.display_string())
    }
}