
use std::{rc::Rc, collections::HashSet};

use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};
use crate::utils::proof_tree_display::{ProofTreeDisplayable, ProofTreeNode};

use crate::core::types::minlog_type::MinlogType;
use crate::core::terms::minlog_term::MinlogTerm;
use crate::core::formulas::minlog_formula::MinlogFormula;
use crate::core::predicates::minlog_predicate::MinlogPredicate;

use crate::core::proofs::minlog_proof::{MinlogProof, ProofBody};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Axiom {
    name: String,
    formula: Rc<MinlogFormula>,
}

impl Axiom {
    pub fn create(name: String, formula: Rc<MinlogFormula>) -> Rc<MinlogProof> {
        Rc::new(MinlogProof::Axiom(Axiom { name, formula }))
    }
    
    pub fn name(&self) -> &str {
        &self.name
    }
}

impl ProofBody for Axiom {
    fn proved_formula(&self) -> Rc<MinlogFormula> {
        self.formula.clone()
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> Rc<MinlogProof> {
        Rc::new(MinlogProof::Axiom(Axiom {
            name: self.name.clone(),
            formula: self.formula.normalize(eta, pi),
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
    
    fn get_axioms(&self) -> HashSet<Rc<MinlogProof>> {
        HashSet::from([Rc::new(MinlogProof::Axiom(self.clone()))])
    }
}

impl PrettyPrintable for Axiom {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        PPElement::group(vec![
            PPElement::text("<".to_string()),
            PPElement::break_elem(1, 4, false),
            PPElement::group(vec![
                PPElement::text(self.name.clone()),
                PPElement::break_elem(0, 0, false),
                PPElement::text(":".to_string()),
                PPElement::break_elem(1, 4, false),
                self.formula.to_pp_element(detail),
            ], BreakType::Flexible, 0),
            PPElement::break_elem(1, 0, false),
            PPElement::text(">".to_string()),
        ], BreakType::Consistent, 0)
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}

impl ProofTreeDisplayable for Axiom {
    fn to_proof_tree_node(&self) -> ProofTreeNode {
        ProofTreeNode::new_node(vec![
            ProofTreeNode::new_leaf(self.name.clone())
        ], self.formula.display_string(), Some("Axiom".to_string()))
    }
}