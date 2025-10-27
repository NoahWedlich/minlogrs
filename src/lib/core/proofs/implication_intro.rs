
use std::{rc::Rc, collections::HashSet};

use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};
use crate::utils::proof_tree_display::{ProofTreeDisplayable, ProofTreeNode};

use crate::core::types::minlog_type::MinlogType;
use crate::core::terms::minlog_term::MinlogTerm;

use crate::core::formulas::minlog_formula::MinlogFormula;
use crate::core::formulas::implication::Implication;

use crate::core::predicates::minlog_predicate::MinlogPredicate;

use crate::core::proofs::minlog_proof::{MinlogProof, ProofBody};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ImplicationIntro {
    assumption: Rc<MinlogProof>,
    conclusion: Rc<MinlogProof>,
    formula: Rc<MinlogFormula>,
}

impl ImplicationIntro {
    pub fn create(proof: Rc<MinlogProof>, assumption: Rc<MinlogProof>) -> Rc<MinlogProof> {
        if !assumption.is_assumption() {
            panic!("ImplicationIntro::create called with a non-assumption proof as assumption");
        }
        
        if !proof.contains_assumption(&assumption) {
            panic!("ImplicationIntro::create called with a proof that does not have the given assumption free");
        }
        
        let premise_formula = assumption.proved_formula();
        let conclusion_formula = proof.proved_formula();
        let implication_formula = Implication::create(vec![premise_formula], conclusion_formula);
        
        Rc::new(MinlogProof::ImplicationIntro(ImplicationIntro {
            assumption,
            conclusion: proof,
            formula: implication_formula,
        }))
    }

    pub fn assumption(&self) -> Rc<MinlogProof> {
        self.assumption.clone()
    }
    
    pub fn conclusion(&self) -> Rc<MinlogProof> {
        self.conclusion.clone()
    }
}

impl ProofBody for ImplicationIntro {
    fn proved_formula(&self) -> Rc<MinlogFormula> {
        self.formula.clone()
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> Rc<MinlogProof> {
        Rc::new(MinlogProof::ImplicationIntro(ImplicationIntro {
            assumption: self.assumption.normalize(eta, pi),
            conclusion: self.conclusion.normalize(eta, pi),
            formula: self.formula.normalize(eta, pi),
        }))
    }
    
    fn get_type_variables(&self) -> HashSet<Rc<MinlogType>> {
        self.assumption.get_type_variables()
            .union(&self.conclusion.get_type_variables())
            .cloned().collect()
    }
    
    fn get_algebra_types(&self) -> HashSet<Rc<MinlogType>> {
        self.assumption.get_algebra_types()
            .union(&self.conclusion.get_algebra_types())
            .cloned().collect()
    }
    
    fn get_free_variables(&self) -> HashSet<Rc<MinlogTerm>> {
        self.assumption.get_free_variables()
            .union(&self.conclusion.get_free_variables())
            .cloned().collect()
    }
    
    fn get_bound_variables(&self) -> HashSet<Rc<MinlogTerm>> {
        self.assumption.get_bound_variables()
            .union(&self.conclusion.get_bound_variables())
            .cloned().collect()
    }
    
    fn get_predicate_variables(&self) -> HashSet<Rc<MinlogPredicate>> {
        self.assumption.get_predicate_variables()
            .union(&self.conclusion.get_predicate_variables())
            .cloned().collect()
    }
    
    fn get_comprehension_terms(&self) -> HashSet<Rc<MinlogPredicate>> {
        self.assumption.get_comprehension_terms()
            .union(&self.conclusion.get_comprehension_terms())
            .cloned().collect()
    }
    
    fn get_inductive_predicates(&self) -> HashSet<Rc<MinlogPredicate>> {
        self.assumption.get_inductive_predicates()
            .union(&self.conclusion.get_inductive_predicates())
            .cloned().collect()
    }
    
    fn get_prime_formulas(&self) -> HashSet<Rc<MinlogFormula>> {
        self.assumption.get_prime_formulas()
            .union(&self.conclusion.get_prime_formulas())
            .cloned().collect()
    }

    fn get_goals(&self) -> HashSet<Rc<MinlogProof>> {
        self.conclusion.get_goals()
            .union(&self.assumption.get_goals())
            .cloned().collect()
    }

    fn get_assumptions(&self) -> HashSet<Rc<MinlogProof>> {
        self.conclusion.get_assumptions()
            .difference(&HashSet::from([self.assumption.clone()]))
            .cloned().collect()
    }
    
    fn get_axioms(&self) -> HashSet<Rc<MinlogProof>> {
        self.assumption.get_axioms()
            .union(&self.conclusion.get_axioms())
            .cloned().collect()
    }
    
    fn get_theorems(&self) -> HashSet<Rc<MinlogProof>> {
        self.assumption.get_theorems()
            .union(&self.conclusion.get_theorems())
            .cloned().collect()
    }
}

impl PrettyPrintable for ImplicationIntro {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        PPElement::group(vec![
            PPElement::text("→⁺ (".to_string()),
            PPElement::break_elem(1, 4, false),
            PPElement::list(
                vec![
                    self.conclusion.to_pp_element(detail),
                    self.assumption.to_pp_element(detail),
                ],
                PPElement::break_elem(0, 0, false),
                PPElement::text(",".to_string()),
                PPElement::break_elem(1, 0, false),
                BreakType::Consistent
            ),
            PPElement::break_elem(1, 0, false),
            PPElement::text(")".to_string()),
        ], BreakType::Consistent, 0)
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}

impl ProofTreeDisplayable for ImplicationIntro {
    fn to_proof_tree_node(&self) -> ProofTreeNode {
        ProofTreeNode::new_node(
            vec![self.conclusion.to_proof_tree_node()],
            self.formula.display_string(),
            Some(format!("→⁺{}", self.assumption.to_assumption().unwrap().name()))
        )
    }
}