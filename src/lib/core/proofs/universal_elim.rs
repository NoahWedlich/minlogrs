
use std::{rc::Rc, collections::HashSet};

use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};
use crate::utils::proof_tree_display::{ProofTreeDisplayable, ProofTreeNode};

use crate::core::types::minlog_type::MinlogType;
use crate::core::terms::minlog_term::MinlogTerm;

use crate::core::formulas::minlog_formula::MinlogFormula;
use crate::core::formulas::all_quantifier::AllQuantifier;

use crate::core::predicates::minlog_predicate::MinlogPredicate;

use crate::core::proofs::minlog_proof::{MinlogProof, ProofBody};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct UniversalElim {
    proof: Rc<MinlogProof>,
    term: Rc<MinlogTerm>,
    replaced_variable: Rc<MinlogTerm>,
    formula: Rc<MinlogFormula>,
}

impl UniversalElim {
    pub fn create(proof: Rc<MinlogProof>, term: Rc<MinlogTerm>) -> Rc<MinlogProof> {
        let universal_formula = proof.proved_formula();
        
        if !universal_formula.is_all_quantifier() {
            panic!("UniversalElim::create called with a proof that does not prove a universal formula");
        }
        
        let all_quantifier = universal_formula.to_all_quantifier().unwrap();
        let vars = all_quantifier.vars();
        
        if vars.is_empty() {
            panic!("UniversalElim::create called with an universal formula that has no bound variables");
        }
        
        let var = vars[0].clone();
        let remaining_vars = vars[1..].to_vec();
        
        let inner_formula = all_quantifier.body().substitute(&var.clone().into(), &term.clone().into());
        let resulting_formula = if remaining_vars.is_empty() {
            inner_formula
        } else {
            AllQuantifier::create(remaining_vars, inner_formula)
        };
        
        Rc::new(MinlogProof::UniversalElim(UniversalElim {
            proof,
            term,
            replaced_variable: var,
            formula: resulting_formula,
        }))
    }
    
    pub fn proof(&self) -> Rc<MinlogProof> {
        self.proof.clone()
    }
    
    pub fn term(&self) -> Rc<MinlogTerm> {
        self.term.clone()
    }
}

impl ProofBody for UniversalElim {
    fn proved_formula(&self) -> Rc<MinlogFormula> {
        self.formula.clone()
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> Rc<MinlogProof> {
        Rc::new(MinlogProof::UniversalElim(UniversalElim {
            proof: self.proof.normalize(eta, pi),
            term: self.term.clone(),
            replaced_variable: self.replaced_variable.clone(),
            formula: self.formula.normalize(eta, pi),
        }))
    }
    
    fn get_type_variables(&self) -> HashSet<Rc<MinlogType>> {
        self.proof.get_type_variables()
            .union(&self.term.get_type_variables())
            .cloned().collect()
    }
    
    fn get_algebra_types(&self) -> HashSet<Rc<MinlogType>> {
        self.proof.get_algebra_types()
            .union(&self.term.get_algebra_types())
            .cloned().collect()
    }
    
    fn get_free_variables(&self) -> HashSet<Rc<MinlogTerm>> {
        self.proof.get_free_variables()
            .union(&self.term.get_free_variables())
            .cloned().collect()
    }
    
    fn get_bound_variables(&self) -> HashSet<Rc<MinlogTerm>> {
        self.proof.get_bound_variables()
            .union(&self.term.get_bound_variables())
            .cloned().collect::<HashSet<_>>()
            .difference(&HashSet::from([self.replaced_variable.clone()]))
            .cloned().collect()
    }
    
    fn get_predicate_variables(&self) -> HashSet<Rc<MinlogPredicate>> {
        self.proof.get_predicate_variables()
    }
    
    fn get_comprehension_terms(&self) -> HashSet<Rc<MinlogPredicate>> {
        self.proof.get_comprehension_terms()
    }
    
    fn get_inductive_predicates(&self) -> HashSet<Rc<MinlogPredicate>> {
        self.proof.get_inductive_predicates()
    }
    
    fn get_prime_formulas(&self) -> HashSet<Rc<MinlogFormula>> {
        self.proof.get_prime_formulas()
    }
    
    fn get_assumptions(&self) -> HashSet<Rc<MinlogProof>> {
        self.proof.get_assumptions()
    }
    
    fn get_axioms(&self) -> HashSet<Rc<MinlogProof>> {
        self.proof.get_axioms()
    }
    
    fn get_theorems(&self) -> HashSet<Rc<MinlogProof>> {
        self.proof.get_theorems()
    }
}

impl PrettyPrintable for UniversalElim {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        PPElement::group(vec![
            PPElement::text("∀⁻ (".to_string()),
            PPElement::break_elem(1, 4, false),
            PPElement::list(
                vec![
                    self.proof.to_pp_element(detail),
                    self.term.to_pp_element(detail),
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

impl ProofTreeDisplayable for UniversalElim {
    fn to_proof_tree_node(&self) -> ProofTreeNode {
        ProofTreeNode::new_node(
            vec![self.proof.to_proof_tree_node(), ProofTreeNode::new_leaf(self.term.display_string())],
            self.formula.display_string(),
            Some("∀⁻".to_string())
        )
    }
}