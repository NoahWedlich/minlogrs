
use indexmap::{IndexMap, IndexSet};
use std::rc::Rc;

use crate::core::terms::application::Application;
use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};
use crate::utils::proof_tree_display::{ProofTreeDisplayable, ProofTreeNode};

use crate::core::substitution::MatchOutput;

use crate::core::types::minlog_type::MinlogType;
use crate::core::terms::minlog_term::MinlogTerm;

use crate::core::predicates::minlog_predicate::MinlogPredicate;
use crate::core::predicates::implication::Implication;

use crate::core::proofs::minlog_proof::{MinlogProof, ProofBody};

use crate::core::proofs::proof_substitution::ProofSubstEntry;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ImplicationElim {
    premise: Rc<MinlogProof>,
    implication: Rc<MinlogProof>,
    formula: Rc<MinlogPredicate>,
}

impl ImplicationElim {
    pub fn create(implication: Rc<MinlogProof>, premise: Rc<MinlogProof>) -> Rc<MinlogProof> {
        let implication_formula = implication.proved_formula();
        if !implication_formula.is_implication() {
            panic!("ImplicationElim::create called with a non-implication proof as implication");
        }
        
        let implication_obj = implication_formula.to_implication().unwrap();
        let premises = implication_obj.premises();
        let conclusion = implication_obj.conclusion();
        
        if premises.is_empty() {
            panic!("ImplicationElim::create called with an implication that has no premises");
        }
        
        if premises[0] != premise.proved_formula() {
            panic!("Can't eliminate implication {} with premise {}", implication_formula.debug_string(), premise.proved_formula().debug_string());
        }
        
        let formula = if premises.len() == 1 {
            conclusion.clone()
        } else {
            Implication::create(premises[1..].to_vec(), conclusion.clone())
        };
        
        Rc::new(MinlogProof::ImplicationElim(ImplicationElim {
            premise,
            implication,
            formula,
        }))
    }
    
    pub fn premise(&self) -> Rc<MinlogProof> {
        self.premise.clone()
    }
    
    pub fn implication(&self) -> Rc<MinlogProof> {
        self.implication.clone()
    }
}

impl ProofBody for ImplicationElim {
    fn proved_formula(&self) -> Rc<MinlogPredicate> {
        self.formula.clone()
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> Rc<MinlogProof> {
        Rc::new(MinlogProof::ImplicationElim(ImplicationElim {
            premise: self.premise.normalize(eta, pi),
            implication: self.implication.normalize(eta, pi),
            formula: self.formula.normalize(eta, pi),
        }))
    }
    
    fn unfold(&self) -> Rc<MinlogProof> {
        Rc::new(MinlogProof::ImplicationElim(ImplicationElim {
            premise: self.premise.unfold(),
            implication: self.implication.unfold(),
            formula: self.formula.clone(),
        }))
    }
    
    fn extracted_term(&self) -> Option<Rc<MinlogTerm>> {
        self.implication.extracted_term().map(|imp_term| {
            if let Some(prem_term) = self.premise.extracted_term() {
                Application::create(
                    imp_term,
                    vec![prem_term],
                )
            } else {
                imp_term
            }
        })?.remove_nulls()
    }
    
    fn get_type_variables(&self) -> IndexSet<Rc<MinlogType>> {
        self.premise.get_type_variables()
            .union(&self.implication.get_type_variables())
            .cloned().collect()
    }
    
    fn get_algebra_types(&self) -> IndexSet<Rc<MinlogType>> {
        self.premise.get_algebra_types()
            .union(&self.implication.get_algebra_types())
            .cloned().collect()
    }
    
    fn get_free_variables(&self) -> IndexSet<Rc<MinlogTerm>> {
        self.premise.get_free_variables()
            .union(&self.implication.get_free_variables())
            .cloned().collect()
    }
    
    fn get_bound_variables(&self) -> IndexSet<Rc<MinlogTerm>> {
        self.premise.get_bound_variables()
            .union(&self.implication.get_bound_variables())
            .cloned().collect()
    }
    
    fn get_predicate_variables(&self) -> IndexSet<Rc<MinlogPredicate>> {
        self.premise.get_predicate_variables()
            .union(&self.implication.get_predicate_variables())
            .cloned().collect()
    }
    
    fn get_comprehension_terms(&self) -> IndexSet<Rc<MinlogPredicate>> {
        self.premise.get_comprehension_terms()
            .union(&self.implication.get_comprehension_terms())
            .cloned().collect()
    }
    
    fn get_inductive_predicates(&self) -> IndexSet<Rc<MinlogPredicate>> {
        self.premise.get_inductive_predicates()
            .union(&self.implication.get_inductive_predicates())
            .cloned().collect()
    }
    
    fn get_prime_formulas(&self) -> IndexSet<Rc<MinlogPredicate>> {
        self.premise.get_prime_formulas()
            .union(&self.implication.get_prime_formulas())
            .cloned().collect()
    }

    fn get_goals(&self) -> IndexSet<Rc<MinlogProof>> {
        self.premise.get_goals()
            .union(&self.implication.get_goals())
            .cloned().collect()
    }
    

    fn get_assumptions(&self) -> IndexSet<Rc<MinlogProof>> {
        self.premise.get_assumptions()
            .union(&self.implication.get_assumptions())
            .cloned().collect()
    }
    
    fn get_axioms(&self) -> IndexSet<Rc<MinlogProof>> {
        self.premise.get_axioms()
            .union(&self.implication.get_axioms())
            .cloned().collect()
    }
    
    fn get_theorems(&self) -> IndexSet<Rc<MinlogProof>> {
        self.premise.get_theorems()
            .union(&self.implication.get_theorems())
            .cloned().collect()
    }
    
    fn substitute(&self, from: &ProofSubstEntry, to: &ProofSubstEntry) -> Rc<MinlogProof> {
        if let ProofSubstEntry::Proof(from_proof) = from && from_proof.is_implication_elim() && self == from_proof.to_implication_elim().unwrap() {
            to.to_proof().unwrap()
        } else {
            ImplicationElim::create(
                self.implication.substitute(from, to),
                self.premise.substitute(from, to),
            )
        }
    }
    
    fn first_conflict_with(&self, other: &Rc<MinlogProof>) -> Option<(ProofSubstEntry, ProofSubstEntry)> {
        if let Some(imp_elim) = other.to_implication_elim() {
            if let Some(conflict) = self.implication.first_conflict_with(&imp_elim.implication) {
                return Some(conflict);
            }
            
            if let Some(conflict) = self.premise.first_conflict_with(&imp_elim.premise) {
                return Some(conflict);
            }
            
            None
        } else {
            Some((Rc::new(MinlogProof::ImplicationElim(self.clone())).into(), other.clone().into()))
        }
    }
    
    fn match_with(&self, instance: &Rc<MinlogProof>) -> MatchOutput<ProofSubstEntry> {
        if !instance.is_implication_elim() {
            return MatchOutput::FailedMatch;
        }
        
        let imp_elim_instance = instance.to_implication_elim().unwrap();
        
        let mut conditions = IndexMap::new();
        
        if self.implication != imp_elim_instance.implication {
            conditions.insert(self.implication.clone().into(), imp_elim_instance.implication.clone().into());
        }
        
        if self.premise != imp_elim_instance.premise {
            conditions.insert(self.premise.clone().into(), imp_elim_instance.premise.clone().into());
        }
        
        MatchOutput::Matched(conditions)
    }
}

impl PrettyPrintable for ImplicationElim {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        PPElement::group(vec![
            PPElement::text("→⁻ (".to_string()),
            PPElement::break_elem(1, 4, false),
            PPElement::list(
                vec![
                    self.implication.to_pp_element(detail),
                    self.premise.to_pp_element(detail),
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

impl ProofTreeDisplayable for ImplicationElim {
    fn to_proof_tree_node(&self) -> ProofTreeNode {
        ProofTreeNode::new_node(
            vec![self.implication.to_proof_tree_node(), self.premise.to_proof_tree_node()],
            self.proved_formula().display_string(),
            Some("→⁻".to_string())
        )
    }
}