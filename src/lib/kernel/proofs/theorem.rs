
use crate::includes::{
    essential::*,
    utils::*,
    kernel::{
        types::*,
        terms::*,
        predicates::*,
        proofs::*,
    }
};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Theorem {
    name: String,
    formula: Arc<MinlogPredicate>,
    proof: Arc<MinlogProof>,
}

impl Theorem {
    pub fn create(name: String, proof: Arc<MinlogProof>) -> Arc<MinlogProof> {
        let formula = proof.proved_formula();
        Arc::new(MinlogProof::Theorem(Theorem { name, formula, proof }))
    }
    
    pub fn name(&self) -> &str {
        &self.name
    }
    
    pub fn proof(&self) -> Arc<MinlogProof> {
        self.proof.clone()
    }
    
    pub fn formula(&self) -> Arc<MinlogPredicate> {
        self.formula.clone()
    }
}

impl ProofBody for Theorem {
    fn proved_formula(&self) -> Arc<MinlogPredicate> {
        self.formula.clone()
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> Arc<MinlogProof> {
        Arc::new(MinlogProof::Theorem(Theorem {
            name: self.name.clone(),
            formula: self.formula.normalize(eta, pi),
            proof: self.proof.clone(),
        }))
    }
    
    fn unfold(&self) -> Arc<MinlogProof> {
        Arc::new(MinlogProof::Theorem(Theorem {
            name: self.name.clone(),
            formula: self.formula.clone(),
            proof: self.proof.unfold(),
        }))
    }
    
    fn extracted_term(&self) -> Option<MinlogTerm> {
        self.proof.extracted_term()
    }
    
    fn get_type_variables(&self) -> IndexSet<Arc<MinlogType>> {
        self.formula.get_type_variables(&mut IndexSet::new())
    }
    
    fn get_algebra_types(&self) -> IndexSet<Arc<MinlogType>> {
        self.formula.get_algebra_types(&mut IndexSet::new())
    }
    
    fn get_free_variables(&self) -> IndexSet<MinlogTerm> {
        self.formula.get_free_variables(&mut IndexSet::new())
    }
    
    fn get_bound_variables(&self) -> IndexSet<MinlogTerm> {
        self.formula.get_bound_variables(&mut IndexSet::new())
    }
    
    fn get_predicate_variables(&self) -> IndexSet<Arc<MinlogPredicate>> {
        self.formula.get_predicate_variables(&mut IndexSet::new())
    }
    
    fn get_comprehension_terms(&self) -> IndexSet<Arc<MinlogPredicate>> {
        self.formula.get_comprehension_terms(&mut IndexSet::new())
    }
    
    fn get_inductive_predicates(&self) -> IndexSet<Arc<MinlogPredicate>> {
        self.formula.get_inductive_predicates(&mut IndexSet::new())
    }
    
    fn get_prime_formulas(&self) -> IndexSet<Arc<MinlogPredicate>> {
        self.formula.get_prime_formulas(&mut IndexSet::new())
    }
    
    fn get_theorems(&self) -> IndexSet<Arc<MinlogProof>> {
        IndexSet::from([Arc::new(MinlogProof::Theorem(self.clone()))])
    }
    
    fn substitute(&self, from: &ProofSubstEntry, to: &ProofSubstEntry) -> Arc<MinlogProof> {
        if let ProofSubstEntry::Proof(from_proof) = from && from_proof.is_theorem() && self == from_proof.to_theorem().unwrap() {
            to.to_proof().unwrap()
        } else {
            Arc::new(MinlogProof::Theorem(Theorem {
                name: self.name.clone(),
                formula: self.formula.substitute_with(from, to),
                proof: self.proof.substitute(from, to),
            }))
        }
    }
    
    fn first_conflict_with(&self, other: &Arc<MinlogProof>) -> Option<(ProofSubstEntry, ProofSubstEntry)> {
        if let Some(conflict) = self.formula.first_conflict_with(&other.proved_formula()) {
            return Some((conflict.0.into(), conflict.1.into()));
        }
        
        if other.is_theorem() && self == other.to_theorem().unwrap() {
            None
        } else {
            Some((Arc::new(MinlogProof::Theorem(self.clone())).into(), other.clone().into()))
        }
    }
    
    fn match_with(&self, instance: &Arc<MinlogProof>) -> MatchOutput<ProofSubstEntry> {
        if !instance.is_theorem() {
            return MatchOutput::FailedMatch;
        }
        
        let thm_instance = instance.to_theorem().unwrap();
        
        if self.name() != thm_instance.name() {
            MatchOutput::FailedMatch
        } else {
            MatchOutput::Matched(IndexMap::new())
        }
    }
}

impl PrettyPrintable for Theorem {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        PPElement::group(vec![
            PPElement::text("[".to_string()),
            PPElement::break_elem(1, 4, false),
            PPElement::group(vec![
                PPElement::text(self.name.clone()),
                PPElement::break_elem(0, 0, false),
                PPElement::text(":".to_string()),
                PPElement::break_elem(1, 4, false),
                self.formula.to_pp_element(detail),
            ], BreakType::Flexible, 0),
            PPElement::break_elem(1, 0, false),
            PPElement::text("]".to_string()),
        ], BreakType::Consistent, 0)
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}

impl ProofTreeDisplayable for Theorem {
    fn to_proof_tree_node(&self) -> ProofTreeNode {
        ProofTreeNode::new_node(vec![
            ProofTreeNode::new_leaf(self.name.clone())
        ], self.formula.display_string(), Some("Theorem".to_string()))
    }
}