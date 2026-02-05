
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

#[derive(Clone, PartialEq, Eq)]
pub struct BundledProof {
    proof: Arc<MinlogProof>,
    dependencies: IndexSet<Arc<MinlogProof>>,
    name: String,
}

impl BundledProof {
    pub fn create(proof: Arc<MinlogProof>, name: String, mut extra_dependencies: IndexSet<Arc<MinlogProof>>) -> Arc<MinlogProof> {
        extra_dependencies.extend(proof.get_goals());
        Arc::new(MinlogProof::BundledProof(BundledProof { proof, name, dependencies: extra_dependencies }))
    }
    
    pub fn proof(&self) -> &Arc<MinlogProof> {
        &self.proof
    }
    
    pub fn name(&self) -> &str {
        &self.name
    }
    
    pub fn dependencies(&self) -> &IndexSet<Arc<MinlogProof>> {
        &self.dependencies
    }
}

impl ProofBody for BundledProof {
    fn proved_formula(&self) -> Arc<MinlogPredicate> {
        self.proof.proved_formula()
    }
    
    fn length(&self) -> usize {
        1
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> Arc<MinlogProof> {
        BundledProof::create(
            self.proof.normalize(eta, pi),
            self.name.clone(),
            self.dependencies.iter().map(|d| d.normalize(eta, pi)).collect(),
        )
    }
    
    fn unfold(&self) -> Arc<MinlogProof> {
        self.proof.unfold()
    }
    
    fn extracted_term(&self) -> Option<MinlogTerm> {
        self.proof.extracted_term()
    }
    
    fn get_type_variables(&self) -> IndexSet<Arc<MinlogType>> {
        self.proof.get_type_variables()
    }
    
    fn get_algebra_types(&self) -> IndexSet<Arc<MinlogType>> {
        self.proof.get_algebra_types()
    }
    
    fn get_free_variables(&self) -> IndexSet<MinlogTerm> {
        self.proof.get_free_variables()
    }
    
    fn get_bound_variables(&self) -> IndexSet<MinlogTerm> {
        self.proof.get_bound_variables()
    }
    
    fn get_predicate_variables(&self) -> IndexSet<Arc<MinlogPredicate>> {
        self.proof.get_predicate_variables()
    }
    
    fn get_comprehension_terms(&self) -> IndexSet<Arc<MinlogPredicate>> {
        self.proof.get_comprehension_terms()
    }
    
    fn get_inductive_predicates(&self) -> IndexSet<Arc<MinlogPredicate>> {
        self.proof.get_inductive_predicates()
    }
    
    fn get_prime_formulas(&self) -> IndexSet<Arc<MinlogPredicate>> {
        self.proof.get_prime_formulas()
    }
    
    fn get_goals(&self) -> IndexSet<Arc<MinlogProof>> {
        self.proof.get_goals()
    }
    
    fn get_assumptions(&self) -> IndexSet<Arc<MinlogProof>> {
        self.proof.get_assumptions()
    }
    
    fn get_axioms(&self) -> IndexSet<Arc<MinlogProof>> {
        self.proof.get_axioms()
    }
    
    fn get_theorems(&self) -> IndexSet<Arc<MinlogProof>> {
        self.proof.get_theorems()
    }
    
    fn substitute(&self, from: &ProofSubstEntry, to: &ProofSubstEntry) -> Arc<MinlogProof> {
        if let ProofSubstEntry::Proof(from_proof) = from && from_proof.is_bundled_proof() && self == from_proof.to_bundled_proof().unwrap() {
            to.to_proof().unwrap()
        } else {
            BundledProof::create(
                self.proof.substitute(from, to),
                self.name.clone(),
                self.dependencies.iter().map(|d| d.substitute(from, to)).collect(),
            )
        }
    }

    fn first_conflict_with(&self, other: &Arc<MinlogProof>) -> Option<(ProofSubstEntry, ProofSubstEntry)> {
        if let Some(conflict) = self.proved_formula().first_conflict_with(&other.proved_formula()) {
            return Some((conflict.0.into(), conflict.1.into()));
        }
        
        if let Some(bundled) = other.to_bundled_proof() && self.name == bundled.name() {
            bundled.proof.first_conflict_with(&self.proof)
        } else {
            self.proof.first_conflict_with(other)
        }
    }
    
    fn match_with(&self, instance: &Arc<MinlogProof>) -> MatchOutput<ProofSubstEntry> {
        if !instance.is_bundled_proof() {
            return MatchOutput::FailedMatch;
        }
        
        let bundled_instance = instance.to_bundled_proof().unwrap();
        
        if self.name() != bundled_instance.name() {
            return MatchOutput::FailedMatch;
        }
        
        let mut conditions = IndexMap::new();
        
        if self.proved_formula() != bundled_instance.proved_formula() {
            conditions.insert(self.proved_formula().into(), bundled_instance.proved_formula().into());
        }
        
        if self.proof() != bundled_instance.proof() {
            conditions.insert(self.proof().clone().into(), bundled_instance.proof().clone().into());
        }
        
        MatchOutput::Matched(conditions)
    }
}

impl PrettyPrintable for BundledProof {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        PPElement::group(vec![
            PPElement::text(format!("{} (", self.name)),
            PPElement::break_elem(1, 4, false),
            PPElement::list(
                self.dependencies.iter().map(|d| d.to_pp_element(detail)).collect(),
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

impl ProofTreeDisplayable for BundledProof {
    fn to_proof_tree_node(&self) -> ProofTreeNode {
        let mut goals = self.proof.get_goals().iter().cloned().collect::<Vec<_>>();
        goals.sort_by_key(|a| a.to_goal().map_or("".to_string(), |g| g.name().to_string()));
        
        ProofTreeNode::new_node(
            self.dependencies.iter().map(|d| d.to_proof_tree_node()).collect(),
            self.proved_formula().display_string(),
            Some(self.name.clone()),
        )
    }
}

impl Hash for BundledProof {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.proof.hash(state);
        self.name.hash(state);
    }
}