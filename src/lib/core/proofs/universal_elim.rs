
use crate::includes::{
    essential::*,
    utils::*,
    core::{
        types::*,
        terms::*,
        predicates::*,
        proofs::*,
    }
};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct UniversalElim {
    proof: Arc<MinlogProof>,
    term: MinlogTerm,
    replaced_variable: MinlogTerm,
    formula: Arc<MinlogPredicate>,
}

impl UniversalElim {
    pub fn create(proof: Arc<MinlogProof>, term: MinlogTerm) -> Arc<MinlogProof> {
        let universal_formula = proof.proved_formula();
        
        if !universal_formula.is_all_quantifier() {
            panic!("UniversalElim::create called with a proof that does not prove a universal formula");
        }
        
        let all_quantifier = universal_formula.to_all_quantifier().unwrap();
        let var = all_quantifier.var();
        
        let formula = all_quantifier.body().substitute(&var.clone().into(), &term.clone().into());
        
        Arc::new(MinlogProof::UniversalElim(UniversalElim {
            proof,
            term,
            replaced_variable: var.clone(),
            formula,
        }))
    }
    
    pub fn proof(&self) -> Arc<MinlogProof> {
        self.proof.clone()
    }
    
    pub fn term(&self) -> MinlogTerm {
        self.term.clone()
    }
}

impl ProofBody for UniversalElim {
    fn proved_formula(&self) -> Arc<MinlogPredicate> {
        self.formula.clone()
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> Arc<MinlogProof> {
        Arc::new(MinlogProof::UniversalElim(UniversalElim {
            proof: self.proof.normalize(eta, pi),
            term: self.term.clone(),
            replaced_variable: self.replaced_variable.clone(),
            formula: self.formula.normalize(eta, pi),
        }))
    }
    
    fn unfold(&self) -> Arc<MinlogProof> {
        Arc::new(MinlogProof::UniversalElim(UniversalElim {
            proof: self.proof.unfold(),
            term: self.term.clone(),
            replaced_variable: self.replaced_variable.clone(),
            formula: self.formula.clone(),
        }))
    }
    
    fn extracted_term(&self) -> Option<MinlogTerm> {
        self.proof.extracted_term()
    }
    
    fn get_type_variables(&self) -> IndexSet<Arc<MinlogType>> {
            self.proof.get_type_variables()
            .union(&self.term.get_type_variables(&mut IndexSet::new()))
            .cloned().collect()
    }
    
    fn get_algebra_types(&self) -> IndexSet<Arc<MinlogType>> {
            self.proof.get_algebra_types()
            .union(&self.term.get_algebra_types(&mut IndexSet::new()))
            .cloned().collect()
    }
    
    fn get_free_variables(&self) -> IndexSet<MinlogTerm> {
            self.proof.get_free_variables()
            .union(&self.term.get_free_variables(&mut IndexSet::new()))
            .cloned().collect()
    }
    
    fn get_bound_variables(&self) -> IndexSet<MinlogTerm> {
        self.proof.get_bound_variables()
            .union(&self.term.get_bound_variables(&mut IndexSet::new()))
            .cloned().collect::<IndexSet<_>>()
            .difference(&IndexSet::from([self.replaced_variable.clone()]))
            .cloned().collect()
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
        if let ProofSubstEntry::Proof(from_proof) = from && from_proof.is_universal_elim() && self == from_proof.to_universal_elim().unwrap() {
            to.to_proof().unwrap()
        } else if let Some(term) = from.to_term() && (self.replaced_variable == term || self.term == term) {
            UniversalElim::create(
                self.proof.clone(),
                self.term.substitute_with(from, to),
            )
        } else {
            UniversalElim::create(
                self.proof.substitute(from, to),
                self.term.substitute_with(from, to),
            )
        }
    }
    
    fn first_conflict_with(&self, other: &Arc<MinlogProof>) -> Option<(ProofSubstEntry, ProofSubstEntry)> {
        if let MinlogProof::UniversalElim(other_universal_elim) = other.as_ref() {
            if let Some(conflict) = self.proof.first_conflict_with(&other_universal_elim.proof) {
                return Some(conflict);
            }
            
            if let Some(conflict) = self.term.first_conflict_with(&other_universal_elim.term) {
                return Some((conflict.0.into(), conflict.1.into()));
            }
            
            None
        } else {
            Some((Arc::new(MinlogProof::UniversalElim(self.clone())).into(), other.clone().into()))
        }
    }
    
    fn match_with(&self, instance: &Arc<MinlogProof>) -> MatchOutput<ProofSubstEntry> {
        if !instance.is_universal_elim() {
            return MatchOutput::FailedMatch;
        }
        
        let ue_instance = instance.to_universal_elim().unwrap();
        
        let mut conditions = IndexMap::new();
        
        if self.term != ue_instance.term {
            conditions.insert(self.term.clone().into(), ue_instance.term.clone().into());
        }
        
        if self.proof != ue_instance.proof {
            conditions.insert(self.proof.clone().into(), ue_instance.proof.clone().into());
        }
        
        MatchOutput::Matched(conditions)
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