
use crate::includes::{
    essential::*,
    utils::*,
    kernel::{
        structures::*,
        types::*,
        terms::*,
        predicates::*,
        proofs::*,
    }
};

#[derive(Clone)]
pub struct Axiom {
    name: String,
    formula: Arc<MinlogPredicate>,
    content: Arc<RwLock<Option<MinlogTerm>>>,
}

impl Axiom {
    pub fn create(name: String, formula: Arc<MinlogPredicate>) -> Arc<MinlogProof> {
        if !formula.is_formula() {
            panic!("Can only create axioms of nullary predicates")
        }
        
        Arc::new(MinlogProof::Axiom(Axiom { name, formula, content: Arc::new(RwLock::new(None)) }))
    }
    
    pub fn name(&self) -> &str {
        &self.name
    }
    
    pub fn has_content(&self) -> bool {
        self.content.read().unwrap().is_some()
    }
    
    pub fn content(&self) -> Option<MinlogTerm> {
        self.content.read().unwrap().clone()
    }
    
    pub fn set_content(&self, content: MinlogTerm) {
        if content.minlog_type() != self.formula.extracted_type_pattern() {
            panic!("Tried to set axiom content with mismatching type, expected {}, got {}",
                self.formula.extracted_type_pattern().debug_string(),
                content.minlog_type().debug_string()
            );
        }
        
        *self.content.write().unwrap() = Some(content);
    }
}

impl ProofBody for Axiom {
    fn proved_formula(&self) -> Arc<MinlogPredicate> {
        self.formula.clone()
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> Arc<MinlogProof> {
        Arc::new(MinlogProof::Axiom(Axiom {
            name: self.name.clone(),
            formula: self.formula.normalize(eta, pi),
            content: Arc::new(RwLock::new(self.content.read().unwrap().clone())),
        }))
    }
    
    fn unfold(&self) -> Arc<MinlogProof> {
        Arc::new(MinlogProof::Axiom(self.clone()))
    }
    
    fn extracted_term(&self) -> Option<MinlogTerm> {
        self.formula.extracted_type().remove_nulls().map(|t| {
            if let Some(content) = self.content.read().unwrap().clone() {
                let et_subst = self.formula.et_pattern_to_et();
                et_subst.substitute(&content)
            } else {
                let pconst = ProgramConstant::create(self.name.clone(), t);
                ProgramTerm::create(pconst, TermSubstitution::make_empty())
            }
        })?.remove_nulls()
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
    
    fn get_axioms(&self) -> IndexSet<Arc<MinlogProof>> {
        IndexSet::from([Arc::new(MinlogProof::Axiom(self.clone()))])
    }
    
    fn substitute(&self, from: &ProofSubstEntry, to: &ProofSubstEntry) -> Arc<MinlogProof> {
        if let ProofSubstEntry::Proof(from_proof) = from && from_proof.is_axiom() && self == from_proof.to_axiom().unwrap() {
            to.to_proof().unwrap()
        } else {
            Arc::new(MinlogProof::Axiom(Axiom {
                name: self.name.clone(),
                formula: self.formula.substitute_with(from, to),
                content: Arc::new(RwLock::new(self.content.read().unwrap().as_ref().map(|c| c.substitute_with(from, to)).clone())),
            }))
        }
    }
    
    fn first_conflict_with(&self, other: &Arc<MinlogProof>) -> Option<(ProofSubstEntry, ProofSubstEntry)> {
        if let Some(conflict) = self.formula.first_conflict_with(&other.proved_formula()) {
            return Some((conflict.0.into(), conflict.1.into()));
        }
        
        if other.is_axiom() && self == other.to_axiom().unwrap() {
            None
        } else {
            Some((Arc::new(MinlogProof::Axiom(self.clone())).into(), other.clone().into()))
        }
    }
    
    fn match_with(&self, instance: &Arc<MinlogProof>) -> MatchOutput<ProofSubstEntry> {
        if !instance.is_axiom() {
            return MatchOutput::FailedMatch;
        }
        
        let axiom_instance = instance.to_axiom().unwrap();
        
        if self.name() != axiom_instance.name() {
            return MatchOutput::FailedMatch;
        }
        
        let conditions = if self.formula != axiom_instance.formula {
            IndexMap::from([(self.formula.clone().into(), axiom_instance.formula.clone().into())])
        } else {
            IndexMap::new()
        };
        
        MatchOutput::Matched(conditions)
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

impl Hash for Axiom {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.formula.hash(state);
    }
}

impl PartialEq for Axiom {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.formula == other.formula
    }
}

impl Eq for Axiom {}