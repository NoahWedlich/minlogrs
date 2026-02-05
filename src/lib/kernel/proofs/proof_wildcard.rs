
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

#[derive(Clone)]
pub struct ProofWildcard {
    formula: Arc<MinlogPredicate>,
    context: Arc<RwLock<ProofContext>>,
}

impl ProofWildcard {
    pub fn create(formula: Arc<MinlogPredicate>, context: ProofContext) -> Arc<MinlogProof> {
        if !formula.is_formula() {
            panic!("Can only create wildcards of nullary predicates")
        }
        
        Arc::new(MinlogProof::Wildcard(ProofWildcard { formula, context: Arc::new(RwLock::new(context)) }))
    }
    
    pub fn get_context(&self) -> std::sync::RwLockReadGuard<'_, ProofContext> {
        self.context.read().unwrap()
    }
    
    pub fn get_context_mut(&self) -> std::sync::RwLockWriteGuard<'_, ProofContext> {
        self.context.write().unwrap()
    }
}

impl ProofBody for ProofWildcard {
    fn proved_formula(&self) -> Arc<MinlogPredicate> {
        self.formula.clone()
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> Arc<MinlogProof> {
        let new_context = ProofContext {
            assumptions: self.context.read().unwrap().assumptions.iter()
                .map(|a| a.normalize(eta, pi))
                .collect(),
            variables: self.context.read().unwrap().variables.clone(),
        };
        
        let new_formula = self.formula.normalize(eta, pi);
        
        Arc::new(MinlogProof::Wildcard(ProofWildcard {
            formula: new_formula,
            context: Arc::new(RwLock::new(new_context)),
        }))
    }
    
    fn unfold(&self) -> Arc<MinlogProof> {
        Arc::new(MinlogProof::Wildcard(self.clone()))
    }
    
    fn extracted_term(&self) -> Option<MinlogTerm> {
        None
    }
    
    fn get_type_variables(&self) -> IndexSet<Arc<MinlogType>> {
        self.formula.get_type_variables(&mut IndexSet::new())
    }
    
    fn get_algebra_types(&self) -> IndexSet<Arc<MinlogType>> {
        self.formula.get_algebra_types(&mut IndexSet::new())
    }
    
    fn get_free_variables(&self) -> IndexSet<MinlogTerm> {
        self.formula.get_free_variables(&mut IndexSet::new())
            .union(&self.context.read().unwrap().variables).cloned().collect()
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
    
    fn get_assumptions(&self) -> IndexSet<Arc<MinlogProof>> {
        self.context.read().unwrap().assumptions.clone()
    }
    
    fn substitute(&self, from: &ProofSubstEntry, to: &ProofSubstEntry) -> Arc<MinlogProof> {
        let new_context = ProofContext {
            assumptions: self.context.read().unwrap().assumptions.iter()
                .map(|a| a.substitute(from, to))
                .collect(),
            variables: self.context.read().unwrap().variables.iter()
                .map(|v| v.substitute_with(from, to))
                .collect()
        };
        
        let new_formula = self.formula.substitute_with(from, to);
        
        Arc::new(MinlogProof::Wildcard(ProofWildcard {
            formula: new_formula,
            context: Arc::new(RwLock::new(new_context)),
        }))
    }
    
    fn first_conflict_with(&self, other: &Arc<MinlogProof>) -> Option<(ProofSubstEntry, ProofSubstEntry)> {
        if let Some(conflict) = self.formula.first_conflict_with(&other.proved_formula()) {
            return Some((conflict.0.into(), conflict.1.into()));
        }
        
        
        // TODO: Check context conflicts
        None
    }
    
    fn match_with(&self, instance: &Arc<MinlogProof>) -> MatchOutput<ProofSubstEntry> {
        let conditions = if self.proved_formula() != instance.proved_formula() {
            IndexMap::from([(self.proved_formula().into(), instance.proved_formula().into())])
        } else {
            IndexMap::new()
        };
        
        // TODO: Match contexts
        MatchOutput::Matched(conditions)
    }
}

impl PrettyPrintable for ProofWildcard {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        PPElement::group(vec![
            PPElement::text("{".to_string()),
            PPElement::break_elem(1, 4, false),
            PPElement::group(vec![
                PPElement::text("_:".to_string()),
                PPElement::break_elem(1, 4, false),
                self.formula.to_pp_element(detail),
            ], BreakType::Flexible, 0),
            PPElement::break_elem(1, 0, false),
            PPElement::text("} (".to_string()),
            PPElement::break_elem(1, 4, false),
            self.context.read().unwrap().to_pp_element(detail),
            PPElement::break_elem(1, 0, false),
            PPElement::text(")".to_string()),
        ], BreakType::Consistent, 0)
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}

impl ProofTreeDisplayable for ProofWildcard {
    fn to_proof_tree_node(&self) -> ProofTreeNode {
        ProofTreeNode::new_node(vec![
            ProofTreeNode::new_node(
                vec![ProofTreeNode::new_leaf(self.context.read().unwrap().display_string())],
                "⋮\n_\n⋮".to_string(),
                None
            )
        ], self.formula.display_string(), None)
    }
}

impl Hash for ProofWildcard {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.formula.hash(state);
    }
}

impl PartialEq for ProofWildcard {
    fn eq(&self, other: &Self) -> bool {
        self.formula == other.formula
    }
}

impl Eq for ProofWildcard {}