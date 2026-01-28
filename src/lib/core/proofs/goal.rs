
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

#[derive(Clone)]
pub struct Goal {
    name: String,
    formula: Arc<MinlogPredicate>,
    context: Arc<RwLock<ProofContext>>,
}

impl Goal {
    pub fn create(name: String, formula: Arc<MinlogPredicate>, context: ProofContext) -> Arc<MinlogProof> {
        if !formula.is_formula() {
            panic!("Can only create goals of nullary predicates")
        }
        
        for assumption in context.get_assumptions() {
            if assumption.proved_formula() == formula {
                return assumption.clone();
            }
        }
        
        Arc::new(MinlogProof::Goal(Goal { name, formula, context: Arc::new(RwLock::new(context)) }))
    }
    
    pub fn name(&self) -> &str {
        &self.name
    }
    
    pub fn get_context(&self) -> std::sync::RwLockReadGuard<'_, ProofContext> {
        self.context.read().unwrap()
    }
    
    pub fn get_context_mut(&self) -> std::sync::RwLockWriteGuard<'_, ProofContext> {
        self.context.write().unwrap()
    }
}

impl ProofBody for Goal {
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
        
        for assumption in &new_context.assumptions {
            if assumption.proved_formula() == new_formula {
                return assumption.clone();
            }
        }
        
        Arc::new(MinlogProof::Goal(Goal {
            name: self.name.clone(),
            formula: new_formula,
            context: Arc::new(RwLock::new(new_context)),
        }))
    }
    
    fn unfold(&self) -> Arc<MinlogProof> {
        Arc::new(MinlogProof::Goal(self.clone()))
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
    
    fn get_goals(&self) -> IndexSet<Arc<MinlogProof>> {
        IndexSet::from([Arc::new(MinlogProof::Goal(self.clone()))])
    }
    
    fn get_assumptions(&self) -> IndexSet<Arc<MinlogProof>> {
        self.context.read().unwrap().assumptions.clone()
    }
    
    fn substitute(&self, from: &ProofSubstEntry, to: &ProofSubstEntry) -> Arc<MinlogProof> {
        if let ProofSubstEntry::Proof(from_proof) = from && from_proof.is_goal() && self == from_proof.to_goal().unwrap() {
            to.to_proof().unwrap()
        } else {
            let new_context = ProofContext {
                    assumptions: self.context.read().unwrap().assumptions.iter()
                        .map(|a| a.substitute(from, to))
                        .collect(),
                    variables: self.context.read().unwrap().variables.iter()
                        .map(|v| v.substitute_with(from, to))
                        .collect()
            };
            
            let new_formula = self.formula.substitute_with(from, to);
            
            for assumption in &new_context.assumptions {
                if assumption.proved_formula() == new_formula {
                    return assumption.clone();
                }
            }
            
            Arc::new(MinlogProof::Goal(Goal {
                name: self.name.clone(),
                formula: new_formula,
                context: Arc::new(RwLock::new(new_context)),
            }))
        }
    }
    
    fn first_conflict_with(&self, other: &Arc<MinlogProof>) -> Option<(ProofSubstEntry, ProofSubstEntry)> {
        if let Some(conflict) = self.formula.first_conflict_with(&other.proved_formula()) {
            return Some((conflict.0.into(), conflict.1.into()));
        }
        
        if other.is_goal() && self == other.to_goal().unwrap() {
            None
        } else {
            Some((Arc::new(MinlogProof::Goal(self.clone())).into(), other.clone().into()))
        }
    }
    
    fn match_with(&self, instance: &Arc<MinlogProof>) -> MatchOutput<ProofSubstEntry> {
        if self.proved_formula() != instance.proved_formula() {
            MatchOutput::Matched(IndexMap::from([
                (Arc::new(MinlogProof::Goal(self.clone())).into(), instance.clone().into()),
                (self.proved_formula().into(), instance.proved_formula().into()),
            ]))
        } else {
            MatchOutput::Substitution(Arc::new(MinlogProof::Goal(self.clone())).into(), instance.clone().into())
        }
    }
}

impl PrettyPrintable for Goal {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        PPElement::group(vec![
            PPElement::text("{".to_string()),
            PPElement::break_elem(1, 4, false),
            PPElement::group(vec![
                PPElement::text(self.name.clone()),
                PPElement::break_elem(0, 0, false),
                PPElement::text(":".to_string()),
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

impl ProofTreeDisplayable for Goal {
    fn to_proof_tree_node(&self) -> ProofTreeNode {
        let v_ell_left_offset = self.name.chars().count() / 2;
        let v_ell_right_offset = self.name.chars().count() - v_ell_left_offset - 1;
        let v_ell = format!("{}⋮{}", " ".repeat(v_ell_left_offset), " ".repeat(v_ell_right_offset));
        
        ProofTreeNode::new_node(vec![
            ProofTreeNode::new_node(
                vec![ProofTreeNode::new_leaf(self.context.read().unwrap().display_string())],
                format!("{}\n{}\n{}", v_ell.clone(), self.name, v_ell),
                None
            )
        ], self.formula.display_string(), None)
    }
}

impl Hash for Goal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.formula.hash(state);
    }
}

impl PartialEq for Goal {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.formula == other.formula
    }
}

impl Eq for Goal {}