
use indexmap::IndexSet;
use std::{rc::Rc, cell::RefCell, hash::{Hash, Hasher}};

use crate::utils::pretty_printer::{BreakType, PPElement, PrettyPrintable};
use crate::utils::proof_tree_display::{ProofTreeDisplayable, ProofTreeNode};

use crate::core::substitution::{MatchContext, MatchOutput, SubstitutableWith};

use crate::core::types::minlog_type::MinlogType;
use crate::core::terms::minlog_term::MinlogTerm;
use crate::core::predicates::minlog_predicate::MinlogPredicate;

use crate::core::proofs::minlog_proof::{MinlogProof, ProofBody};
use crate::core::proofs::proof_context::ProofContext;

use crate::core::proofs::proof_substitution::ProofSubstEntry;

#[derive(Clone, PartialEq, Eq)]
pub struct ProofWildcard {
    formula: Rc<MinlogPredicate>,
    context: RefCell<ProofContext>,
}

impl ProofWildcard {
    pub fn create(formula: Rc<MinlogPredicate>, context: ProofContext) -> Rc<MinlogProof> {
        if !formula.is_formula() {
            panic!("Can only create wildcards of nullary predicates")
        }
        
        Rc::new(MinlogProof::Wildcard(ProofWildcard { formula, context: RefCell::new(context) }))
    }
    
    pub fn get_context(&self) -> std::cell::Ref<'_, ProofContext> {
        self.context.borrow()
    }
    
    pub fn get_context_mut(&self) -> std::cell::RefMut<'_, ProofContext> {
        self.context.borrow_mut()
    }
}

impl ProofBody for ProofWildcard {
    fn proved_formula(&self) -> Rc<MinlogPredicate> {
        self.formula.clone()
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> Rc<MinlogProof> {
        let new_context = ProofContext {
            assumptions: self.context.borrow().assumptions.iter()
                .map(|a| a.normalize(eta, pi))
                .collect(),
            variables: self.context.borrow().variables.clone(),
        };
        
        let new_formula = self.formula.normalize(eta, pi);
        
        Rc::new(MinlogProof::Wildcard(ProofWildcard {
            formula: new_formula,
            context: RefCell::new(new_context),
        }))
    }
    
    fn unfold(&self) -> Rc<MinlogProof> {
        Rc::new(MinlogProof::Wildcard(self.clone()))
    }
    
    fn get_type_variables(&self, _visited: &mut IndexSet<MinlogProof>) -> IndexSet<Rc<MinlogType>> {
        self.formula.get_type_variables(&mut IndexSet::new())
    }
    
    fn get_algebra_types(&self, _visited: &mut IndexSet<MinlogProof>) -> IndexSet<Rc<MinlogType>> {
        self.formula.get_algebra_types(&mut IndexSet::new())
    }
    
    fn get_free_variables(&self, _visited: &mut IndexSet<MinlogProof>) -> IndexSet<Rc<MinlogTerm>> {
        self.formula.get_free_variables(&mut IndexSet::new())
            .union(&self.context.borrow().variables).cloned().collect()
    }
    
    fn get_bound_variables(&self, _visited: &mut IndexSet<MinlogProof>) -> IndexSet<Rc<MinlogTerm>> {
        self.formula.get_bound_variables(&mut IndexSet::new())
    }
    
    fn get_predicate_variables(&self, _visited: &mut IndexSet<MinlogProof>) -> IndexSet<Rc<MinlogPredicate>> {
        self.formula.get_predicate_variables(&mut IndexSet::new())
    }
    
    fn get_comprehension_terms(&self, _visited: &mut IndexSet<MinlogProof>) -> IndexSet<Rc<MinlogPredicate>> {
        self.formula.get_comprehension_terms(&mut IndexSet::new())
    }
    
    fn get_inductive_predicates(&self, _visited: &mut IndexSet<MinlogProof>) -> IndexSet<Rc<MinlogPredicate>> {
        self.formula.get_inductive_predicates(&mut IndexSet::new())
    }
    
    fn get_prime_formulas(&self, _visited: &mut IndexSet<MinlogProof>) -> IndexSet<Rc<MinlogPredicate>> {
        self.formula.get_prime_formulas(&mut IndexSet::new())
    }
    
    fn get_assumptions(&self, _visited: &mut IndexSet<MinlogProof>) -> IndexSet<Rc<MinlogProof>> {
        self.context.borrow().assumptions.clone()
    }
    
    fn substitute(&self, from: &ProofSubstEntry, to: &ProofSubstEntry) -> Rc<MinlogProof> {
        let new_context = ProofContext {
            assumptions: self.context.borrow().assumptions.iter()
                .map(|a| a.substitute(from, to))
                .collect(),
            variables: self.context.borrow().variables.iter()
                .map(|v| v.substitute_with(from, to))
                .collect()
        };
        
        let new_formula = self.formula.substitute_with(from, to);
        
        Rc::new(MinlogProof::Wildcard(ProofWildcard {
            formula: new_formula,
            context: RefCell::new(new_context),
        }))
    }
    
    fn first_conflict_with(&self, other: &Rc<MinlogProof>) -> Option<(ProofSubstEntry, ProofSubstEntry)> {
        if let Some(conflict) = self.formula.first_conflict_with(&other.proved_formula()) {
            return Some((conflict.0.into(), conflict.1.into()));
        }
        
        
        // TODO: Check context conflicts
        None
    }
    
    fn match_with(&self, ctx: &mut impl MatchContext<ProofSubstEntry>) -> MatchOutput<ProofSubstEntry> {
        let pattern = ctx.next_pattern().unwrap();
        let instance = ctx.next_instance().unwrap();
        
        match (pattern, instance) {
            (ProofSubstEntry::Proof(p), ProofSubstEntry::Proof(i)) => {
                if p.proved_formula() != i.proved_formula() {
                    ctx.extend(&p.proved_formula().into(), &i.proved_formula().into());
                }
                
                // TODO: Match contexts
                MatchOutput::Matched
            },
            _ => MatchOutput::FailedMatch,
        }
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
            self.context.borrow().to_pp_element(detail),
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
                vec![ProofTreeNode::new_leaf(self.context.borrow().display_string())],
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