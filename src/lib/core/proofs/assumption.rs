
use std::{rc::Rc, collections::HashSet};

use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};
use crate::utils::proof_tree_display::{ProofTreeDisplayable, ProofTreeNode};

use crate::core::substitution::{MatchContext, MatchOutput, SubstitutableWith};

use crate::core::types::minlog_type::MinlogType;
use crate::core::terms::minlog_term::MinlogTerm;
use crate::core::predicates::minlog_predicate::MinlogPredicate;

use crate::core::proofs::minlog_proof::{MinlogProof, ProofBody};

use crate::core::proofs::proof_substitution::ProofSubstEntry;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Assumption {
    name: String,
    formula: Rc<MinlogPredicate>,
    index: usize,
}

impl Assumption {
    pub fn create(name: String, formula: Rc<MinlogPredicate>) -> Rc<MinlogProof> {
        if !formula.is_formula() {
            panic!("Only assumptions of nullary predicates are allowed.")
        }
        
        Rc::new(MinlogProof::Assumption(Assumption { name, formula, index: 0 }))
    }
    
    pub fn unshadow(proof: &Rc<MinlogProof>) -> Rc<MinlogProof> {
        if let Some(assump) = proof.to_assumption() {
            Rc::new(MinlogProof::Assumption(Assumption {
                name: assump.name.clone(),
                formula: assump.formula.clone(),
                index: assump.index + 1,
            }))
        } else {
            panic!("Called Assumption::unshadow on a non-assumption proof");
        }
    }
    
    pub fn name(&self) -> &str {
        &self.name
    }
    
    pub fn index(&self) -> usize {
        self.index
    }
}

impl ProofBody for Assumption {
    fn proved_formula(&self) -> Rc<MinlogPredicate> {
        self.formula.clone()
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> Rc<MinlogProof> {
        Rc::new(MinlogProof::Assumption(Assumption {
            name: self.name.clone(),
            formula: self.formula.normalize(eta, pi),
            index: self.index,
        }))
    }
    
    fn unfold(&self) -> Rc<MinlogProof> {
        Rc::new(MinlogProof::Assumption(self.clone()))
    }
    
    fn get_type_variables(&self, _visited: &mut HashSet<MinlogProof>) -> HashSet<Rc<MinlogType>> {
        self.formula.get_type_variables(&mut HashSet::new())
    }
    
    fn get_algebra_types(&self, _visited: &mut HashSet<MinlogProof>) -> HashSet<Rc<MinlogType>> {
        self.formula.get_algebra_types(&mut HashSet::new())
    }
    
    fn get_free_variables(&self, _visited: &mut HashSet<MinlogProof>) -> HashSet<Rc<MinlogTerm>> {
        self.formula.get_free_variables(&mut HashSet::new())
    }
    
    fn get_bound_variables(&self, _visited: &mut HashSet<MinlogProof>) -> HashSet<Rc<MinlogTerm>> {
        self.formula.get_bound_variables(&mut HashSet::new())
    }
    
    fn get_predicate_variables(&self, _visited: &mut HashSet<MinlogProof>) -> HashSet<Rc<MinlogPredicate>> {
        self.formula.get_predicate_variables(&mut HashSet::new())
    }
    
    fn get_comprehension_terms(&self, _visited: &mut HashSet<MinlogProof>) -> HashSet<Rc<MinlogPredicate>> {
        self.formula.get_comprehension_terms(&mut HashSet::new())
    }
    
    fn get_inductive_predicates(&self, _visited: &mut HashSet<MinlogProof>) -> HashSet<Rc<MinlogPredicate>> {
        self.formula.get_inductive_predicates(&mut HashSet::new())
    }
    
    fn get_prime_formulas(&self, _visited: &mut HashSet<MinlogProof>) -> HashSet<Rc<MinlogPredicate>> {
        self.formula.get_prime_formulas(&mut HashSet::new())
    }
    
    fn get_assumptions(&self, _visited: &mut HashSet<MinlogProof>) -> HashSet<Rc<MinlogProof>> {
        HashSet::from([Rc::new(MinlogProof::Assumption(self.clone()))])
    }
    
    fn substitute(&self, from: &ProofSubstEntry, to: &ProofSubstEntry) -> Rc<MinlogProof> {
        if let ProofSubstEntry::Proof(from_proof) = from && from_proof.is_assumption() && self == from_proof.to_assumption().unwrap() {
            to.to_proof().unwrap()
        } else {
            Assumption::create(
                self.name.clone(),
                self.formula.substitute_with(from, to),
            )
        }
    }
    
    fn first_conflict_with(&self, other: &Rc<MinlogProof>) -> Option<(ProofSubstEntry, ProofSubstEntry)> {
        if let Some(conflict) = self.formula.first_conflict_with(&other.proved_formula()) {
            return Some((conflict.0.into(), conflict.1.into()));
        }
        
        if other.is_assumption() && self == other.to_assumption().unwrap() {
            None
        } else {
            Some((Rc::new(MinlogProof::Assumption(self.clone())).into(), other.clone().into()))
        }
    }
    
    fn match_with(&self, ctx: &mut impl MatchContext<ProofSubstEntry>) -> MatchOutput<ProofSubstEntry> {
        let pattern = ctx.next_pattern().unwrap();
        let instance = ctx.next_instance().unwrap();
        
        match (pattern, instance) {
            (ProofSubstEntry::Proof(p), ProofSubstEntry::Proof(i)) => {
                if !p.is_assumption() || !i.is_assumption() {
                    return MatchOutput::FailedMatch;
                }
                
                if p.proved_formula() != i.proved_formula() {
                    ctx.extend(&p.proved_formula().into(), &i.proved_formula().into());
                }
                
                MatchOutput::Matched
            },
            _ => MatchOutput::FailedMatch,
        }
    }
}

impl PrettyPrintable for Assumption {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        PPElement::group(vec![
            if self.index > 0 {
                PPElement::text(format!("{}_{}", self.name, self.index))
            } else {
                PPElement::text(self.name.clone())
            },
            PPElement::break_elem(0, 0, false),
            PPElement::text(":".to_string()),
            PPElement::break_elem(1, 4, false),
            self.formula.to_pp_element(detail),
        ], BreakType::Flexible, 0)
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}

impl ProofTreeDisplayable for Assumption {
    fn to_proof_tree_node(&self) -> ProofTreeNode {
        ProofTreeNode::new_leaf(self.display_string())
    }
}