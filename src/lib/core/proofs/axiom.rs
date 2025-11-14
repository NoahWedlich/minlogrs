
use indexmap::IndexSet;
use std::{rc::Rc, cell::RefCell, hash::{Hash, Hasher}};

use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};
use crate::utils::proof_tree_display::{ProofTreeDisplayable, ProofTreeNode};

use crate::core::substitution::{MatchContext, MatchOutput, SubstitutableWith};

use crate::core::types::minlog_type::MinlogType;

use crate::core::terms::minlog_term::MinlogTerm;
use crate::core::terms::program_term::ProgramTerm;
use crate::core::terms::term_substitution::TermSubstitution;

use crate::core::predicates::minlog_predicate::MinlogPredicate;

use crate::core::proofs::minlog_proof::{MinlogProof, ProofBody};

use crate::core::proofs::proof_substitution::ProofSubstEntry;

use crate::core::structures::program_constant::ProgramConstant;

#[derive(Clone, PartialEq, Eq)]
pub struct Axiom {
    name: String,
    formula: Rc<MinlogPredicate>,
    content: RefCell<Option<Rc<MinlogTerm>>>,
}

impl Axiom {
    pub fn create(name: String, formula: Rc<MinlogPredicate>) -> Rc<MinlogProof> {
        if !formula.is_formula() {
            panic!("Can only create axioms of nullary predicates")
        }
        
        Rc::new(MinlogProof::Axiom(Axiom { name, formula, content: RefCell::new(None) }))
    }
    
    pub fn name(&self) -> &str {
        &self.name
    }
    
    pub fn has_content(&self) -> bool {
        self.content.borrow().is_some()
    }
    
    pub fn content(&self) -> Option<Rc<MinlogTerm>> {
        self.content.borrow().clone()
    }
    
    pub fn set_content(&self, content: Rc<MinlogTerm>) {
        if content.minlog_type() != self.formula.extracted_type().remove_nulls().unwrap() {
            panic!("Tried to set axiom content with mismatching type");
        }
        
        *self.content.borrow_mut() = Some(content);
    }
}

impl ProofBody for Axiom {
    fn proved_formula(&self) -> Rc<MinlogPredicate> {
        self.formula.clone()
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> Rc<MinlogProof> {
        Rc::new(MinlogProof::Axiom(Axiom {
            name: self.name.clone(),
            formula: self.formula.normalize(eta, pi),
            content: RefCell::new(self.content.borrow().clone()),
        }))
    }
    
    fn unfold(&self) -> Rc<MinlogProof> {
        Rc::new(MinlogProof::Axiom(self.clone()))
    }
    
    fn extracted_term(&self) -> Option<Rc<MinlogTerm>> {
        self.formula.extracted_type().remove_nulls().map(|t| {
            if let Some(content) = self.content.borrow().clone() {
                content
            } else {
                let pconst = ProgramConstant::create(self.name.clone(), t);
                ProgramTerm::create(pconst, TermSubstitution::make_empty())
            }
        })
    }
    
    fn get_type_variables(&self, _visited: &mut IndexSet<MinlogProof>) -> IndexSet<Rc<MinlogType>> {
        self.formula.get_type_variables(&mut IndexSet::new())
    }
    
    fn get_algebra_types(&self, _visited: &mut IndexSet<MinlogProof>) -> IndexSet<Rc<MinlogType>> {
        self.formula.get_algebra_types(&mut IndexSet::new())
    }
    
    fn get_free_variables(&self, _visited: &mut IndexSet<MinlogProof>) -> IndexSet<Rc<MinlogTerm>> {
        self.formula.get_free_variables(&mut IndexSet::new())
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
    
    fn get_axioms(&self, _visited: &mut IndexSet<MinlogProof>) -> IndexSet<Rc<MinlogProof>> {
        IndexSet::from([Rc::new(MinlogProof::Axiom(self.clone()))])
    }
    
    fn substitute(&self, from: &ProofSubstEntry, to: &ProofSubstEntry) -> Rc<MinlogProof> {
        if let ProofSubstEntry::Proof(from_proof) = from && from_proof.is_axiom() && self == from_proof.to_axiom().unwrap() {
            to.to_proof().unwrap()
        } else {
            Axiom::create(
                self.name.clone(),
                self.formula.substitute_with(from, to),
            )
        }
    }
    
    fn first_conflict_with(&self, other: &Rc<MinlogProof>) -> Option<(ProofSubstEntry, ProofSubstEntry)> {
        if let Some(conflict) = self.formula.first_conflict_with(&other.proved_formula()) {
            return Some((conflict.0.into(), conflict.1.into()));
        }
        
        if other.is_axiom() && self == other.to_axiom().unwrap() {
            None
        } else {
            Some((Rc::new(MinlogProof::Axiom(self.clone())).into(), other.clone().into()))
        }
    }
    
    fn match_with(&self, ctx: &mut impl MatchContext<ProofSubstEntry>) -> MatchOutput<ProofSubstEntry> {
        let pattern = ctx.next_pattern().unwrap();
        let instance = ctx.next_instance().unwrap();
        
        match (pattern, instance) {
            (ProofSubstEntry::Proof(p), ProofSubstEntry::Proof(i)) => {
                if !p.is_axiom() || !i.is_axiom() {
                    return MatchOutput::FailedMatch;
                }
                
                if p.proved_formula() != i.proved_formula() {
                    ctx.extend(&p.proved_formula().into(), &i.proved_formula().into());
                }
                
                let axiom_pattern = p.to_axiom().unwrap();
                let axiom_instance = i.to_axiom().unwrap();
                
                if axiom_pattern.name != axiom_instance.name {
                    MatchOutput::FailedMatch
                } else {
                    MatchOutput::Matched
                }
            },
            _ => MatchOutput::FailedMatch,
        }
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