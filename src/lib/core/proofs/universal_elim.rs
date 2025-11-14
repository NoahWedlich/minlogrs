
use indexmap::IndexSet;
use std::rc::Rc;

use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};
use crate::utils::proof_tree_display::{ProofTreeDisplayable, ProofTreeNode};

use crate::core::substitution::{MatchContext, MatchOutput, SubstitutableWith};

use crate::core::types::minlog_type::MinlogType;
use crate::core::terms::minlog_term::MinlogTerm;

use crate::core::predicates::minlog_predicate::MinlogPredicate;
use crate::core::predicates::all_quantifier::AllQuantifier;

use crate::core::proofs::minlog_proof::{MinlogProof, ProofBody};

use crate::core::proofs::proof_substitution::ProofSubstEntry;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct UniversalElim {
    proof: Rc<MinlogProof>,
    term: Rc<MinlogTerm>,
    replaced_variable: Rc<MinlogTerm>,
    formula: Rc<MinlogPredicate>,
}

impl UniversalElim {
    pub fn create(proof: Rc<MinlogProof>, term: Rc<MinlogTerm>) -> Rc<MinlogProof> {
        let universal_formula = proof.proved_formula();
        
        if !universal_formula.is_all_quantifier() {
            panic!("UniversalElim::create called with a proof that does not prove a universal formula");
        }
        
        let all_quantifier = universal_formula.to_all_quantifier().unwrap();
        let vars = all_quantifier.vars();
        
        if vars.is_empty() {
            panic!("UniversalElim::create called with an universal formula that has no bound variables");
        }
        
        let var = vars[0].clone();
        let remaining_vars = vars[1..].to_vec();
        
        let inner_formula = all_quantifier.body().substitute(&var.clone().into(), &term.clone().into());
        let resulting_formula = if remaining_vars.is_empty() {
            inner_formula
        } else {
            AllQuantifier::create(remaining_vars, inner_formula)
        };
        
        Rc::new(MinlogProof::UniversalElim(UniversalElim {
            proof,
            term,
            replaced_variable: var,
            formula: resulting_formula,
        }))
    }
    
    pub fn proof(&self) -> Rc<MinlogProof> {
        self.proof.clone()
    }
    
    pub fn term(&self) -> Rc<MinlogTerm> {
        self.term.clone()
    }
}

impl ProofBody for UniversalElim {
    fn proved_formula(&self) -> Rc<MinlogPredicate> {
        self.formula.clone()
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> Rc<MinlogProof> {
        Rc::new(MinlogProof::UniversalElim(UniversalElim {
            proof: self.proof.normalize(eta, pi),
            term: self.term.clone(),
            replaced_variable: self.replaced_variable.clone(),
            formula: self.formula.normalize(eta, pi),
        }))
    }
    
    fn unfold(&self) -> Rc<MinlogProof> {
        Rc::new(MinlogProof::UniversalElim(UniversalElim {
            proof: self.proof.unfold(),
            term: self.term.clone(),
            replaced_variable: self.replaced_variable.clone(),
            formula: self.formula.clone(),
        }))
    }
    
    fn extracted_term(&self) -> Option<Rc<MinlogTerm>> {
        self.proof.extracted_term()
    }
    
    fn get_type_variables(&self, visited: &mut IndexSet<MinlogProof>) -> IndexSet<Rc<MinlogType>> {
        if visited.contains(&MinlogProof::UniversalElim(self.clone())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogProof::UniversalElim(self.clone()));
            
            self.proof.get_type_variables(visited)
                .union(&self.term.get_type_variables(&mut IndexSet::new()))
                .cloned().collect()
        }
    }
    
    fn get_algebra_types(&self, visited: &mut IndexSet<MinlogProof>) -> IndexSet<Rc<MinlogType>> {
        if visited.contains(&MinlogProof::UniversalElim(self.clone())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogProof::UniversalElim(self.clone()));
            
            self.proof.get_algebra_types(visited)
                .union(&self.term.get_algebra_types(&mut IndexSet::new()))
                .cloned().collect()
        }
    }
    
    fn get_free_variables(&self, visited: &mut IndexSet<MinlogProof>) -> IndexSet<Rc<MinlogTerm>> {
        if visited.contains(&MinlogProof::UniversalElim(self.clone())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogProof::UniversalElim(self.clone()));
            
            self.proof.get_free_variables(visited)
                .union(&self.term.get_free_variables(&mut IndexSet::new()))
                .cloned().collect()
        }
    }
    
    fn get_bound_variables(&self, visited: &mut IndexSet<MinlogProof>) -> IndexSet<Rc<MinlogTerm>> {
        if visited.contains(&MinlogProof::UniversalElim(self.clone())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogProof::UniversalElim(self.clone()));
            
            self.proof.get_bound_variables(visited)
                .union(&self.term.get_bound_variables(&mut IndexSet::new()))
                .cloned().collect::<IndexSet<_>>()
                .difference(&IndexSet::from([self.replaced_variable.clone()]))
                .cloned().collect()
        }
    }
    
    fn get_predicate_variables(&self, visited: &mut IndexSet<MinlogProof>) -> IndexSet<Rc<MinlogPredicate>> {
        if visited.contains(&MinlogProof::UniversalElim(self.clone())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogProof::UniversalElim(self.clone()));
            
            self.proof.get_predicate_variables(visited)
        }
    }
    
    fn get_comprehension_terms(&self, visited: &mut IndexSet<MinlogProof>) -> IndexSet<Rc<MinlogPredicate>> {
        if visited.contains(&MinlogProof::UniversalElim(self.clone())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogProof::UniversalElim(self.clone()));
            
            self.proof.get_comprehension_terms(visited)
        }
    }
    
    fn get_inductive_predicates(&self, visited: &mut IndexSet<MinlogProof>) -> IndexSet<Rc<MinlogPredicate>> {
        if visited.contains(&MinlogProof::UniversalElim(self.clone())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogProof::UniversalElim(self.clone()));
            
            self.proof.get_inductive_predicates(visited)
        }
    }
    
    fn get_prime_formulas(&self, visited: &mut IndexSet<MinlogProof>) -> IndexSet<Rc<MinlogPredicate>> {
        if visited.contains(&MinlogProof::UniversalElim(self.clone())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogProof::UniversalElim(self.clone()));
            
            self.proof.get_prime_formulas(visited)
        }
    }
    
    fn get_goals(&self, visited: &mut IndexSet<MinlogProof>) -> IndexSet<Rc<MinlogProof>> {
        if visited.contains(&MinlogProof::UniversalElim(self.clone())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogProof::UniversalElim(self.clone()));
            
            self.proof.get_goals(visited)
        }
    }
    
    fn get_assumptions(&self, visited: &mut IndexSet<MinlogProof>) -> IndexSet<Rc<MinlogProof>> {
        if visited.contains(&MinlogProof::UniversalElim(self.clone())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogProof::UniversalElim(self.clone()));
            
            self.proof.get_assumptions(visited)
        }
    }
    
    fn get_axioms(&self, visited: &mut IndexSet<MinlogProof>) -> IndexSet<Rc<MinlogProof>> {
        if visited.contains(&MinlogProof::UniversalElim(self.clone())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogProof::UniversalElim(self.clone()));
            
            self.proof.get_axioms(visited)
        }
    }
    
    fn get_theorems(&self, visited: &mut IndexSet<MinlogProof>) -> IndexSet<Rc<MinlogProof>> {
        if visited.contains(&MinlogProof::UniversalElim(self.clone())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogProof::UniversalElim(self.clone()));
            
            self.proof.get_theorems(visited)
        }
    }
    
    fn substitute(&self, from: &ProofSubstEntry, to: &ProofSubstEntry) -> Rc<MinlogProof> {
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
    
    fn first_conflict_with(&self, other: &Rc<MinlogProof>) -> Option<(ProofSubstEntry, ProofSubstEntry)> {
        if let MinlogProof::UniversalElim(other_universal_elim) = other.as_ref() {
            if let Some(conflict) = self.proof.first_conflict_with(&other_universal_elim.proof) {
                return Some(conflict);
            }
            
            if let Some(conflict) = self.term.first_conflict_with(&other_universal_elim.term) {
                return Some((conflict.0.into(), conflict.1.into()));
            }
            
            None
        } else {
            Some((Rc::new(MinlogProof::UniversalElim(self.clone())).into(), other.clone().into()))
        }
    }
    
    fn match_with(&self, ctx: &mut impl MatchContext<ProofSubstEntry>) -> MatchOutput<ProofSubstEntry> {
        let pattern = ctx.next_pattern().unwrap();
        let instance = ctx.next_instance().unwrap();
        
        match (pattern, instance) {
            (ProofSubstEntry::Proof(p), ProofSubstEntry::Proof(i)) => {
                if !p.is_universal_elim() || !i.is_universal_elim() {
                    return MatchOutput::FailedMatch;
                }
                
                let ue_pattern = p.to_universal_elim().unwrap();
                let ue_instance = i.to_universal_elim().unwrap();
                
                if ue_pattern.term != ue_instance.term {
                    ctx.extend(&ue_pattern.term.clone().into(), &ue_instance.term.clone().into());
                }
                
                if ue_pattern.proof != ue_instance.proof {
                    ctx.extend(&ue_pattern.proof.clone().into(), &ue_instance.proof.clone().into());
                }
                
                MatchOutput::Matched
            },
            _ => MatchOutput::FailedMatch,
        }
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