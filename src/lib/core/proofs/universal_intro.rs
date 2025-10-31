
use std::{rc::Rc, collections::HashSet};

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
pub struct UniversalIntro {
    proof: Rc<MinlogProof>,
    variable: Rc<MinlogTerm>,
    formula: Rc<MinlogPredicate>,
}

impl UniversalIntro {
    pub fn create(proof: Rc<MinlogProof>, variable: Rc<MinlogTerm>) -> Rc<MinlogProof> {
        if proof.get_assumptions().iter().any(|assump| assump.contains_free_variable(&variable)) {
            panic!("UniversalIntro::create called with a proof that has assumptions depending on the variable");
        }
        
        let inner_formula = proof.proved_formula();
        let universal_formula = AllQuantifier::create(vec![variable.clone()], inner_formula);
        
        Rc::new(MinlogProof::UniversalIntro(UniversalIntro {
            proof,
            variable,
            formula: universal_formula,
        }))
    }
    
    pub fn proof(&self) -> Rc<MinlogProof> {
        self.proof.clone()
    }
    
    pub fn variable(&self) -> Rc<MinlogTerm> {
        self.variable.clone()
    }
}

impl ProofBody for UniversalIntro {
    fn proved_formula(&self) -> Rc<MinlogPredicate> {
        self.formula.clone()
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> Rc<MinlogProof> {
        Rc::new(MinlogProof::UniversalIntro(UniversalIntro {
            proof: self.proof.normalize(eta, pi),
            variable: self.variable.clone(),
            formula: self.formula.normalize(eta, pi),
        }))
    }
    
    fn get_type_variables(&self) -> HashSet<Rc<MinlogType>> {
        self.proof.get_type_variables()
    }
    
    fn get_algebra_types(&self) -> HashSet<Rc<MinlogType>> {
        self.proof.get_algebra_types()
    }
    
    fn get_free_variables(&self) -> HashSet<Rc<MinlogTerm>> {
        self.proof.get_free_variables()
            .difference(&HashSet::from([self.variable.clone()]))
            .cloned().collect()
    }
    
    fn get_bound_variables(&self) -> HashSet<Rc<MinlogTerm>> {
        let mut vars = self.proof.get_bound_variables();
        vars.insert(self.variable.clone());
        vars
    }
    
    fn get_predicate_variables(&self) -> HashSet<Rc<MinlogPredicate>> {
        self.proof.get_predicate_variables()
    }
    
    fn get_comprehension_terms(&self) -> HashSet<Rc<MinlogPredicate>> {
        self.proof.get_comprehension_terms()
    }
    
    fn get_inductive_predicates(&self) -> HashSet<Rc<MinlogPredicate>> {
        self.proof.get_inductive_predicates()
    }
    
    fn get_prime_formulas(&self) -> HashSet<Rc<MinlogPredicate>> {
        self.proof.get_prime_formulas()
    }

    fn get_goals(&self) -> HashSet<Rc<MinlogProof>> {
        self.proof.get_goals()
    }

    fn get_assumptions(&self) -> HashSet<Rc<MinlogProof>> {
        self.proof.get_assumptions()
    }
    
    fn get_axioms(&self) -> HashSet<Rc<MinlogProof>> {
        self.proof.get_axioms()
    }
    
    fn get_theorems(&self) -> HashSet<Rc<MinlogProof>> {
        self.proof.get_theorems()
    }
    
    fn substitute(&self, from: &ProofSubstEntry, to: &ProofSubstEntry) -> Rc<MinlogProof> {
        if let ProofSubstEntry::Proof(from_proof) = from && from_proof.is_universal_intro() && self == from_proof.to_universal_intro().unwrap() {
            to.to_proof().unwrap()
        } else {
            UniversalIntro::create(
                self.proof.substitute(from, to),
                self.variable.substitute_with(from, to),
            )
        }
    }
    
    fn first_conflict_with(&self, other: &Rc<MinlogProof>) -> Option<(ProofSubstEntry, ProofSubstEntry)> {
        if let Some(ue_intro) = other.to_universal_intro() {
            if let Some(conflict) = self.proof.first_conflict_with(&ue_intro.proof) {
                return Some(conflict);
            }
            
            if let Some(var_conflict) = self.variable.first_conflict_with(&ue_intro.variable) {
                return Some((var_conflict.0.into(), var_conflict.1.into()));
            }
            
            None
        } else {
            Some((Rc::new(MinlogProof::UniversalIntro(self.clone())).into(), other.clone().into()))
        }
    }
    
    fn match_with(&self, ctx: &mut impl MatchContext<ProofSubstEntry>) -> MatchOutput<ProofSubstEntry> {
        let pattern = ctx.next_pattern().unwrap();
        let instance = ctx.next_instance().unwrap();
        
        match (pattern, instance) {
            (ProofSubstEntry::Proof(p), ProofSubstEntry::Proof(i)) => {
                if !p.is_universal_intro() || !i.is_universal_intro() {
                    return MatchOutput::FailedMatch;
                }
                
                let ui_pattern = p.to_universal_intro().unwrap();
                let ui_instance = i.to_universal_intro().unwrap();
                
                if ui_pattern.variable != ui_instance.variable {
                    ctx.extend(&ui_pattern.variable.clone().into(), &ui_instance.variable.clone().into());
                }
                
                if ui_pattern.proof != ui_instance.proof {
                    ctx.extend(&ui_pattern.proof.clone().into(), &ui_instance.proof.clone().into());
                }
                
                MatchOutput::Matched
            },
            _ => MatchOutput::FailedMatch,
        }
    }
}

impl PrettyPrintable for UniversalIntro {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        PPElement::group(vec![
            PPElement::text("∀⁺ (".to_string()),
            PPElement::break_elem(1, 4, false),
            PPElement::list(
                vec![
                    self.proof.to_pp_element(detail),
                    self.variable.to_pp_element(detail),
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

impl ProofTreeDisplayable for UniversalIntro {
    fn to_proof_tree_node(&self) -> ProofTreeNode {
        ProofTreeNode::new_node(
            vec![self.proof.to_proof_tree_node()],
            self.formula.display_string(),
            Some(format!("∀⁺{}", self.variable.to_variable().unwrap().name()))
        )
    }
}