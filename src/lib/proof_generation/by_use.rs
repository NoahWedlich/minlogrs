
use std::{rc::Rc, collections::HashSet};
use crate::core::predicates::minlog_predicate::MinlogPredicate;
use crate::core::proofs::goal::Goal;
use crate::core::proofs::minlog_proof::MinlogProof;
use crate::core::proofs::implication_elim::ImplicationElim;
use crate::core::proofs::universal_elim::UniversalElim;
use crate::core::proofs::proof_substitution::{ProofSubstEntry, ProofSubstitution};
use crate::core::proofs::proof_context::ProofContext;

pub fn generate_proof_by_use(target: &Rc<MinlogPredicate>, to_use: &Rc<MinlogProof>, context: &ProofContext) -> Rc<MinlogProof> {
    let mut proof = to_use.clone();
    let mut goal_index = proof.get_goals(&mut HashSet::new()).len();
    
    loop {
        if let Some(subst) = ProofSubstitution::match_with(&proof.proved_formula().into(), &target.into()) {
            let substituted_proof: ProofSubstEntry = subst.substitute(&proof.into());
            return substituted_proof.to_proof().unwrap();
        } else if let Some(imp) = proof.proved_formula().to_implication() {
            if let Some(first_premise) = imp.premises().first() {
                proof = ImplicationElim::create(
                    proof,
                    Goal::create(
                        format!("g{}", goal_index),
                        first_premise.clone(),
                        context.clone()
                    )
                );
                goal_index += 1;
            } else {
                panic!("Found implication with no premises while trying to use proof.");
            }
        } else if let Some(all) = proof.proved_formula().to_all_quantifier() {
            if let Some(var) = all.vars().first() {
                proof = UniversalElim::create(proof, var.clone());
            } else {
                panic!("Found universal quantifier with no variables while trying to use proof.");
            }
        } else {
            panic!("Failed to match the provided proof's formula with the target formula.");
        }
    }
}