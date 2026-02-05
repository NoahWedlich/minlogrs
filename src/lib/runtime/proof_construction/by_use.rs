
use crate::includes::{
    essential::*,
    kernel::{
        predicates::*,
        proofs::*,
    }
};

pub fn generate_proof_by_use(target: &Arc<MinlogPredicate>, to_use: &Arc<MinlogProof>, context: &ProofContext) -> Arc<MinlogProof> {
    let mut proof = to_use.clone();
    let mut goal_index = proof.get_goals().len();
    
    loop {
        if let Some(subst) = ProofSubstitution::match_with(&proof.proved_formula().into(), &target.into()) {
            let substituted_proof: ProofSubstEntry = subst.substitute(&proof.into());
            return substituted_proof.to_proof().unwrap();
        } else if let Some(imp) = proof.proved_formula().to_implication() {
            proof = ImplicationElim::create(
                proof,
                Goal::create(
                    format!("g{}", goal_index),
                    imp.premise().clone(),
                    context.clone()
                )
            );
            goal_index += 1;
        } else if let Some(all) = proof.proved_formula().to_all_quantifier() {
            proof = UniversalElim::create(proof, all.var().clone());
        } else {
            panic!("Failed to match the provided proof's formula with the target formula.");
        }
    }
}