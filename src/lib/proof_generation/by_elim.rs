
use crate::includes::{
    essential::*,
    core::proofs::*,
    builtin::*,
    proof_generation::*,
};

pub fn generate_proof_by_elim(inductive_predicate: &Rc<MinlogPredicate>) -> Rc<MinlogProof> {
    let elim_axiom = extract_elimination_axiom(inductive_predicate, &mut IndexMap::new());
    
    let mut proof = elim_axiom;
    
    let mut dependencies = IndexSet::new();
    
    let mut variables = vec![];
    while let Some(all) = proof.proved_formula().to_all_quantifier() {
        let var = all.vars()[0].clone();
        variables.push(var.clone());
        proof = UniversalElim::create(proof, var);
    }
    
    let mut idp_premises = vec![];
    while let Some(imp_inner) = proof.proved_formula().to_implication()
        && let Some(first_prime) = imp_inner.premises().first().unwrap().to_prime()
        && let Some(idp) = first_prime.body().to_inductive_predicate()
        && idp.references_idp(inductive_predicate)
    {
        let assumption = Assumption::create(
            format!("idp_premise_{}", idp_premises.len()),
            imp_inner.premises()[0].clone()
        );
        
        idp_premises.push(assumption.clone());
        proof = ImplicationElim::create(proof, assumption);
    }
    
    let mut goal_index = 0;
    while let Some(imp) = proof.proved_formula().to_implication() {
        let premise = imp.premises()[0].clone();
        let provided_vars = variables.iter().cloned().collect::<IndexSet<_>>()
            .intersection(&premise.get_free_variables(&mut IndexSet::new())).cloned().collect::<IndexSet<_>>();
        
        let context = ProofContext {
            assumptions: IndexSet::new(),
            variables: provided_vars,
        };
        
        let reduced_premise = generate_proof_by_assume_with_name(
            &premise,
            &vec![],
            &context,
            format!("g{}", goal_index),
        );
        
        dependencies.extend(
            reduced_premise.to_bundled_proof().map_or(IndexSet::new(), |bp| bp.dependencies().clone())
        );
        
        goal_index += 1;
        
        proof = ImplicationElim::create(proof, reduced_premise);
    }
    
    for assumption in idp_premises.into_iter().rev() {
        proof = ImplicationIntro::create(proof, assumption);
    }
    
    for arg in variables.into_iter().rev() {
        proof = UniversalIntro::create(proof, arg);
    }

    BundledProof::create(proof, "by_elim".to_string(), dependencies)
}