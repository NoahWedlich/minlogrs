
use indexmap::IndexSet;
use std::rc::Rc;
use crate::utils::pretty_printer::*;
use crate::core::polarity::Polarity;
use crate::proof_generation::by_use::generate_proof_by_use;
use crate::core::predicates::minlog_predicate::MinlogPredicate;
use crate::core::proofs::minlog_proof::MinlogProof;
use crate::core::proofs::axiom::Axiom;
use crate::core::proofs::proof_context::ProofContext;

pub fn generate_proof_by_intro(target: &Rc<MinlogPredicate>, clause: &String, context: &ProofContext) -> Rc<MinlogProof> {
    let idps = target.get_polarized_inductive_preds(Polarity::StrictlyPositive, &mut IndexSet::new())
        .into_iter().filter_map(|polarized| {
            if polarized.polarity == Polarity::StrictlyPositive {
                Some(polarized.value)
            } else {
                None
            }
        }).collect::<IndexSet<_>>();

    let inductive_predicate = if idps.len() == 1 {
        idps.iter().next().unwrap().clone()
    } else {
        panic!("Cannot generate proof by introduction for target \n{}\n\tfound {} strictly positive inductive predicates, expected exactly one.",
        target.debug_string(), idps.len());
    };
    
    if let Some(inductive) = inductive_predicate.to_inductive_predicate() {
        let clauses = inductive.clauses();
        if let Some((name, clause_pred)) = clauses.iter().find(|(name, _)| name == clause) {
            let intro_axiom = Axiom::create(
                name.clone(),
                clause_pred.clone()
            );
            
            generate_proof_by_use(
                target,
                &intro_axiom,
                context
            )
        } else {
            panic!("Clause {} not found in inductive predicate {}", clause, inductive_predicate.debug_string());
        }
    } else {
        panic!("Cannot generate proof by introduction for non-inductive predicate {}", inductive_predicate.debug_string());
    }
}