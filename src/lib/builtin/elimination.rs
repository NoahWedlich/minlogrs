
use std::{rc::Rc, collections::{HashMap, HashSet}};
use crate::core::proofs::{assumption::Assumption, axiom::Axiom, bundled_proof::BundledProof, implication_intro::ImplicationIntro, universal_intro::UniversalIntro};
use crate::core::proofs::goal::Goal;
use crate::core::proofs::implication_elim::ImplicationElim;
use crate::core::proofs::minlog_proof::MinlogProof;
use crate::core::proofs::universal_elim::UniversalElim;
use crate::utils::pretty_printer::*;

use crate::core::{
    predicates::{
        minlog_predicate::MinlogPredicate,
        predicate_variable::PredicateVariable,
        prime_formula::PrimeFormula,
        implication::Implication,
        all_quantifier::AllQuantifier
    }, terms::{
        minlog_term::Totality, term_variable::TermVariable
    }
};

pub fn extract_elimination_proof(inductive_predicate: &Rc<MinlogPredicate>) -> Rc<MinlogProof> {
    let elim_axiom = extract_elimination_axiom(inductive_predicate);
    let mut proof = elim_axiom;
    
    let mut arguments = HashSet::new();
    
    while let Some(all) = proof.proved_formula().to_all_quantifier() {
        let var = all.vars()[0].clone();
        arguments.insert(var.clone());
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
        let provided_vars = arguments.intersection(&premise.get_free_variables(&mut HashSet::new())).cloned().collect::<HashSet<_>>();
        
        let mut reduced_premise = premise.clone();
        
        let mut sub_vars = vec![];
        if let Some(all) = reduced_premise.to_all_quantifier() {
            sub_vars.extend(all.vars().iter().cloned());
            reduced_premise = all.body().clone();
        }
        
        let mut assumptions = vec![];
        if let Some(imp_inner) = reduced_premise.to_implication() {
            for assumption in imp_inner.premises().iter() {
                assumptions.push(Assumption::create(
                    format!("a{}", assumptions.len()),
                    assumption.clone()
                ));
            }
            reduced_premise = imp_inner.conclusion().clone();
        }
        
        let sub_vars_set = sub_vars.iter().cloned().collect::<HashSet<_>>();
        let assumptions_set = assumptions.iter().cloned().collect::<HashSet<_>>();
        
        let mut sub_proof = Goal::create(
            format!("g{}", goal_index),
            reduced_premise,
            provided_vars.union(&sub_vars_set).cloned().collect(),
            assumptions_set
        );
        
        for assumption in assumptions.iter().rev() {
            sub_proof = ImplicationIntro::create(sub_proof, assumption.clone());
        }
        
        for var in sub_vars.iter().rev() {
            sub_proof = UniversalIntro::create(sub_proof, var.clone());
        }
        
        proof = ImplicationElim::create(
            proof,
            sub_proof
        );
        goal_index += 1;
    }
    
    for assumption in idp_premises.iter().rev() {
        proof = ImplicationIntro::create(proof, assumption.clone());
    }
    
    for arg in arguments.iter() {
        proof = UniversalIntro::create(proof, arg.clone());
    }
    
    BundledProof::create(proof, "elim".to_string())
}

pub fn extract_elimination_axiom(inductive_predicate: &Rc<MinlogPredicate>) -> Rc<MinlogProof> {
    let pvar = PredicateVariable::create(
        "P0".to_string(),
        inductive_predicate.arity().clone(),
        inductive_predicate.degree()
    );
    
    let arguments = (0..inductive_predicate.unpacked_arity().len()).map(|i| {
        TermVariable::create(
            format!("x{}", i),
            inductive_predicate.unpacked_arity()[i].clone(),
            Totality::Partial
        )
    }).collect::<Vec<_>>();
    
    let mut elimination_clauses = vec![PrimeFormula::create(inductive_predicate.clone(), arguments.clone())];

    elimination_clauses.extend(
        inductive_predicate.to_inductive_predicate().unwrap().clauses().iter().map(|(_, clause)| {
            clause_to_elimination_clauses(clause, inductive_predicate, &mut HashMap::new())
        })
    );
    
    let conclusion = PrimeFormula::create(pvar, arguments.clone());
    
    Axiom::create(
        "Elim".to_string(),
        AllQuantifier::create(
            arguments,
            Implication::create(elimination_clauses, conclusion)
        )
    )
}

fn clause_to_elimination_clauses(
    clause: &Rc<MinlogPredicate>,
    rel_idp: &Rc<MinlogPredicate>,
    pvars: &mut HashMap<Rc<MinlogPredicate>, Rc<MinlogPredicate>>
) -> Rc<MinlogPredicate> {
    match clause.as_ref() {
        MinlogPredicate::Prime(prime) => {
            if let Some(idp) = prime.body().to_inductive_predicate() && idp.references_idp(rel_idp) {
                let pvar = if let Some(existing) = pvars.get(prime.body()) {
                    existing.clone()
                } else {
                    let index = pvars.len();
                    let new_pvar = PredicateVariable::create(
                        format!("P{}", index),
                        prime.body().arity().clone(),
                        prime.body().degree()
                    );
                    pvars.insert(prime.body().clone(), new_pvar.clone());
                    new_pvar
                };
                
                PrimeFormula::create(pvar, prime.arguments().clone())
            } else {
                panic!("Clause must result in IDP, but found: {}", clause.debug_string());
            }
        },
        MinlogPredicate::Implication(imp) => {
            let premise_clauses = imp.premises().iter().flat_map(|arg| {
                inner_clause_to_elimination_clauses(arg, rel_idp, pvars)
            }).collect::<Vec<_>>();
            
            let conclusion_clause = clause_to_elimination_clauses(imp.conclusion(), rel_idp, pvars);
            
            Implication::create(premise_clauses, conclusion_clause)
        },
        MinlogPredicate::AllQuantifier(all) => {
            let body_clause = clause_to_elimination_clauses(all.body(), rel_idp, pvars);
            AllQuantifier::create(all.vars().clone(), body_clause)
        },
        _ => {
            if !clause.is_formula() {
                panic!("Expected formula, but got non-nullary predicate: {}", clause.debug_string());
            }
            
            clause_to_elimination_clauses(&PrimeFormula::create(clause.clone(), vec![]), rel_idp, pvars)
        }
    }
}

fn inner_clause_to_elimination_clauses(
    clause: &Rc<MinlogPredicate>,
    rel_idp: &Rc<MinlogPredicate>,
    pvars: &mut HashMap<Rc<MinlogPredicate>, Rc<MinlogPredicate>>
) -> Vec<Rc<MinlogPredicate>> {
    match clause.as_ref() {
        MinlogPredicate::Prime(prime) => {
            if let Some(idp) = prime.body().to_inductive_predicate() && idp.references_idp(rel_idp) {
                let pvar = if let Some(existing) = pvars.get(prime.body()) {
                    existing.clone()
                } else {
                    let index = pvars.len();
                    let new_pvar = PredicateVariable::create(
                        format!("P{}", index),
                        prime.body().arity().clone(),
                        prime.body().degree()
                    );
                    pvars.insert(prime.body().clone(), new_pvar.clone());
                    new_pvar
                };
                
                vec![
                    PrimeFormula::create(prime.body().clone(), prime.arguments().clone()),
                    PrimeFormula::create(pvar, prime.arguments().clone())
                ]
            } else {
                vec![clause.clone()]
            }
        },
        MinlogPredicate::Implication(imp) => {
            let premise_clauses = imp.premises().iter().filter_map(|arg| {
                let clauses = inner_clause_to_elimination_clauses(arg, rel_idp, pvars);
                if clauses.len() > 1 {
                    panic!("Found recursion in non-strictly positive position");
                } else {
                    clauses.first().cloned()
                }
            }).collect::<Vec<_>>();
            
            let conclusion_clauses = inner_clause_to_elimination_clauses(imp.conclusion(), rel_idp, pvars);
            
            conclusion_clauses.into_iter().map(|concl| {
                Implication::create(premise_clauses.clone(), concl)
            }).collect()
        },
        MinlogPredicate::AllQuantifier(all) => {
            let body_clauses = inner_clause_to_elimination_clauses(all.body(), rel_idp, pvars);
            body_clauses.into_iter().map(|body| {
                AllQuantifier::create(all.vars().clone(), body)
            }).collect()
        },
        _ => {
            panic!("Expected formula, but got non-nullary predicate.")
        }
    }
}