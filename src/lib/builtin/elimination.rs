
use indexmap::{IndexMap, IndexSet};
use std::rc::Rc;
use crate::core::proofs::axiom::Axiom;
use crate::core::proofs::minlog_proof::MinlogProof;
use crate::utils::pretty_printer::*;

use crate::core::{
    predicates::{
        minlog_predicate::MinlogPredicate,
        predicate_variable::PredicateVariable,
        prime_formula::PrimeFormula,
        implication::Implication,
        all_quantifier::AllQuantifier
    }, terms::{
        term_variable::TermVariable
    }
};

pub fn extract_elimination_axiom(inductive_predicate: &Rc<MinlogPredicate>) -> Rc<MinlogProof> {
    let pvar = PredicateVariable::create(
        "P0".to_string(),
        inductive_predicate.arity().clone(),
    );
    
    let arguments = (0..inductive_predicate.unpacked_arity().len()).map(|i| {
        TermVariable::create(
            format!("x{}", i),
            inductive_predicate.unpacked_arity()[i].clone(),
        )
    }).collect::<Vec<_>>();
    
    let mut elimination_clauses = vec![PrimeFormula::create(inductive_predicate.clone(), arguments.clone())];

    elimination_clauses.extend(
        inductive_predicate.to_inductive_predicate().unwrap().clauses().iter().map(|(_, clause)| {
            clause_to_elimination_clauses(clause, inductive_predicate, &mut IndexMap::new())
        })
    );
    
    let conclusion = PrimeFormula::create(pvar, arguments.clone());
    
    Axiom::create(
        format!("{}Elim", inductive_predicate.to_inductive_predicate().unwrap().name()),
        AllQuantifier::create(
            arguments,
            Implication::create(elimination_clauses, conclusion)
        )
    )
}

fn clause_to_elimination_clauses(
    clause: &Rc<MinlogPredicate>,
    rel_idp: &Rc<MinlogPredicate>,
    pvars: &mut IndexMap<Rc<MinlogPredicate>, Rc<MinlogPredicate>>
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
    pvars: &mut IndexMap<Rc<MinlogPredicate>, Rc<MinlogPredicate>>
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