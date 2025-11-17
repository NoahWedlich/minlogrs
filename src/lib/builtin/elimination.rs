
use indexmap::{IndexMap, IndexSet};
use std::rc::Rc;
use crate::core::proofs::axiom::Axiom;
use crate::core::proofs::minlog_proof::MinlogProof;
use crate::core::structures::program_constant::{ProgramConstant, RewriteRule};
use crate::utils::pretty_printer::*;

use crate::core::{
    predicates::{
        minlog_predicate::MinlogPredicate,
        predicate_variable::PredicateVariable,
        prime_formula::PrimeFormula,
        implication::Implication,
        all_quantifier::AllQuantifier
    }, terms::{
        minlog_term::MinlogTerm,
        term_variable::TermVariable,
        application::Application,
        program_term::ProgramTerm,
        term_substitution::TermSubstitution
    }
};

pub fn extract_elimination_axiom(idp: &Rc<MinlogPredicate>, elim_axioms: &mut IndexMap<Rc<MinlogPredicate>, Rc<MinlogProof>>) -> Rc<MinlogProof> {
    if let Some(axiom) = elim_axioms.get(idp) {
        return axiom.clone();
    }
    
    let mut clauses = IndexMap::new();
    let mut rel_idps = IndexSet::new();
    idp.to_inductive_predicate().unwrap().collect_relevant_idps(&mut rel_idps);
    
    for ridp in rel_idps.iter() {
        let ridp_clauses = ridp.to_inductive_predicate().unwrap().clauses()
            .into_iter().collect::<IndexMap<_, _>>();
        clauses.insert(ridp.clone(), ridp_clauses);
    }
    
    let pvars = rel_idps.iter().enumerate().map(|(i, ridp)| {
        let pvar = PredicateVariable::create(
            format!("P{}", i),
            ridp.arity().clone(),
        );
        (ridp.clone(), pvar)
    }).collect::<IndexMap<_, _>>();
    
    let mut elimination_clauses = IndexMap::new();
    for ridp in rel_idps.iter() {
        let elim_clauses = clauses.get(ridp).unwrap().iter().map(|(name, clause)| {
            (name.clone(), outer_clause_to_elimination_clause(clause, &pvars))
        }).collect::<IndexMap<_, _>>();
        elimination_clauses.insert(ridp.clone(), elim_clauses);
    }
    
    for ridp in rel_idps.iter() {
        let arguments = (0..ridp.unpacked_arity().len()).map(|i| {
            TermVariable::create(format!("x{}", i), ridp.unpacked_arity()[i].clone())
        }).collect::<Vec<_>>();
        let predicate_clause = PrimeFormula::create(ridp.clone(), arguments.clone());
        
        let conclusion = PrimeFormula::create(
            pvars.get(ridp).unwrap().clone(),
            arguments.clone()
        );
        
        let all_clauses = std::iter::once(predicate_clause)
            .chain(elimination_clauses.values().flat_map(|cls| cls.values().cloned()))
            .collect::<Vec<_>>();
        
        let elim_axiom = Axiom::create(
            format!("{}Elim", ridp.to_inductive_predicate().unwrap().name()),
            AllQuantifier::create(
                arguments,
                Implication::create(all_clauses, conclusion)
            )
        );
        
        elim_axioms.insert(ridp.clone(), elim_axiom);
    }
    
    create_computational_content_for_elimination_axioms(
        &rel_idps,
        &elimination_clauses,
        elim_axioms
    );
    
    elim_axioms.get(idp).unwrap().clone()
}

fn outer_clause_to_elimination_clause(
    clause: &Rc<MinlogPredicate>,
    pvars: &IndexMap<Rc<MinlogPredicate>, Rc<MinlogPredicate>>
) -> Rc<MinlogPredicate> {
    match clause.as_ref() {
        MinlogPredicate::Prime(prime) => {
            if let Some(pvar) = pvars.get(prime.body()) {
                PrimeFormula::create(pvar.clone(), prime.arguments().clone())
            } else {
                panic!("Expected prime formula body to be a relevant idp, found: {}", prime.body().debug_string());
            }
        },
        MinlogPredicate::Implication(imp) => {
            let premises = imp.premises().iter().flat_map(|premise| {
                inner_clause_to_elimination_clause(premise, pvars)
            }).collect::<Vec<_>>();
            
            let conclusion = outer_clause_to_elimination_clause(imp.conclusion(), pvars);
            
            Implication::create(premises, conclusion)
        },
        MinlogPredicate::AllQuantifier(all) => {
            AllQuantifier::create(
                all.vars().clone(),
                outer_clause_to_elimination_clause(all.body(), pvars)
            )
        },
        _ => {
            panic!("Unexpected predicate in elimination clause: {}", clause.debug_string());
        }
    }
}

fn inner_clause_to_elimination_clause(
    clause: &Rc<MinlogPredicate>,
    pvars: &IndexMap<Rc<MinlogPredicate>, Rc<MinlogPredicate>>
) -> Vec<Rc<MinlogPredicate>> {
    match clause.as_ref() {
        MinlogPredicate::Prime(prime) => {
            if let Some(pvar) = pvars.get(prime.body()) {
                vec![
                    clause.clone(),
                    PrimeFormula::create(pvar.clone(), prime.arguments().clone())
                ]
            } else {
                vec![clause.clone()]
            }
        },
        MinlogPredicate::Implication(imp) => {
            let premises = imp.premises().iter().flat_map(|premise| {
                inner_clause_to_elimination_clause(premise, pvars)
            }).collect::<Vec<_>>();
            
            let conclusions = inner_clause_to_elimination_clause(imp.conclusion(), pvars);
            
            conclusions.into_iter().map(|conclusion| {
                Implication::create(premises.clone(), conclusion)
            }).collect::<Vec<_>>()
        },
        MinlogPredicate::AllQuantifier(all) => {
            inner_clause_to_elimination_clause(all.body(), pvars).into_iter().map(|body| {
                AllQuantifier::create(
                    all.vars().clone(),
                    body
                )
            }).collect::<Vec<_>>()
        },
        _ => {
            panic!("Unexpected predicate in elimination clause: {}", clause.debug_string());
        }
    }
}

fn create_computational_content_for_elimination_axioms(
    rel_idps: &IndexSet<Rc<MinlogPredicate>>,
    elimination_clauses: &IndexMap<Rc<MinlogPredicate>, IndexMap<String, Rc<MinlogPredicate>>>,
    elim_axioms: &mut IndexMap<Rc<MinlogPredicate>, Rc<MinlogProof>>
) {
    if rel_idps.iter().any(|ridp| ridp.extracted_type().is_null()) {
        return;
    }
    
    let rel_contents = rel_idps.iter()
        .map(|ridp| {
            (ridp.clone(), ridp.to_inductive_predicate().unwrap().get_computational_content().unwrap())
        }).collect::<IndexMap<_, _>>();
        
    let axiom_pconsts = elim_axioms.iter().map(|(ridp, axiom)| {
        let pconst = ProgramConstant::create(
            format!("{}Elim^et", ridp.to_inductive_predicate().unwrap().name()),
            axiom.proved_formula().extracted_type_pattern(),
        );
        (ridp.clone(), pconst)
    }).collect::<IndexMap<_, _>>();
    
    let mut proof_variables = IndexMap::new();
    for ridp in rel_idps.iter() {
        for (name, clause) in elimination_clauses.get(ridp).unwrap().iter() {
            let constr_name = rel_contents.get(ridp).unwrap().clause_mapping.get(name).unwrap();
            let constr = rel_contents.get(ridp).unwrap().algebra.to_algebra().unwrap().constructor(constr_name).unwrap();
            
            let et_type = clause.extracted_type_pattern();
            
            let pvar = TermVariable::create(
                format!("p{}", proof_variables.len()),
                et_type,
            );
            
            proof_variables.insert(constr.clone(), pvar);
        }
    }
    
    let mut patterns = IndexMap::new();
    
    for ridp in rel_idps.iter() {
        let mut curr_patterns = vec![];
        
        let mut var_index = 0usize;
        for constr in rel_contents.get(ridp).unwrap().algebra.to_algebra().unwrap().constructors() {
            let arg_vars = if let Some(arrow_type) = constr.minlog_type().to_arrow() {
                arrow_type.arguments().iter().map(|arg_type| {
                    let var = TermVariable::create(format!("z{}", var_index), arg_type.clone());
                    var_index += 1;
                    var
                }).collect::<Vec<_>>()
            } else {
                vec![]
            };
            
            curr_patterns.push(
                Application::create(
                    constr.clone(),
                    arg_vars
                )
            );
        }
        
        patterns.insert(ridp.clone(), curr_patterns);
    }
    
    let mut instances = IndexMap::new();
    
    for (redp, pats) in patterns.iter() {
        let mut curr_instances = IndexMap::new();
        
        for pat in pats.iter() {
            match pat.as_ref() {
                MinlogTerm::Application(app) => {
                    let mut new_args = vec![];
                    for arg in app.operands().iter() {
                        if let Some(idp) = rel_idps.iter().find(|ridp| {
                            arg.minlog_type() == ridp.extracted_type_pattern()
                        }) {
                            new_args.push(arg.clone());
                            
                            let pconst = axiom_pconsts.get(idp).unwrap();
                            let pterm = ProgramTerm::create(pconst.clone(), TermSubstitution::make_empty());
                            
                            let mut pterm_args = vec![];
                            pterm_args.push(arg.clone());
                            for pv in proof_variables.values() {
                                pterm_args.push(pv.clone() as Rc<MinlogTerm>);
                            }
                            
                            new_args.push(
                                Application::create(
                                    pterm,
                                    pterm_args
                                )
                            );
                        } else {
                            new_args.push(arg.clone());
                        }
                    }
                    
                    let proof_var = proof_variables.get(app.operator()).unwrap();
                    curr_instances.insert(
                        pat.clone(),
                        Application::create(
                            proof_var.clone(),
                            new_args
                        )
                    );
                },
                MinlogTerm::Constructor(_) => {
                    if let Some(pvar) = proof_variables.get(pat) {
                        curr_instances.insert(pat.clone(), pvar.clone());
                    } else {
                        panic!("No proof variable found for constructor pattern in elimination axiom computational content: {}", pat.debug_string());
                    }
                },
                _ => {
                    panic!("Unexpected pattern term in elimination axiom computational content: {}", pat.debug_string());
                }
            }
        }
        
        instances.insert(redp.clone(), curr_instances);
    }
    
    for ridp in rel_idps.iter() {
        let elim_axiom = elim_axioms.get(ridp).unwrap();
        let pconst = axiom_pconsts.get(ridp).unwrap();
        let pterm = ProgramTerm::create(pconst.clone(), TermSubstitution::make_empty());
        
        for pattern in patterns.get(ridp).unwrap().iter() {
            let instance = instances.get(ridp).unwrap().get(pattern).unwrap();
            let mut arguments = vec![pattern.clone()];
            for pv in proof_variables.values() {
                arguments.push(pv.clone());
            }
            
            let rewrite_rule = RewriteRule::create(
                Application::create(
                    pterm.clone(),
                    arguments
                ),
                instance.clone()
            );

            pconst.add_computation_rule(rewrite_rule);
        }
        
        elim_axiom.to_axiom().unwrap().set_content(
            ProgramTerm::create(pconst.clone(), TermSubstitution::make_empty())
        );
    }
    
    println!("Computational content for elimination axioms created.");
    for ridp in rel_idps.iter() {
        let elim_axiom = elim_axioms.get(ridp).unwrap();
        println!(" - Content for {}: {}", ridp.to_inductive_predicate().unwrap().name(),
            elim_axiom.to_axiom().unwrap().content().unwrap()
                .to_program_term().unwrap().pconst().debug_string()
        );
    }
}