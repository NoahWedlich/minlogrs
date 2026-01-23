
use crate::includes::{
    essential::*,
    utils::*,
    core::{
        terms::*,
        predicates::*,
        proofs::*,
    }
};

enum AssumptionOrVariable {
    Assumption(Rc<MinlogProof>),
    Variable(MinlogTerm),
}

pub fn generate_proof_by_assume(target: &Rc<MinlogPredicate>, names: &Vec<String>, context: &ProofContext) -> Rc<MinlogProof> {
    generate_proof_by_assume_with_name(target, names, context, "g0".to_string())
}

pub fn generate_proof_by_assume_with_name(target: &Rc<MinlogPredicate>, names: &Vec<String>, context: &ProofContext, goal_name: String) -> Rc<MinlogProof> {
    println!("Generating proof by assume for target: {}", target.debug_string());
    
    let mut assumptions_and_vars = vec![];
    let mut remaining = target.clone();
    
    if names.is_empty() {
        if !target.is_implication() && !target.is_all_quantifier() {
            return Goal::create(
                goal_name,
                target.clone(),
                context.clone()
            );
        }
        
        let mut assumption_count = 0;
        
        loop {
            if let Some(imp) = remaining.to_implication() {
                for assumption in imp.all_premises().iter() {
                    assumptions_and_vars.push(AssumptionOrVariable::Assumption(
                        Assumption::create(
                            format!("a{}", assumption_count),
                            assumption.clone()
                        )
                    ));
                    assumption_count += 1;
                }
                
                remaining = imp.final_conclusion().clone();
            } else if let Some(all) = remaining.to_all_quantifier() {
                let var = all.var().clone();
                assumptions_and_vars.push(AssumptionOrVariable::Variable(var.clone()));
                remaining = all.body().clone();
            } else {
                break;
            }
        }
    } else {
        for name in names {
            if let Some(imp) = remaining.to_implication() {
                let assumption = Assumption::create(name.clone(), imp.premise().clone());
                assumptions_and_vars.push(AssumptionOrVariable::Assumption(assumption));
                remaining = imp.conclusion().clone();
            } else if let Some(all) = remaining.to_all_quantifier() {
                let var = all.var().clone();
                assumptions_and_vars.push(AssumptionOrVariable::Variable(var.clone()));
                remaining = all.body().clone();
            } else {
                panic!("Couldn't assume {} from target {}", name, target.debug_string());
            }
        }
    }
    
    let mut new_context = context.clone();
    for av in &assumptions_and_vars {
        match av {
            AssumptionOrVariable::Assumption(assump) => {
                new_context.add_assumption(assump.clone());
            },
            AssumptionOrVariable::Variable(var) => {
                new_context.add_variable(var.clone());
            },
        }
    }
    
    let mut proof = Goal::create(
        goal_name,
        remaining,
        new_context
    );
    
    let goal = proof.clone();
    
    for av in assumptions_and_vars.iter().rev() {
        match av {
            AssumptionOrVariable::Assumption(assump) => {
                proof = ImplicationIntro::create(proof, assump.clone());
            },
            AssumptionOrVariable::Variable(var) => {
                proof = UniversalIntro::create(proof, var.clone());
            },
        }
    }
    
    BundledProof::create(proof, "by_assume".to_string(), IndexSet::from([goal]))
}