
use std::collections::HashMap;
use lib::builtin::{totality::extract_totality, elimination::extract_elimination_axiom};
use lib::core::predicates::implication::Implication;
use lib::core::predicates::minlog_predicate::PredicateDegree;
use lib::core::predicates::predicate_substitution::PredicateSubstitution;
use lib::core::predicates::predicate_variable::PredicateVariable;
use lib::core::predicates::inductive_predicate::InductivePredicate;
use lib::core::structures::inductive_constant::InductiveConstant;
use lib::core::types::type_substitution::TypeSubstitution;
use lib::core::types::type_variable::TypeVariable;
use lib::utils::pretty_printer::*;
use lib::utils::proof_tree_display::*;
use lib::core::types::{algebra_type::*, arrow_type::*};
use lib::core::terms::constructor::*;

use lib::core::structures::{algebra::*};

fn main() {
    
    let nat = Algebra::create("Nat".to_string());
    let nat_type = AlgebraType::create(nat.clone(), TypeSubstitution::make_empty());
    println!("Algebra Type:");
    println!("{}", nat_type.debug_string());
    
    let zero = Constructor::create("Zero".to_string(), nat_type.clone());
    nat.add_constructor(zero);
    
    let succ_type = ArrowType::create(vec![nat_type.clone()], nat_type.clone());
    let succ = Constructor::create("Succ".to_string(), succ_type.clone());
    nat.add_constructor(succ);
    
    nat_type.to_algebra().unwrap().ensure_well_founded();

    println!("Algebra:");
    println!("{}", nat.debug_string());
    
    let nat_total = extract_totality(&nat, &mut HashMap::new());
    println!("Totality Predicate:");
    println!("{}", nat_total.to_inductive_predicate().unwrap().definition().debug_string());
    
    let nat_total_elim = extract_elimination_axiom(&nat_total);
    println!("Totality Elimination Axiom:");
    println!("{}", nat_total_elim.debug_string());
    println!("{}", nat_total_elim.render_proof_tree());
    
    let tvar = TypeVariable::create("T".to_string());
    let pred_var_1 = PredicateVariable::create("P".to_string(), tvar.clone(), PredicateDegree {
        positive_content: false, negative_content: false
    });
    let pred_var_2 = PredicateVariable::create("Q".to_string(), tvar.clone(), PredicateDegree {
        positive_content: false, negative_content: false
    });
    
    let and_def = InductiveConstant::create("and".to_string(), tvar.clone());
    let and_pred = InductivePredicate::create(and_def.clone(), PredicateSubstitution::make_empty());
    
    println!("And Predicate:");
    println!("{}", and_pred.debug_string());
    
    let and_clause = Implication::create(
        vec![pred_var_1.clone(), pred_var_2.clone()],
        and_pred.clone()
    );
    and_def.add_clause("AndInit".to_string(), and_clause);
    
    and_pred.to_inductive_predicate().unwrap().ensure_well_founded();
    
    println!("And Constant:");
    println!("{}", and_def.debug_string());
    
    let and_alg = Algebra::create("AndAlg".to_string());
    and_def.make_computational(and_alg, false);
    
    println!("And Algebra:");
    println!("{}", and_pred.to_inductive_predicate().unwrap().get_algebra().unwrap()
        .to_algebra().unwrap().algebra().debug_string());
    
    println!("And ET-Type:");
    println!("{}", and_pred.extracted_type().debug_string());
    
    
}