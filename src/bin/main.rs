
use std::collections::HashMap;
use lib::builtin::totality::extract_totality;
use lib::core::types::type_substitution::TypeSubstitution;
use lib::utils::pretty_printer::*;
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
}