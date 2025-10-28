
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
    
    let ord = Algebra::create("Ord".to_string());
    let ord_type = AlgebraType::create(ord.clone(), TypeSubstitution::make_empty());
    println!("Ordinal Type:");
    println!("{}", ord_type.debug_string());
    
    let zero_ord = Constructor::create("Zero".to_string(), ord_type.clone());
    ord.add_constructor(zero_ord);
    
    let succ_ord_type = ArrowType::create(vec![ord_type.clone()], ord_type.clone());
    let succ_ord = Constructor::create("Succ".to_string(), succ_ord_type.clone());
    ord.add_constructor(succ_ord);
    
    let lim_ord_type = ArrowType::create(
        vec![ArrowType::create(vec![nat_type.clone()], ord_type.clone())],
        ord_type.clone()
    );
    let lim_ord = Constructor::create("Lim".to_string(), lim_ord_type.clone());
    ord.add_constructor(lim_ord);
    
    ord_type.to_algebra().unwrap().ensure_well_founded();
    
    println!("Ordinal Algebra:");
    println!("{}", ord.debug_string());
    
    let ord_total = extract_totality(&ord, &mut HashMap::new());
    println!("Ordinal Totality Predicate:");
    println!("{}", ord_total.to_inductive_predicate().unwrap().definition().debug_string());
}