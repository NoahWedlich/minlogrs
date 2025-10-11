
use std::rc::Rc;
use lib::utils::pretty_printer::*;
use lib::core::substitution::*;
use lib::core::types::{type_variable::*, arrow_type::*, star_type::*, type_substitution::*};

fn main() {
    let type_var_1 = TypeVariable::create("T".to_string());
    let type_var_2 = TypeVariable::create("U".to_string());
    
    let star = StarType::create(vec![Rc::clone(&type_var_2); 3]);
    let arrow = ArrowType::create(vec![Rc::clone(&type_var_1); 2], Rc::clone(&type_var_1));
    
    let substitution = TypeSubstitution::from_pairs(vec![
        (Rc::clone(&type_var_1), Rc::clone(&star)),
        (Rc::clone(&type_var_2), Rc::clone(&arrow)),
    ]);
    
    println!("Type Variable 1: {}", type_var_1.debug_string());
    println!("Type Variable 2: {}", type_var_2.debug_string());
    println!("Star Type: {}", star.debug_string());
    println!("Arrow Type: {}", arrow.debug_string());
    
    println!("{}", substitution.debug_string());
    
    let substituted_arrow = substitution.substitute(&arrow);
    println!("Substituted Arrow Type: {}", substituted_arrow.debug_string());

    let substituted_star = substitution.substitute(&star);
    println!("Substituted Star Type: {}", substituted_star.debug_string());
}