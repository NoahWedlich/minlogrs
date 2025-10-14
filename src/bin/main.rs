
use std::rc::Rc;
use lib::utils::pretty_printer::*;
use lib::core::types::{type_variable::*, arrow_type::*, star_type::*, type_substitution::*};
use lib::core::terms::{minlog_term::*, term_variable::*, abstraction::*, application::*, term_substitution::*, tuple::*, projection::*};

fn main() {
    let type_var_1 = TypeVariable::create("T".to_string());
    let type_var_2 = TypeVariable::create("U".to_string());
    
    let star = StarType::create(vec![type_var_2.clone(); 3]);
    let arrow = ArrowType::create(vec![type_var_1.clone(); 2], type_var_1.clone());
    
    println!("Type Variable 1: {}", type_var_1.debug_string());
    println!("Type Variable 2: {}", type_var_2.debug_string());
    println!("Star Type: {}", star.debug_string());
    println!("Arrow Type: {}", arrow.debug_string());
    
    let var_x = TermVariable::create("x".to_string(), type_var_1.clone(), Totality::Total);
    let var_y = TermVariable::create("y".to_string(), type_var_2.clone(), Totality::Total);
    let var_z = TermVariable::create("z".to_string(), type_var_1.clone(), Totality::Total);
    let var_w = TermVariable::create("w".to_string(), type_var_2.clone(), Totality::Total);
    
    let abs = Abstraction::create(vec![var_x.clone(), var_y.clone()], var_x.clone());
    let app = Application::create(abs.clone(), vec![var_z.clone()]);
    
    let tuple = Tuple::create(vec![var_y.clone(), var_w.clone(), var_x.clone()]);
    let proj = Projection::create(tuple.clone(), 1);
    
    println!("Term Variable x: {}", var_x.debug_string());
    println!("Term Variable y: {}", var_y.debug_string());
    println!("Term Variable z: {}", var_z.debug_string());
    println!("Term Variable w: {}", var_w.debug_string());
    
    println!("Abstraction: {}", abs.debug_string());
    println!("Application: {}", app.debug_string());
    println!("Tuple: {}", tuple.debug_string());
    println!("Projection: {}", proj.debug_string());
    
    let normalized_app = app.normalize(true, true);
    println!("Normalized Application: {}", normalized_app.debug_string());
    
    let normalized_proj = proj.normalize(true, true);
    println!("Normalized Projection: {}", normalized_proj.debug_string());
    
    let subst = TermSubstitution::from_pairs(vec![
        (TermSubstEntry::Type(type_var_2.clone()), TermSubstEntry::Type(arrow.clone())),
    ]);
    
    println!("Substitution: {}", subst.debug_string());
    
    let substituted_app = subst.substitute(&TermSubstEntry::Term(app.clone())).to_term().unwrap();
    println!("Substituted Application: {}", substituted_app.debug_string());
    println!("Substituted Application Type: {}", substituted_app.minlog_type().debug_string());
    
    let substituted_normalized_app = subst.substitute(&TermSubstEntry::Term(normalized_app.clone())).to_term().unwrap();
    println!("Substituted Normalized Application: {}", substituted_normalized_app.debug_string());
    println!("Substituted Normalized Application Type: {}", substituted_normalized_app.minlog_type().debug_string());
}