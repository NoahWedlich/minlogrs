
use std::rc::Rc;
use lib::utils::pretty_printer::*;
use lib::core::types::{type_variable::*, arrow_type::*, star_type::*};

fn main() {
    let type_var = TypeVariable::create("T".to_string());
    let star = StarType::create(vec![Rc::clone(&type_var); 3]);
    let arrow = ArrowType::create(vec![Rc::clone(&star); 4], Rc::clone(&type_var));

    let collapsed = ArrowType::collapse(&arrow);

    println!("{}", collapsed.debug_string());
}