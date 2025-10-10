
use std::rc::Rc;
use lib::utils::pretty_printer::*;
use lib::core::types::{type_variable::*, arrow_type::*};

fn main() {
    let type_var = TypeVariable::create("T".to_string());
    let inner_arrow = ArrowType::create(vec![Rc::clone(&type_var); 3], Rc::clone(&type_var));
    let arrow = ArrowType::create(vec![Rc::clone(&inner_arrow); 4], Rc::clone(&type_var));
    let outer_arrow = ArrowType::create(vec![Rc::clone(&arrow); 2], Rc::clone(&type_var));

    let collapsed = ArrowType::collapse(&outer_arrow);

    println!("{}", collapsed.debug_string());
}