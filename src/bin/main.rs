
use lib::utils::pretty_printer::*;
use lib::core::types::{type_variable::*, algebra_type::*, arrow_type::*, star_type::*};
use lib::core::terms::{minlog_term::*, term_variable::*, constructor::*, program_term::*, abstraction::*, application::*,
    term_substitution::*, tuple::*, projection::*};

use lib::core::structures::{algebra::*, program_constant::*};

fn main() {
    let type_var_1 = TypeVariable::create("T".to_string());
    let type_var_2 = TypeVariable::create("U".to_string());
    
    let star = StarType::create(vec![type_var_2.clone(); 3]);
    let arrow = ArrowType::create(vec![type_var_1.clone(); 2], type_var_1.clone());
    
    println!("Type Variable 1:");
    println!("{}", type_var_1.debug_string());
    println!("Type Variable 2:");
    println!("{}", type_var_2.debug_string());
    println!("Star Type:");
    println!("{}", star.debug_string());
    println!("Arrow Type:");
    println!("{}", arrow.debug_string());
    
    let var_x = TermVariable::create("x".to_string(), type_var_1.clone(), Totality::Total);
    let var_y = TermVariable::create("y".to_string(), type_var_2.clone(), Totality::Total);
    let var_z = TermVariable::create("z".to_string(), type_var_1.clone(), Totality::Total);
    let var_w = TermVariable::create("w".to_string(), type_var_2.clone(), Totality::Total);
    
    let abs = Abstraction::create(vec![var_x.clone(), var_y.clone()], var_x.clone());
    let app = Application::create(abs.clone(), vec![var_z.clone()]);
    
    let tuple = Tuple::create(vec![var_y.clone(), var_w.clone(), var_x.clone()]);
    let proj = Projection::create(tuple.clone(), 1);
    
    println!("Term Variable x:");
    println!("{}", var_x.debug_string());
    println!("Term Variable y:");
    println!("{}", var_y.debug_string());
    println!("Term Variable z:");
    println!("{}", var_z.debug_string());
    println!("Term Variable w:");
    println!("{}", var_w.debug_string());
    
    println!("Abstraction:");
    println!("{}", abs.debug_string());
    println!("Application:");
    println!("{}", app.debug_string());
    println!("Tuple:");
    println!("{}", tuple.debug_string());
    println!("Projection:");
    println!("{}", proj.debug_string());
    
    let normalized_app = app.normalize(true, true);
    println!("Normalized Application:");
    println!("{}", normalized_app.debug_string());
    
    let normalized_proj = proj.normalize(true, true);
    println!("Normalized Projection:");
    println!("{}", normalized_proj.debug_string());
    
    let subst = TermSubstitution::from_pairs(vec![
        (TermSubstEntry::Type(type_var_2.clone()), TermSubstEntry::Type(arrow.clone())),
    ]);
    
    println!("Substitution:");
    println!("{}", subst.debug_string());
    
    let substituted_app = subst.substitute(&TermSubstEntry::Term(app.clone())).to_term().unwrap();
    println!("Substituted Application:");
    println!("{}", substituted_app.debug_string());
    println!("Substituted Application Type:");
    println!("{}", substituted_app.minlog_type().debug_string());
    
    let substituted_normalized_app = subst.substitute(&TermSubstEntry::Term(normalized_app.clone())).to_term().unwrap();
    println!("Substituted Normalized Application:");
    println!("{}", substituted_normalized_app.debug_string());
    println!("Substituted Normalized Application Type:");
    println!("{}", substituted_normalized_app.minlog_type().debug_string());
    
    let nat = Algebra::create("Nat".to_string(), vec![]);
    let nat_type = AlgebraType::create(nat.clone(), vec![]);
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
    
    let list = Algebra::create("List".to_string(), vec![type_var_1.clone()]);
    let list_type = AlgebraType::create(list.clone(), vec![(type_var_1.clone(), type_var_1.clone())]);
    println!("Algebra Type:");
    println!("{}", list_type.debug_string());
    
    let nil = Constructor::create("Nil".to_string(), list_type.clone());
    list.add_constructor(nil);
    
    let cons_type = ArrowType::create(vec![type_var_1.clone(), list_type.clone()], list_type.clone());
    let cons = Constructor::create("Cons".to_string(), cons_type.clone());
    list.add_constructor(cons);
    
    list_type.to_algebra().unwrap().ensure_well_founded();
    
    println!("Algebra:");
    println!("{}", list.debug_string());

    let nat_list_type = AlgebraType::create(list.clone(), vec![(type_var_1.clone(), nat_type.clone())]);
    println!("Nat List Type:");
    println!("{}", nat_list_type.debug_string());
    
    for constructor in nat_list_type.to_algebra().unwrap().constructors() {
        println!("Constructor:");
        println!("{}", constructor.debug_string());
        println!("Constructor Type:");
        println!("{}", constructor.minlog_type().debug_string());
    }
    
    let nat_zero = nat_type.to_algebra().unwrap().constructor(&"Zero".to_string()).unwrap();
    let nat_succ = nat_type.to_algebra().unwrap().constructor(&"Succ".to_string()).unwrap();
    
    let nat_0 = nat_zero.clone();
    let nat_1 = Application::create(nat_succ.clone(), vec![nat_0.clone()]);
    let nat_2 = Application::create(nat_succ.clone(), vec![nat_1.clone()]);
    
    println!("Natural Number 0:");
    println!("{}", nat_0.debug_string());
    println!("Natural Number 1:");
    println!("{}", nat_1.debug_string());
    println!("Natural Number 2:");
    println!("{}", nat_2.debug_string());
    
    let nat_list_nil = nat_list_type.to_algebra().unwrap().constructor(&"Nil".to_string()).unwrap();
    let nat_list_cons = nat_list_type.to_algebra().unwrap().constructor(&"Cons".to_string()).unwrap();
    
    let nat_list_0 = nat_list_nil.clone();
    let nat_list_1 = Application::create(nat_list_cons.clone(), vec![nat_0.clone(), nat_list_0.clone()]);
    let nat_list_2 = Application::create(nat_list_cons.clone(), vec![nat_1.clone(), nat_list_1.clone()]);
    let nat_list_3 = Application::create(nat_list_cons.clone(), vec![nat_2.clone(), nat_list_2.clone()]);
    
    println!("Natural Number List 0:");
    println!("{}", nat_list_0.debug_string());
    println!("Natural Number List 1:");
    println!("{}", nat_list_1.debug_string());
    println!("Natural Number List 2:");
    println!("{}", nat_list_2.debug_string());
    println!("Natural Number List 3:");
    println!("{}", nat_list_3.debug_string());
    
    let nat_add_type = ArrowType::create(vec![nat_type.clone(), nat_type.clone()], nat_type.clone());
    let nat_add = ProgramConstant::create("NatAdd".to_string(), nat_add_type.clone(), Totality::Total, vec![]);
    
    let nat_var_0 = TermVariable::create("m".to_string(), nat_type.clone(), Totality::Total);
    let nat_var_1 = TermVariable::create("n".to_string(), nat_type.clone(), Totality::Total);
    
    let nat_add_term = ProgramTerm::create(nat_add.clone(), vec![]);
    println!("Program Term nat_add:");
    println!("{}", nat_add_term.debug_string());
    
    let nat_add_rule_0 = RewriteRule::create(
        Application::create(nat_add_term.clone(), vec![nat_0.clone(), nat_var_1.clone()]),
        nat_var_1.clone()
    );
    
    let nat_add_rule_succ = RewriteRule::create(
        Application::create(nat_add_term.clone(), vec![
            Application::create(nat_succ.clone(), vec![nat_var_1.clone()]),
            nat_var_0.clone()
        ]),
        Application::create(nat_succ.clone(), vec![
            Application::create(nat_add_term.clone(), vec![nat_var_1.clone(), nat_var_0.clone()])
        ])
    );
    
    nat_add.add_computation_rule(nat_add_rule_0);
    nat_add.add_computation_rule(nat_add_rule_succ);
    
    println!("Program Constant nat_add:");
    println!("{}", nat_add.debug_string());
    
    let mut num_1 = nat_0.clone();
    for _ in 0..3 {
        num_1 = Application::create(nat_succ.clone(), vec![num_1]);
    }
    
    let mut num_2 = nat_0.clone();
    for _ in 0..2 {
        num_2 = Application::create(nat_succ.clone(), vec![num_2]);
    }
    
    let addition = Application::create(nat_add_term.clone(), vec![num_1.clone(), num_2.clone()]);
    println!("Term addition:");
    println!("{}", addition.debug_string());
    
    let normalized_add = addition.normalize(true, true);
    println!("Normalized Term addition:");
    println!("{}", normalized_add.debug_string());
    
    let list_concat_type = ArrowType::create(vec![list_type.clone(), list_type.clone()], list_type.clone());
    let list_concat = ProgramConstant::create("ListConcat".to_string(),
        list_concat_type.clone(), Totality::Total, vec![type_var_1.clone()]);
        
    let list_var_0 = TermVariable::create("l1".to_string(), list_type.clone(), Totality::Total);
    let list_var_1 = TermVariable::create("l2".to_string(), list_type.clone(), Totality::Total);
    
    let list_concat_term = ProgramTerm::create(list_concat.clone(), vec![(type_var_1.clone(), type_var_1.clone())]);
    println!("Program Term list_concat:");
    println!("{}", list_concat_term.debug_string());
    println!("Program Term list_concat Type:");
    println!("{}", list_concat_term.minlog_type().debug_string());
    
    let nil = list_type.to_algebra().unwrap().constructor(&"Nil".to_string()).unwrap();
    let cons = list_type.to_algebra().unwrap().constructor(&"Cons".to_string()).unwrap();
    
    let list_concat_rule_0 = RewriteRule::create(
        Application::create(list_concat_term.clone(), vec![nil.clone(), list_var_1.clone()]),
        list_var_1.clone()
    );
    
    let list_concat_rule_cons = RewriteRule::create(
        Application::create(list_concat_term.clone(), vec![
            Application::create(cons.clone(), vec![var_x.clone(), list_var_0.clone()]),
            list_var_1.clone()
        ]),
        Application::create(cons.clone(), vec![
            var_x.clone(),
            Application::create(list_concat_term.clone(), vec![list_var_0.clone(), list_var_1.clone()])
        ])
    );
    
    list_concat.add_computation_rule(list_concat_rule_0);
    list_concat.add_computation_rule(list_concat_rule_cons);
    
    println!("Program Constant list_concat:");
    println!("{}", list_concat.debug_string());
    
    let nat_list_concat = ProgramTerm::create(list_concat.clone(), vec![(type_var_1.clone(), nat_type.clone())]);
    println!("Program Term nat_list_concat:");
    println!("{}", nat_list_concat.debug_string());
    
    let nat_list_concat = Application::create(nat_list_concat.clone(), vec![nat_list_2.clone(), nat_list_3.clone()]);
    println!("Term nat_list_concat:");
    println!("{}", nat_list_concat.debug_string());
    
    let normalized_nat_list_concat = nat_list_concat.normalize(true, true);
    println!("Normalized Term nat_list_concat:");
    println!("{}", normalized_nat_list_concat.debug_string());
}