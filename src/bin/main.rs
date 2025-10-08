
use lib::core::substitution::Substitution;
use lib::core::types::arrow_type::ArrowType;
use lib::core::types::star_type::StarType;
use lib::utils::indirect_ref::RefGroup;
use lib::core::types::type_variable::TypeVariable;
use lib::core::types::type_substitution::TypeSubstitution;

fn main() {
    let group = RefGroup::new();
    let tvar1 = TypeVariable::create("T", &group);
    let tvar2 = TypeVariable::create("U", &group);
    let tvar3 = TypeVariable::create("V", &group);

    println!("Type Variable T: {:?}", tvar1);
    println!("Type Variable U: {:?}", tvar2);
    println!("Type Variable V: {:?}", tvar3);

    let type1 = ArrowType::create(vec![tvar1.clone()], tvar2.clone(), &group);
    
    println!("Type 1: {:?}", type1);
    
    let mut substitution = TypeSubstitution::make_empty();
    substitution.extend((tvar1.clone(), tvar2.clone()));
    
    println!("Substitution: {:?}", substitution);
    
    let substituted_type = substitution.substitute(&type1);
    println!("Substituted Type 1: {:?}", substituted_type);
    
    substitution.extend((tvar2.clone(), tvar3.clone()));
    println!("Extended Substitution: {:?}", substitution);
    
    let substituted_type2 = substitution.substitute(&substituted_type);
    println!("Substituted Type 2: {:?}", substituted_type2);
}