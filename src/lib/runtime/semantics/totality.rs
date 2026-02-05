
pub use crate::includes::{
    essential::*,
    kernel::{
        structures::*,
        types::*,
        terms::*,
        predicates::*,
    }
};

pub fn extract_totality(algebra: &Arc<MinlogType>, totalities: &mut IndexMap<Arc<MinlogType>, Arc<MinlogPredicate>>) -> Arc<MinlogPredicate> {
    if !algebra.is_algebra() {
        panic!("extract_totality called with a non-algebra type");
    }
    
    if let Some(pred) = totalities.get(algebra) {
        pred.clone()
    } else {
        let totality_def = InductiveConstant::create(
            format!("{}Total", algebra.to_algebra().unwrap().name()),
            algebra.clone()
        );
        
        let totality_pred = InductivePredicate::create(
            totality_def.clone(), 
            PredicateSubstitution::make_empty()
        );
        
        totalities.insert(algebra.clone(), totality_pred.clone());
        
        for constructor in algebra.to_algebra().unwrap().constructors() {
            let clause = constructor_to_totality_clause(&constructor, totalities);
            totality_def.add_clause(format!("{}Total", constructor.to_constructor().unwrap().name()), clause);
        }
        
        totality_def.register_computational_content(algebra.clone());
        totality_def.make_computational(true);
        
        totality_pred
    }
}

fn constructor_to_totality_clause(
    constructor: &MinlogTerm,
    totalities: &mut IndexMap<Arc<MinlogType>, Arc<MinlogPredicate>>
) -> Arc<MinlogPredicate> {
    if !constructor.is_constructor() {
        panic!("constructor_to_totality_clause called with a non-constructor term");
    }
    
    match constructor.minlog_type().as_ref() {
        MinlogType::Algebra(_) => {
            let totality_pred = extract_totality(&constructor.minlog_type(), totalities);
            PrimeFormula::create(totality_pred, constructor.clone())
        },
        MinlogType::Arrow(arrow_type) => {
            let mut var_index = 0usize;
            
            let vars = arrow_type.all_arguments().iter().map(|arg_type| {
                let var = TermVariable::create(format!("v{}", var_index), arg_type.clone());
                var_index += 1;
                var
            }).collect::<Vec<_>>();
            
            let var_clauses = vars.iter().filter_map(|var| {
                term_to_totality_condition(var.clone(), totalities, &mut var_index)
            }).collect::<Vec<_>>();

            let value = Application::create_nested(
                constructor.clone(),
                vars.iter().map(|v| v.clone() as MinlogTerm).collect()
            );
            
            if let Some(totality_pred) = totalities.get(&value.minlog_type()) {
                let value_clause = PrimeFormula::create(totality_pred.clone(), value);
                
                if var_clauses.is_empty() {
                    AllQuantifier::closure(&value_clause)
                } else {
                    AllQuantifier::closure(
                        &Implication::create_nested(var_clauses, value_clause)
                    )
                }
            } else {
                panic!("No totality predicate found for arrow type return type");
            }
        },
        _ => {
            panic!("Unexpected constructor type in constructor_to_totality_clause");
        }
    }
}

fn term_to_totality_condition(
    term: MinlogTerm,
    totalities: &mut IndexMap<Arc<MinlogType>, Arc<MinlogPredicate>>,
    var_index: &mut usize
) -> Option<Arc<MinlogPredicate>> {
    match term.minlog_type().as_ref() {
        MinlogType::Variable(_) => {
            if let Some(totality) = totalities.get(&term.minlog_type()) {
                Some(PrimeFormula::create(totality.clone(), term))
            } else {
                panic!("No totality predicate found for type variable");
            }
        },
        MinlogType::Algebra(_) => {
            let totality = extract_totality(&term.minlog_type(), totalities);
            Some(PrimeFormula::create(totality, term))
        },
        MinlogType::Arrow(arrow_type) => {
            let argument_vars = arrow_type.all_arguments().iter().map(|arg_type| {
                let var = TermVariable::create(format!("v{}", var_index), arg_type.clone());
                *var_index += 1;
                var
            }).collect::<Vec<_>>();
            
            let argument_clauses = argument_vars.iter().filter_map(|arg_var| {
                term_to_totality_condition(arg_var.clone(), totalities, var_index)
            }).collect::<Vec<_>>();
            
            let value = Application::create_nested(
                term.clone(),
                argument_vars.iter().map(|v| v.clone() as MinlogTerm).collect()
            );
            
            if let Some(totality) = totalities.get(&value.minlog_type()) {
                let value_clause = PrimeFormula::create(totality.clone(), value);
                
                if argument_clauses.is_empty() {
                    Some(value_clause)
                } else {
                    Some(AllQuantifier::create_nested(
                        argument_vars,
                        Implication::create_nested(argument_clauses, value_clause)
                    ))
                }
            } else {
                panic!("No totality predicate found for arrow type return type");
            }
        },
        MinlogType::Pair(_) => {
            let conditions = vec![true, false].into_iter().filter_map(|left| {
                let proj = Projection::create(term.clone(), left);
                term_to_totality_condition(proj, totalities, var_index)
            }).collect::<Vec<_>>();
            
            if conditions.is_empty() {
                None
            } else if conditions.len() == 1 {
                Some(conditions.into_iter().next().unwrap())
            } else {
                Some(Implication::create_nested(
                    conditions[..conditions.len()-1].to_vec(), 
                    conditions[conditions.len()-1].clone()
                ))
            }
        },
        _ => None,
    }
}