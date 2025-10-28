
use core::panic;
use std::{rc::Rc, collections::HashMap};

use crate::core::{
    formulas::{
        all_quantifier::AllQuantifier, implication::Implication, minlog_formula::MinlogFormula, prime_formula::PrimeFormula
    }, predicates::{
        inductive_predicate::InductivePredicate,
        minlog_predicate::MinlogPredicate,
        predicate_substitution::PredicateSubstitution
    }, structures::{
        algebra::Algebra,
        inductive_constant::InductiveConstant
    }, terms::{
        minlog_term::{MinlogTerm, Totality}, term_variable::TermVariable, application::Application
    }, types::{
        algebra_type::AlgebraType, minlog_type::MinlogType, type_substitution::TypeSubstitution
    }
};

pub fn extract_totality(algebra: &Rc<Algebra>, totalities: &mut HashMap<Rc<MinlogType>, Rc<MinlogPredicate>>) -> Rc<MinlogPredicate> {
    let alg_type = AlgebraType::create(algebra.clone(), TypeSubstitution::make_empty());
    
    if let Some(pred) = totalities.get(&alg_type) {
        pred.clone()
    } else {
        let totality_def = InductiveConstant::create("Total".to_string(), vec![alg_type.clone()]);
        
        let totality_pred = InductivePredicate::create(
            totality_def.clone(), 
            PredicateSubstitution::make_empty()
        );
        
        totalities.insert(alg_type.clone(), totality_pred.clone());
        
        for constructor in algebra.constructors() {
            let clause = constructor_to_totality_clause(constructor.clone(), totalities);
            totality_def.add_clause(format!("{}Total", constructor.to_constructor().unwrap().name()), clause);
        }
        
        totality_def.make_computational(algebra.clone(), true);
        
        totality_pred
    }
}

fn constructor_to_totality_clause(
    constructor: Rc<MinlogTerm>,
    totalities: &mut HashMap<Rc<MinlogType>, Rc<MinlogPredicate>>
) -> Rc<MinlogFormula> {
    if !constructor.is_constructor() {
        panic!("constructor_to_totality_clause called with a non-constructor term");
    }
    
    match constructor.minlog_type().as_ref() {
        MinlogType::Algebra(alg_type) => {
            let totality_pred = extract_totality(alg_type.algebra(), totalities);
            PrimeFormula::create(totality_pred, vec![constructor.clone()])
        },
        MinlogType::Arrow(arrow_type) => {
            let mut var_index = 0usize;
            
            let vars = arrow_type.arguments().iter().map(|arg_type| {
                let var = TermVariable::create(format!("v{}", var_index), arg_type.clone(), Totality::Partial);
                var_index += 1;
                var
            }).collect::<Vec<_>>();
            
            let var_clauses = vars.iter().filter_map(|var| {
                var_to_totality_condition(var.clone(), totalities, &mut var_index)
            }).collect::<Vec<_>>();

            let value = Application::create(
                constructor.clone(),
                vars.iter().map(|v| v.clone() as Rc<MinlogTerm>).collect()
            );
            
            if let Some(totality_pred) = totalities.get(&value.minlog_type()) {
                let value_clause = PrimeFormula::create(totality_pred.clone(), vec![value]);
                
                if var_clauses.is_empty() {
                    AllQuantifier::closure(&value_clause)
                } else {
                    AllQuantifier::closure(
                        &Implication::create(var_clauses, value_clause)
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

fn var_to_totality_condition(
    var: Rc<MinlogTerm>,
    totalities: &mut HashMap<Rc<MinlogType>, Rc<MinlogPredicate>>,
    var_index: &mut usize
) -> Option<Rc<MinlogFormula>> {
    match var.minlog_type().as_ref() {
        MinlogType::Variable(_) => {
            if let Some(totality) = totalities.get(&var.minlog_type()) {
                Some(PrimeFormula::create(totality.clone(), vec![var]))
            } else {
                panic!("No totality predicate found for type variable");
            }
        },
        MinlogType::Algebra(alg_type) => {
            let totality = extract_totality(alg_type.algebra(), totalities);
            Some(PrimeFormula::create(totality, vec![var]))
        },
        MinlogType::Arrow(arrow_type) => {
            let argument_vars = arrow_type.arguments().iter().map(|arg_type| {
                let var = TermVariable::create(format!("v{}", var_index), arg_type.clone(), Totality::Partial);
                *var_index += 1;
                var
            }).collect::<Vec<_>>();
            
            let argument_clauses = argument_vars.iter().filter_map(|arg_var| {
                var_to_totality_condition(arg_var.clone(), totalities, var_index)
            }).collect::<Vec<_>>();
            
            let value = Application::create(
                var.clone(),
                argument_vars.iter().map(|v| v.clone() as Rc<MinlogTerm>).collect()
            );
            
            if let Some(totality) = totalities.get(&value.minlog_type()) {
                let value_clause = PrimeFormula::create(totality.clone(), vec![value]);
                
                if argument_clauses.is_empty() {
                    Some(value_clause)
                } else {
                    Some(AllQuantifier::create(
                        argument_vars,
                        Implication::create(argument_clauses, value_clause)
                    ))
                }
            } else {
                panic!("No totality predicate found for arrow type return type");
            }
        },
        MinlogType::Star(_) => {
            panic!("Star types are not supported, as there are no conjunctions yet");
        },
        _ => None,
    }
}