
use indexmap::{IndexMap, IndexSet};
use std::rc::Rc;
use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};

use crate::core::substitution::MatchOutput;

use crate::core::types::minlog_type::MinlogType;
use crate::core::types::arrow_type::ArrowType;

use crate::core::terms::minlog_term::{TermBody, MinlogTerm};
use crate::core::terms::term_variable::TermVariable;

use crate::core::terms::term_substitution::{TermSubstEntry, TermSubstitution};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Abstraction {
    vars: Vec<Rc<MinlogTerm>>,
    kernel: Rc<MinlogTerm>,
    minlog_type: Rc<MinlogType>,
}

impl Abstraction {
    pub fn create(vars: Vec<Rc<MinlogTerm>>, kernel: Rc<MinlogTerm>) -> Rc<MinlogTerm> {
        if vars.is_empty() {
            return kernel;
        }
        
        if vars.iter().any(|v| !v.is_variable()) {
            panic!("Tried to create an Abstraction with a non-variable term");
        }
        
        for (i, var) in vars.iter().enumerate() {
            for other in &vars[(i+1)..] {
                if var == other {
                    panic!("Tried to create an Abstraction with duplicate variables");
                }
            }
        }
        
        let var_types: Vec<Rc<MinlogType>> = vars.iter().map(|v| v.minlog_type()).collect();
        Abstraction::collapse(&Rc::new(MinlogTerm::Abstraction(Abstraction {
            minlog_type: ArrowType::create(var_types, kernel.minlog_type()),
            vars,
            kernel,
        })))
    }
    
    pub fn collapse(minlog_term: &Rc<MinlogTerm>) -> Rc<MinlogTerm> {
        if !minlog_term.is_abstraction() || !minlog_term.to_abstraction().unwrap().kernel.is_abstraction() {
            Rc::clone(minlog_term)
        } else {
            let mut abstraction = minlog_term.to_abstraction().unwrap();
            let mut vars = abstraction.vars().clone();
            
            while abstraction.kernel().is_abstraction() {
                let next_abstraction = abstraction.kernel().to_abstraction().unwrap();
                vars.extend(next_abstraction.vars().clone());
                abstraction = next_abstraction;
            }
            
            Abstraction::create(vars, Rc::clone(abstraction.kernel()))
        }
    }
    
    pub fn closure(minlog_term: &Rc<MinlogTerm>) -> Rc<MinlogTerm> {
        let mut vars = vec![];
        
        for var in minlog_term.get_free_variables(&mut IndexSet::new()) {
            if var.is_variable() && !vars.contains(&var) {
                vars.push(var);
            }
        }
        
        Abstraction::create(vars, minlog_term.clone())
    }
    
    pub fn arity(&self) -> usize {
        self.vars.len()
    }
    
    pub fn vars(&self) -> &Vec<Rc<MinlogTerm>> {
        &self.vars
    }
    
    pub fn var(&self, index: usize) -> Option<&Rc<MinlogTerm>> {
        self.vars.get(index)
    }
    
    pub fn kernel(&self) -> &Rc<MinlogTerm> {
        &self.kernel
    }
}

impl TermBody for Abstraction {
    fn minlog_type(&self) -> Rc<MinlogType> {
        self.minlog_type.clone()
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> Rc<MinlogTerm> {
        let kernel = self.kernel.normalize(eta, pi);
        
        let mut vars = vec![];
        
        if eta {
            let free_vars = kernel.get_free_variables(&mut IndexSet::new());
            for var in &self.vars {
                if free_vars.contains(var) {
                    vars.push(var.clone());
                }
            }
        } else {
            vars = self.vars.clone();
        }

        if vars.is_empty() {
            kernel.normalize(eta, pi)
        } else {
            Abstraction::create(vars, kernel.normalize(eta, pi))
        }
    }
    
    fn remove_nulls(&self) -> Option<Rc<MinlogTerm>> {
        let new_vars = self.vars.iter()
            .filter_map(|v| v.remove_nulls())
            .collect::<Vec<_>>();
        
        self.kernel.remove_nulls().map(|k|
            Abstraction::create(new_vars, k)
        )
    }
    
    fn length(&self) -> usize {
        1 + self.kernel.length()
    }
    
    fn depth(&self) -> usize {
        1 + self.kernel.depth()
    }
    
    fn get_type_variables(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogType>> {
        self.minlog_type.get_type_variables(&mut IndexSet::new())
    }
    
    fn get_algebra_types(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogType>> {
        self.minlog_type.get_algebra_types(&mut IndexSet::new())
    }
    
    fn get_free_variables(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogTerm>> {
        self.kernel.get_free_variables(visited).into_iter()
            .filter(|v| !self.vars.contains(v))
            .collect()
    }
    
    fn get_bound_variables(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogTerm>> {
        self.kernel.get_bound_variables(visited).union(&self.vars.iter().cloned().collect::<IndexSet<_>>()).cloned().collect()
    }
    
    fn get_constructors(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogTerm>> {
        self.kernel.get_constructors(visited)
    }
    
    fn get_program_terms(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogTerm>> {
        self.kernel.get_program_terms(visited)
    }
    
    fn get_internal_constants(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogTerm>> {
        self.kernel.get_internal_constants(visited)
    }
    
    fn alpha_equivalent(&self, other: &Rc<MinlogTerm>,
        forward: &mut Vec<(TermVariable, TermVariable)>,
        backward: &mut Vec<(TermVariable, TermVariable)>) -> bool {
        
        if !other.is_abstraction() {
            return false;
        }
        
        let other = other.to_abstraction().unwrap();
        
        if self.vars.len() != other.vars.len() {
            return false;
        }
        
        for (v1, v2) in self.vars.iter().zip(other.vars.iter()) {
            if v1.minlog_type() != v2.minlog_type() {
                return false;
            }
            
            forward.push((v1.to_variable().unwrap().clone(), v2.to_variable().unwrap().clone()));
            backward.push((v2.to_variable().unwrap().clone(), v1.to_variable().unwrap().clone()));
        }
        
        self.kernel.alpha_equivalent(&other.kernel, forward, backward)
    }

    fn substitute(&self, from: &TermSubstEntry, to: &TermSubstEntry) -> Rc<MinlogTerm> {
        if let Some(from_tm) = from.to_term() {
            if from_tm.is_abstraction() && self == from_tm.to_abstraction().unwrap() {
                to.to_term().unwrap()
            } else if from_tm.is_variable() && self.vars.contains(&from_tm) {
                Rc::new(MinlogTerm::Abstraction(self.clone()))
            } else {
                let new_kernel = self.kernel.substitute(from, to);
                Abstraction::create(self.vars.clone(), new_kernel)
            }
        } else {
            let new_vars = self.vars.iter()
                .map(|v| v.substitute(from, to)).collect::<Vec<_>>();
            
            let new_kernel = self.kernel.substitute(from, to);
            
            Abstraction::create(new_vars, new_kernel)
        }
    }
    
    fn first_conflict_with(&self, other: &Rc<MinlogTerm>) -> Option<(TermSubstEntry, TermSubstEntry)> {
        if let Some(conflict) = self.minlog_type.first_conflict_with(&other.minlog_type()) {
            return Some((conflict.0.into(), conflict.1.into()));
        }
        
        if !other.is_abstraction() {
            return Some((Rc::new(MinlogTerm::Abstraction(self.clone())).into(), Rc::clone(other).into()));
        }
        
        let other_abs = other.to_abstraction().unwrap();

        if self.vars.len() != other_abs.vars.len() {
            return Some((Rc::new(MinlogTerm::Abstraction(self.clone())).into(), other.clone().into()));
        }
        
        let mut subst = TermSubstitution::make_empty();
        
        for (v1, v2) in self.vars.iter().zip(other_abs.vars.iter()) {
            if v1 == v2 {
                continue;
            } else if v1.minlog_type() == v2.minlog_type() {
                subst.extend((TermSubstEntry::Term(v2.clone()), TermSubstEntry::Term(v1.clone())));
            } else {
                return Some((v1.clone().into(), v2.clone().into()));
            }
        }
        
        let new_other = subst.substitute(&TermSubstEntry::Term(other.clone()));
        
        if let TermSubstEntry::Term(t) = new_other {
            self.kernel.first_conflict_with(&t.to_abstraction().unwrap().kernel)
        } else {
            panic!("Substitution of abstraction resulted in type.");
        }
    }

    fn match_with(&self, instance: &Rc<MinlogTerm>) -> MatchOutput<TermSubstEntry> {
        if !instance.is_abstraction() {
            return MatchOutput::FailedMatch;
        }
        
        let abs_instance = instance.to_abstraction().unwrap();
        
        if self.arity() != abs_instance.arity() {
            return MatchOutput::FailedMatch;
        }
        
        let mut conditions = self.vars.iter().zip(abs_instance.vars.iter())
            .filter_map(|(v1, v2)| {
                if v1 != v2 {
                    Some((v1.into(), v2.into()))
                } else {
                    None
                }
            }).collect::<IndexMap<_, _>>();
            
        if self.kernel != abs_instance.kernel {
            conditions.insert(self.kernel.clone().into(), abs_instance.kernel.clone().into());
        }
        
        MatchOutput::Matched(conditions)
    }
}

impl PrettyPrintable for Abstraction {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        if self.vars.is_empty() {
            return self.kernel.to_pp_element(detail);
        }
        
        let variables = PPElement::list(
            self.vars.iter().map(|v| v.to_pp_element(detail)).collect(),
            PPElement::break_elem(0, 4, false),
            PPElement::text(",".to_string()),
            PPElement::break_elem(1, 4, false),
            BreakType::Flexible
        );
        
        let elements = vec![
            PPElement::group(vec![
                PPElement::text("[".to_string()),
                PPElement::break_elem(1, 4, false),
                variables,
                PPElement::break_elem(1, 0, false),
                PPElement::text("]".to_string())
            ], BreakType::Consistent, 0),
            PPElement::break_elem(1, 4, false),
            PPElement::text("->".to_string()),
            PPElement::break_elem(1, 4, false),
            self.kernel.to_enclosed_pp_element(detail)
        ];
        
        PPElement::group(elements, BreakType::Flexible, 0)
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        true
    }
    
    fn open_paren(&self) -> String {
        "(".to_string()
    }
    
    fn close_paren(&self) -> String {
        ")".to_string()
    }
}