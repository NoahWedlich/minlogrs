
use std::{rc::Rc, cell::RefCell};
use crate::utils::pretty_printer::*;

use crate::core::substitution::SubstitutableWith;

use crate::core::types::minlog_type::MinlogType;

use crate::core::terms::minlog_term::{MinlogTerm, TermBody, Totality};

use crate::core::terms::term_substitution::{TermSubstitution, TermSubstEntry};

#[derive(Clone, PartialEq, Eq)]
pub struct RewriteRule {
    pattern: Rc<MinlogTerm>,
    result: Rc<MinlogTerm>,
}

impl RewriteRule {
    pub fn create(pattern: Rc<MinlogTerm>, result: Rc<MinlogTerm>) -> Rc<RewriteRule> {
        if let Some(pc) = pattern.to_program_term() {
            if pc.minlog_type().arity() != 0 {
                panic!("Rewrite rule pattern must be a full application of a program constant");
            }
            
            if result.minlog_type() != pattern.minlog_type() {
                panic!("Rewrite rule result type must match the program constant type");
            }
        } else if let Some(app) = pattern.to_application() {
            if !app.operator().is_program_term() {
                panic!("Rewrite rule must either be a program constant or a full application of a program constant");
            }
            
            let pc = app.operator().to_program_term().unwrap();
            
            if app.operands().len() != pc.minlog_type().arity() {
                panic!("Rewrite rule pattern must be a full application of the program constant");
            }
            
            for (op, arg) in app.operands().iter().zip(pc.minlog_type().to_arrow().unwrap().arguments()) {
                if op.minlog_type() != *arg {
                    panic!("Rewrite rule pattern operand types must match the program constant argument types");
                }
            }
        } else {
            panic!("Rewrite rule must either be a program constant or a full application of a program constant");
        }
        
        let pattern_free_vars = MinlogTerm::get_free_variables(&pattern);
        for free_var in MinlogTerm::get_free_variables(&result) {
            if !pattern_free_vars.contains(&free_var) {
                panic!("Rewrite rule result contains free variables not present in the pattern");
            }
        }
        
        Rc::new(RewriteRule { pattern, result })
    }
    
    pub fn pattern(&self) -> Rc<MinlogTerm> {
        self.pattern.clone()
    }
    
    pub fn result(&self) -> Rc<MinlogTerm> {
        self.result.clone()
    }
    
    pub fn program_constant(&self) -> Rc<ProgramConstant> {
        if let Some(pc) = self.pattern.to_program_term() {
            pc.pconst().clone()
        } else if let Some(app) = self.pattern.to_application() {
            app.operator().to_program_term().unwrap().pconst().clone()
        } else {
            panic!("Rewrite rule must either be a program constant or a full application of a program constant");
        }
    }
    
    pub fn arity(&self) -> usize {
        self.pattern.minlog_type().arity()
    }
    
    pub fn is_left_linear(term: &Rc<MinlogTerm>, bound: &mut Vec<Rc<MinlogTerm>>) -> bool {
        if !term.constructor_pattern() {
            return true;
        }
        
        if term.is_variable() {
            if bound.contains(term) {
                return false;
            }
            
            bound.push(term.clone());
        } else if let Some(app) = term.to_application() {
            for op in app.operands() {
                if !RewriteRule::is_left_linear(op, bound) {
                    return false;
                }
            }
        }
        
        true
    }
    
    pub fn is_computation_rule(&self) -> bool {
        if self.pattern.is_program_term() {
            true
        } else if let Some(app) = self.pattern.to_application() {
            if !app.operator().is_program_term() {
                return false;
            }
            
            if app.operands().iter().any(|op| !op.constructor_pattern()) {
                return false;
            }
            
            let mut bound = vec![];
            for op in app.operands() {
                if !RewriteRule::is_left_linear(op, &mut bound) {
                    return false;
                }
            }
            
            true
        } else {
            false
        }
    }
}

impl SubstitutableWith<TermSubstEntry> for Rc<RewriteRule> {
    fn substitute_with(&self, from: &TermSubstEntry, to: &TermSubstEntry) -> Self {
        RewriteRule::create(
            self.pattern.substitute(from, to),
            self.result.substitute(from, to)
        )
    }
}

impl PrettyPrintable for RewriteRule {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        PPElement::group(vec![
            self.pattern.to_pp_element(detail),
            PPElement::break_elem(1, 4, false),
            PPElement::text("->".to_string()),
            PPElement::break_elem(1, 4, false),
            self.result.to_pp_element(detail)
        ], BreakType::Flexible, 0)
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}


#[derive(Clone, PartialEq, Eq)]
pub struct ProgramConstant {
    name: String,
    minlog_type: Rc<MinlogType>,
    computation_rules: RefCell<Vec<Rc<RewriteRule>>>,
    rewrite_rules: RefCell<Vec<Rc<RewriteRule>>>,
    totality: RefCell<Totality>,
    type_variables: Vec<Rc<MinlogType>>,
}

impl ProgramConstant {
    pub fn create(name: String, minlog_type: Rc<MinlogType>, totality: Totality, type_variables: Vec<Rc<MinlogType>>) -> Rc<ProgramConstant> {
        Rc::new(ProgramConstant {
            name,
            minlog_type,
            computation_rules: RefCell::new(vec![]),
            rewrite_rules: RefCell::new(vec![]),
            totality: RefCell::new(totality),
            type_variables,
        })
    }
    
    pub fn name(&self) -> &String {
        &self.name
    }
    
    pub fn minlog_type(&self) -> Rc<MinlogType> {
        self.minlog_type.clone()
    }
    
    pub fn totality(&self) -> Totality {
        self.totality.borrow().clone()
    }
    
    pub fn set_totality(&self, totality: Totality) {
        *self.totality.borrow_mut() = totality;
    }
    
    pub fn computation_rules(&self) -> Vec<Rc<RewriteRule>> {
        self.computation_rules.borrow().iter().cloned().collect()
    }
    
    pub fn add_computation_rule(&self, rule: Rc<RewriteRule>) {
        if rule.program_constant().as_ref() != self {
            panic!("Attempted to add a rewrite rule for a different program constant");
        }
        
        if !rule.is_computation_rule() {
            panic!("Attempted to add a non-computation rule as a computation rule");
        }
        
        for var in rule.pattern().get_type_variables() {
            if !self.type_variables.contains(&var) {
                panic!("Type variable '{}' in computation rule pattern not in program constant type variables", var.debug_string());
            }
        }
        
        for rule in self.computation_rules.borrow().iter() {
            let unifier = TermSubstitution::unify(&rule.pattern().into(), &rule.pattern().into());
            if let Some(unifier) = unifier {
                let existing = unifier.substitute::<TermSubstEntry>(&rule.result().into());
                let new = unifier.substitute::<TermSubstEntry>(&rule.result().into());

                if existing != new {
                    panic!("Attempted to add a computation rule that overlaps with an existing computation rule but has a different result");
                }
            }
        }
        
        self.computation_rules.borrow_mut().push(rule);
    }
    
    pub fn rewrite_rules(&self) -> Vec<Rc<RewriteRule>> {
        self.rewrite_rules.borrow().iter().cloned().collect()
    }
    
    pub fn add_rewrite_rule(&self, rule: Rc<RewriteRule>) {
        if rule.program_constant().as_ref() != self {
            panic!("Attempted to add a rewrite rule for a different program constant");
        }
        
        self.rewrite_rules.borrow_mut().push(rule);
    }
    
    pub fn compute(&self, term: &Rc<MinlogTerm>) -> (Rc<MinlogTerm>, bool) {
        for rule in self.computation_rules.borrow().iter() {
            if let Some(subst) = TermSubstitution::match_with(&rule.pattern().into(), &term.into()) {
                return (subst.substitute::<TermSubstEntry>(&rule.result().into()).to_term().unwrap(), true);
            }
        }
        
        for rule in self.rewrite_rules.borrow().iter() {
            if let Some(subst) = TermSubstitution::match_with(&rule.pattern().into(), &term.into()) {
                return (subst.substitute::<TermSubstEntry>(&rule.result().into()).to_term().unwrap(), true);
            }
        }
        
        (term.clone(), false)
    }
    
    pub fn get_free_variables(&self) -> Vec<Rc<MinlogTerm>> {
        let mut vars = vec![];
        
        for rule in self.computation_rules.borrow().iter() {
            for var in MinlogTerm::get_free_variables(&rule.pattern()) {
                if !vars.contains(&var) {
                    vars.push(var);
                }
            }
        }
        
        vars
    }
    
    pub fn get_type_variables(&self) -> &Vec<Rc<MinlogType>> {
        &self.type_variables
    }
}

impl PrettyPrintable for ProgramConstant {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        let tvars = self.get_type_variables();
        
        let name = if tvars.is_empty() {
            PPElement::text(self.name.clone())
        } else {
            let tvars = PPElement::list(
                self.get_type_variables().iter().map(|tv| tv.to_pp_element(detail)).collect(),
                PPElement::break_elem(0, 4, false),
                PPElement::text(",".to_string()),
                PPElement::break_elem(1, 4, false),
                BreakType::Flexible
            );
            
            PPElement::group(vec![
                PPElement::text(self.name.clone()),
                PPElement::text("<".to_string()),
                PPElement::break_elem(1, 4, false),
                tvars,
                PPElement::break_elem(1, 0, false),
                PPElement::text(">".to_string())
            ], BreakType::Consistent, 0)
        };
        
        if detail {
            let comp_rules = self.computation_rules.borrow();
            let rewrite_rules = self.rewrite_rules.borrow();
            
            let has_comp_rules = !comp_rules.is_empty();
            let has_rewrite_rules = !rewrite_rules.is_empty();
            
            let comp_rules = PPElement::list(
                comp_rules.iter().map(|c| c.to_pp_element(true)).collect(),
                PPElement::break_elem(0, 0, false),
                PPElement::text(";".to_string()),
                PPElement::break_elem(1, 0, false),
                BreakType::Flexible
            );
            
            let rewrite_rules = PPElement::list(
                rewrite_rules.iter().map(|r| r.to_pp_element(true)).collect(),
                PPElement::break_elem(0, 0, false),
                PPElement::text(";".to_string()),
                PPElement::break_elem(1, 0, false),
                BreakType::Flexible
            );
            
            let mut rule_objects = vec![];
            if has_comp_rules {
                rule_objects.push(comp_rules);
                if has_rewrite_rules {
                    rule_objects.push(PPElement::break_elem(1, 4, true));
                } else {
                    rule_objects.push(PPElement::break_elem(1, 0, true));
                }
            }
            
            if has_comp_rules && has_rewrite_rules {
                rule_objects.push(PPElement::text("-------".to_string()));
            }
            
            if has_rewrite_rules {
                rule_objects.push(PPElement::break_elem(1, 4, true));
                rule_objects.push(rewrite_rules);
            }
            
            PPElement::group(vec![
                name,
                PPElement::text(" {".to_string()),
                PPElement::break_elem(1, 4, true),
                PPElement::group(rule_objects, BreakType::Flexible, 0),
                PPElement::break_elem(1, 0, true),
                PPElement::text("}".to_string())
            ], BreakType::Consistent, 0)
        } else {
            name
        }
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}