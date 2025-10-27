
use std::{cmp::{max, min}, rc::Rc, collections::HashSet};
use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};

use crate::core::substitution::{MatchContext, MatchOutput};

use crate::core::types::minlog_type::MinlogType;
use crate::core::types::arrow_type::ArrowType;

use crate::core::terms::minlog_term::{TermBody, MinlogTerm, Totality};
use crate::core::terms::term_variable::TermVariable;
use crate::core::terms::abstraction::Abstraction;

use crate::core::terms::term_substitution::{TermSubstEntry, TermSubstitution};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Application {
    operands: Vec<Rc<MinlogTerm>>,
    operator: Rc<MinlogTerm>,
    minlog_type: Rc<MinlogType>,
}

impl Application {
    pub fn create(operator: Rc<MinlogTerm>, operands: Vec<Rc<MinlogTerm>>) -> Rc<MinlogTerm> {
        if operands.is_empty() {
            return operator;
        }
        
        let operator_type = operator.minlog_type();
        if !operator_type.is_arrow() {
            panic!("Tried to create an Application with a non-arrow operator");
        }
        
        let arrow = operator_type.to_arrow().unwrap();
        
        if operator.minlog_type().arity() < operands.len() {
            panic!("Tried to create an Application with too many operands");
        }
        
        for (i, op) in operands.iter().enumerate() {
            if !op.minlog_type().eq(&arrow.arguments()[i]) {
                panic!("Tried to create an Application with an operand of the wrong type");
            }
        }
        
        let remaining_arg_types = arrow.arguments()[operands.len()..].to_vec();
        let minlog_type = if remaining_arg_types.is_empty() {
            Rc::clone(arrow.value())
        } else {
            ArrowType::create(remaining_arg_types, Rc::clone(arrow.value()))
        };
        
        Application::collapse(&Rc::new(MinlogTerm::Application(Application {
            operands,
            operator,
            minlog_type,
        })))
    }
    
    pub fn collapse(minlog_term: &Rc<MinlogTerm>) -> Rc<MinlogTerm> {
        if !minlog_term.is_application() || !minlog_term.to_application().unwrap().operator.is_application() {
            Rc::clone(minlog_term)
        } else {
            let mut application = minlog_term.to_application().unwrap();
            let mut operands = application.operands().clone();
            
            while application.operator().is_application() {
                let next_application = application.operator().to_application().unwrap();
                operands.splice(0..0, next_application.operands().iter().cloned());
                application = next_application;
            }
            
            Application::create(Rc::clone(application.operator()), operands)
        }
    }
    
    pub fn operand_count(&self) -> usize {
        self.operands.len()
    }
    
    pub fn operands(&self) -> &Vec<Rc<MinlogTerm>> {
        &self.operands
    }
    
    pub fn operand(&self, index: usize) -> &Rc<MinlogTerm> {
        &self.operands[index]
    }
    
    pub fn operator(&self) -> &Rc<MinlogTerm> {
        &self.operator
    }
}

impl TermBody for Application {
    fn minlog_type(&self) -> Rc<MinlogType> {
        Rc::clone(&self.minlog_type)
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> Rc<MinlogTerm> {
        if self.operands.is_empty() {
            return self.operator.normalize(eta, pi);
        }
        
        if pi {
            for op in &self.operands {
                if op.is_tuple() || op.is_conditional() {
                    println!("Warning: Pi-normalization for Applications is not implemented yet.");
                    break;
                }
            }
        }
        
        if self.operator.is_abstraction() {
            let abs = self.operator.to_abstraction().unwrap();
            
            let applicable = min(self.operand_count(), abs.arity());
            
            let mut subst = TermSubstitution::make_empty();
            
            for i in 0..applicable {
                let var = abs.var(i).unwrap();
                let op = self.operand(i).clone();
                
                if !var.minlog_type().eq(&op.minlog_type()) {
                    panic!("Tried to apply an abstraction to an operand of the wrong type");
                }
                
                if var.totality(&mut HashSet::new()) == Totality::Total && op.totality(&mut HashSet::new()) == Totality::Partial {
                    panic!("Tried to apply an abstraction with a total variable to a partial operand");
                }
                
                if op.contains_free_variable(&var.clone()) {
                    panic!("Tried to apply an abstraction to an operand that contains the bound variable");
                }
                
                subst.extend((TermSubstEntry::Term(var.clone()), TermSubstEntry::Term(op.normalize(eta, pi))));
            }
            
            let kernel = subst.apply(&TermSubstEntry::Term(abs.kernel().clone())).to_term().unwrap().clone();
            
            let remaining_vars = abs.vars()[applicable..].to_vec();
            let inner_term = if remaining_vars.is_empty() {
                kernel.normalize(eta, pi)
            } else {
                Abstraction::create(remaining_vars, kernel).normalize(eta, pi)
            };
            
            let remaining_operands = self.operands[applicable..].iter()
                .map(|op| op.normalize(eta, pi)).collect::<Vec<_>>();
            
            if remaining_operands.is_empty() {
                return inner_term;
            } else {
                return Application::create(inner_term, remaining_operands);
            }
        } else if self.operator.is_program_term() {
            let (computed, success) = self.operator.to_program_term().unwrap().pconst().compute(
                &Application::create(self.operator.clone(), self.operands.clone())
            );
            
            if success {
                return computed.normalize(eta, pi);
            }
        }
        
        let operator = self.operator.normalize(eta, pi);
        let operands: Vec<Rc<MinlogTerm>> = self.operands.iter().map(|op| op.normalize(eta, pi)).collect();
        Application::create(operator, operands)
    }
    
    fn length(&self) -> usize {
        1 + self.operands.iter().map(|op| op.length()).sum::<usize>() + self.operator.length()
    }
    
    fn depth(&self) -> usize {
        1 + max(
            self.operands.iter().map(|op| op.depth()).max().unwrap_or(0),
            self.operator.depth(),
        )
    }
    
    fn constructor_pattern(&self) -> bool {
        self.operator.is_constructor() && self.operands.iter().all(|op| op.constructor_pattern())
    }
    
    fn get_type_variables(&self) -> HashSet<Rc<MinlogType>> {
        self.minlog_type.get_type_variables()
    }
    
    fn get_algebra_types(&self) -> HashSet<Rc<MinlogType>> {
        self.minlog_type.get_algebra_types()
    }
    
    fn get_free_variables(&self) -> HashSet<Rc<MinlogTerm>> {
        self.operands.iter().flat_map(|op| op.get_free_variables())
            .chain(self.operator.get_free_variables())
            .collect()
    }

    fn get_bound_variables(&self) -> HashSet<Rc<MinlogTerm>> {
        self.operands.iter().flat_map(|op| op.get_bound_variables())
            .chain(self.operator.get_bound_variables())
            .collect()
    }

    fn get_constructors(&self) -> HashSet<Rc<MinlogTerm>> {
        self.operands.iter().flat_map(|op| op.get_constructors())
            .chain(self.operator.get_constructors())
            .collect()
    }

    fn get_program_terms(&self) -> HashSet<Rc<MinlogTerm>> {
        self.operands.iter().flat_map(|op| op.get_program_terms())
            .chain(self.operator.get_program_terms())
            .collect()
    }

    fn get_internal_constants(&self) -> HashSet<Rc<MinlogTerm>> {
        self.operands.iter().flat_map(|op| op.get_internal_constants())
            .chain(self.operator.get_internal_constants())
            .collect()
    }
    
    fn alpha_equivalent(&self, other: &Rc<MinlogTerm>,
        forward: &mut Vec<(TermVariable, TermVariable)>,
        backward: &mut Vec<(TermVariable, TermVariable)>) -> bool {
            
        if !other.is_application() {
            return false;
        }
        
        let other = other.to_application().unwrap();
        
        if self.operands.len() != other.operands.len() {
            return false;
        }
        
        self.operator.alpha_equivalent(other.operator(), forward, backward) &&
            self.operands.iter().zip(other.operands.iter())
            .all(|(a, b)| a.alpha_equivalent(b, forward, backward))
    }
    
    fn totality(&self, bound: &mut HashSet<TermVariable>) -> Totality {
        if self.operands.iter().any(|op| op.totality(bound) == Totality::Partial) {
            return Totality::Partial;
        }
        
        self.operator.totality(bound)
    }

    fn substitute(&self, from: &TermSubstEntry, to: &TermSubstEntry) -> Rc<MinlogTerm> {
        if let Some(tm) = from.to_term() && tm.is_application() && self == tm.to_application().unwrap() {
            to.to_term().unwrap()
        } else {
            let operator = self.operator.substitute(from, to);
            let operands: Vec<Rc<MinlogTerm>> = self.operands.iter().map(|op| op.substitute(from, to)).collect();
            Application::create(operator, operands)
        }
    }

    fn first_conflict_with(&self, other: &Rc<MinlogTerm>) -> Option<(TermSubstEntry, TermSubstEntry)> {
        if let Some(conflict) = self.minlog_type.first_conflict_with(&other.minlog_type()) {
            return Some((conflict.0.into(), conflict.1.into()));
        }
        
        if !other.is_application() {
            return Some((Rc::new(MinlogTerm::Application(self.clone())).into(), Rc::clone(other).into()));
        }
        
        let other_app = other.to_application().unwrap();
        
        if self.operands.len() != other_app.operands.len() {
            return Some((Rc::new(MinlogTerm::Application(self.clone())).into(), other.clone().into()));
        }
        
        if let Some((f, o)) = self.operator.first_conflict_with(other_app.operator()) {
            return Some((f, o));
        }
        
        for (a, b) in self.operands.iter().zip(other_app.operands.iter()) {
            if let Some((f, o)) = a.first_conflict_with(b) {
                return Some((f, o));
            }
        }
        
        None
    }

    fn match_with(&self, ctx: &mut impl MatchContext<TermSubstEntry>) -> MatchOutput<TermSubstEntry> {
        let pattern = ctx.next_pattern().unwrap();
        let instance = ctx.next_instance().unwrap();
        
        match (pattern, instance) {
            (TermSubstEntry::Term(p), TermSubstEntry::Term(i)) => {
                if !p.is_application() || !i.is_application() {
                    return MatchOutput::FailedMatch;
                }
                
                if p.minlog_type() != i.minlog_type() {
                    ctx.extend(&TermSubstEntry::Type(p.minlog_type()), &TermSubstEntry::Type(i.minlog_type()));
                    ctx.extend(&TermSubstEntry::Term(p.clone()), &TermSubstEntry::Term(i.clone()));
                    return MatchOutput::Matched;
                }
                
                let app_pattern = p.to_application().unwrap();
                let app_instance = i.to_application().unwrap();
                
                if app_pattern.operand_count() != app_instance.operand_count() {
                    return MatchOutput::FailedMatch;
                }
                
                for (op_p, op_i) in app_pattern.operands().iter().zip(app_instance.operands().iter()) {
                    ctx.extend(&TermSubstEntry::Term(op_p.clone()), &TermSubstEntry::Term(op_i.clone()));
                }
                
                ctx.extend(&TermSubstEntry::Term(app_pattern.operator().clone()), &TermSubstEntry::Term(app_instance.operator().clone()));
                MatchOutput::Matched
            },
            _ => {
                MatchOutput::FailedMatch
            }
        }
    }
}

impl PrettyPrintable for Application {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        if self.operands.is_empty() {
            return self.operator.to_pp_element(detail);
        }
        
        let operands = PPElement::list(
            self.operands.iter().map(|op| op.to_pp_element(detail)).collect(),
            PPElement::break_elem(0, 0, false),
            PPElement::text(",".to_string()),
            PPElement::break_elem(1, 0, false),
            BreakType::Flexible
        );
        
        PPElement::group(vec![
            self.operator.to_enclosed_pp_element(detail),
            PPElement::text(" (".to_string()),
            PPElement::break_elem(1, 4, false),
            operands,
            PPElement::break_elem(1, 0, false),
            PPElement::text(")".to_string())
        ], BreakType::Consistent, 0)
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