
use crate::includes::{
    essential::*,
    utils::*,
    core::{
        types::*,
        terms::*,
    }
};

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
                panic!("Tried to create an Application with an operand of the wrong type: expected {}, got {}",
                    arrow.arguments()[i].debug_string(),
                    op.minlog_type().debug_string()
                );
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
                if op.is_tuple() || op.is_match_term() {
                    println!("Warning: Pi-normalization for Applications is not implemented yet.");
                    break;
                }
            }
        }
        
        if let Some(computed) = self.operator.apply_args(&self.operands) {
            computed.normalize(eta, pi)
        } else {
            let normalized_operator = self.operator.normalize(eta, pi);
            let normalized_operands: Vec<Rc<MinlogTerm>> = self.operands.iter()
                .map(|op| op.normalize(eta, pi)).collect();
            
            Application::create(normalized_operator, normalized_operands)
        }
    }
    
    fn remove_nulls(&self) -> Option<Rc<MinlogTerm>> {
        let new_operands = self.operands.iter()
            .filter_map(|op| op.remove_nulls())
            .collect::<Vec<_>>();
        
        self.operator.remove_nulls().map(|new_operator| {
            Application::create(new_operator, new_operands)
        })
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
    
    fn get_type_variables(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogType>> {
        self.minlog_type.get_type_variables(&mut IndexSet::new())
    }
    
    fn get_algebra_types(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogType>> {
        self.minlog_type.get_algebra_types(&mut IndexSet::new())
    }
    
    fn get_free_variables(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogTerm>> {
        self.operator.get_free_variables(visited)
            .union(&self.operands.iter().flat_map(|op| op.get_free_variables(visited)).collect::<IndexSet<_>>())
            .cloned().collect()
    }

    fn get_bound_variables(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogTerm>> {
        self.operator.get_bound_variables(visited)
            .union(&self.operands.iter().flat_map(|op| op.get_bound_variables(visited)).collect::<IndexSet<_>>())
            .cloned().collect()
    }

    fn get_constructors(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogTerm>> {
        self.operator.get_constructors(visited)
            .union(&self.operands.iter().flat_map(|op| op.get_constructors(visited)).collect::<IndexSet<_>>())
            .cloned().collect()
    }

    fn get_program_terms(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogTerm>> {
        self.operator.get_program_terms(visited)
            .union(&self.operands.iter().flat_map(|op| op.get_program_terms(visited)).collect::<IndexSet<_>>())
            .cloned().collect()
    }

    fn get_internal_constants(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogTerm>> {
        self.operator.get_internal_constants(visited)
            .union(&self.operands.iter().flat_map(|op| op.get_internal_constants(visited)).collect::<IndexSet<_>>())
            .cloned().collect()
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

    fn match_with(&self, instance: &Rc<MinlogTerm>) -> MatchOutput<TermSubstEntry> {
        if !instance.is_application() {
            return MatchOutput::FailedMatch;
        }
        
        let app_instance = instance.to_application().unwrap();
        
        if self.operands.len() != app_instance.operands.len() {
            return MatchOutput::FailedMatch;
        }
        
        let mut conditions = self.operands.iter().zip(app_instance.operands.iter())
            .filter_map(|(p_op, i_op)| {
                if p_op != i_op {
                    Some((p_op.into(), i_op.into()))
                } else {
                    None
                }
            }).collect::<IndexMap<_, _>>();
        
        if self.operator != app_instance.operator {
            conditions.insert(self.operator.clone().into(), app_instance.operator().clone().into());
        }
        
        MatchOutput::Matched(conditions)
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
            self.operator.to_pp_element(detail),
            PPElement::text(" (".to_string()),
            PPElement::break_elem(1, 4, false),
            operands,
            PPElement::break_elem(1, 0, false),
            PPElement::text(")".to_string())
        ], BreakType::Consistent, 0)
    }
    
    fn requires_parens(&self, detail: bool) -> bool {
        self.operator.requires_parens(detail)
    }
    
    fn open_paren(&self) -> String {
        "(".to_string()
    }
    
    fn close_paren(&self) -> String {
        ")".to_string()
    }
}