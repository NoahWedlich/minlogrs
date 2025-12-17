
use crate::includes::{
    essential::*,
    utils::*,
    core::{
        types::*,
        terms::*,
    }
};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct KernelApplication {
    operands: Vec<MinlogTerm>,
    operator: MinlogTerm,
    minlog_type: Rc<MinlogType>,
}

impl KernelApplication {
    pub fn create(operator: MinlogTerm, operands: Vec<MinlogTerm>) -> MinlogTerm {
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
        
        KernelApplication::collapse(&MinlogTerm::Application(Rc::new(KernelApplication {
            operands,
            operator,
            minlog_type,
        }).into()))
    }
    
    fn collapse(minlog_term: &MinlogTerm) -> MinlogTerm {
        if !minlog_term.is_application() || !minlog_term.to_application().unwrap().operator().is_application() {
            minlog_term.clone()
        } else {
            let mut application = minlog_term.to_application().unwrap();
            let mut operands = application.operands().clone();
            
            while application.operator().is_application() {
                let next_application = application.operator().to_application().unwrap();
                operands.splice(0..0, next_application.operands().iter().cloned());
                application = next_application;
            }
            
            Application::create(application.operator().clone(), operands)
        }
    }
    
    pub fn operand_count(&self) -> usize {
        self.operands.len()
    }
    
    pub fn operands(&self) -> &Vec<MinlogTerm> {
        &self.operands
    }
    
    pub fn operand(&self, index: usize) -> Option<&MinlogTerm> {
        self.operands.get(index)
    }
    
    pub fn operator(&self) -> &MinlogTerm {
        &self.operator
    }
}

impl TermBody for KernelApplication {
    fn minlog_type(&self) -> Rc<MinlogType> {
        Rc::clone(&self.minlog_type)
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> MinlogTerm {
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
            let normalized_operands: Vec<MinlogTerm> = self.operands.iter()
                .map(|op| op.normalize(eta, pi)).collect();
            
            Application::create(normalized_operator, normalized_operands)
        }
    }
    
    fn remove_nulls(&self) -> Option<MinlogTerm> {
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
    
    fn get_free_variables(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        self.operator.get_free_variables(visited)
            .union(&self.operands.iter().flat_map(|op| op.get_free_variables(visited)).collect::<IndexSet<_>>())
            .cloned().collect()
    }

    fn get_bound_variables(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        self.operator.get_bound_variables(visited)
            .union(&self.operands.iter().flat_map(|op| op.get_bound_variables(visited)).collect::<IndexSet<_>>())
            .cloned().collect()
    }

    fn get_constructors(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        self.operator.get_constructors(visited)
            .union(&self.operands.iter().flat_map(|op| op.get_constructors(visited)).collect::<IndexSet<_>>())
            .cloned().collect()
    }

    fn get_program_terms(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        self.operator.get_program_terms(visited)
            .union(&self.operands.iter().flat_map(|op| op.get_program_terms(visited)).collect::<IndexSet<_>>())
            .cloned().collect()
    }
    
    fn alpha_equivalent(&self, other: &MinlogTerm,
        forward: &mut Vec<(TermVariable, TermVariable)>,
        backward: &mut Vec<(TermVariable, TermVariable)>) -> bool {
            
        if !other.is_application() {
            return false;
        }
        
        let other = other.to_application().unwrap();
        
        if self.operands.len() != other.operands().len() {
            return false;
        }
        
        self.operator.alpha_equivalent(other.operator(), forward, backward) &&
            self.operands.iter().zip(other.operands().iter())
            .all(|(a, b)| a.alpha_equivalent(b, forward, backward))
    }

    fn substitute(&self, from: &TermSubstEntry, to: &TermSubstEntry) -> MinlogTerm {
        if let Some(tm) = from.to_term() && tm.is_application() && Application::Kernel(Rc::new(self.clone())) == *tm.to_application().unwrap() {
            to.to_term().unwrap()
        } else {
            let operator = self.operator.substitute(from, to);
            let operands: Vec<MinlogTerm> = self.operands.iter().map(|op| op.substitute(from, to)).collect();
            Application::create(operator, operands)
        }
    }

    fn first_conflict_with(&self, other: &MinlogTerm) -> Option<(TermSubstEntry, TermSubstEntry)> {
        if let Some(conflict) = self.minlog_type.first_conflict_with(&other.minlog_type()) {
            return Some((conflict.0.into(), conflict.1.into()));
        }
        
        if !other.is_application() {
            return Some((MinlogTerm::Application(Rc::new(self.clone()).into()).into(), other.clone().into()));
        }
        
        let other_app = other.to_application().unwrap();
        
        if self.operands.len() != other_app.operands().len() {
            return Some((MinlogTerm::Application(Rc::new(self.clone()).into()).into(), other.clone().into()));
        }
        
        if let Some((f, o)) = self.operator.first_conflict_with(other_app.operator()) {
            return Some((f, o));
        }
        
        for (a, b) in self.operands.iter().zip(other_app.operands().iter()) {
            if let Some((f, o)) = a.first_conflict_with(b) {
                return Some((f, o));
            }
        }
        
        None
    }

    fn match_with(&self, instance: &MinlogTerm) -> MatchOutput<TermSubstEntry> {
        if !instance.is_application() {
            return MatchOutput::FailedMatch;
        }
        
        let app_instance = instance.to_application().unwrap();
        
        if self.operands.len() != app_instance.operands().len() {
            return MatchOutput::FailedMatch;
        }
        
        let mut conditions = self.operands.iter().zip(app_instance.operands().iter())
            .filter_map(|(p_op, i_op)| {
                if p_op != i_op {
                    Some((p_op.into(), i_op.into()))
                } else {
                    None
                }
            }).collect::<IndexMap<_, _>>();
        
        if self.operator != *app_instance.operator() {
            conditions.insert(self.operator.clone().into(), app_instance.operator().clone().into());
        }
        
        MatchOutput::Matched(conditions)
    }
}

impl PrettyPrintable for KernelApplication {
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

pub trait NativeApplication: NativeTermBody {
    fn operand_count(&self) -> usize;
    
    fn operands(&self) -> &Vec<MinlogTerm>;
    
    fn operand(&self, index: usize) -> Option<&MinlogTerm>;
    
    fn operator(&self) -> &MinlogTerm;
    
    fn to_kernel(&self) -> KernelApplication {
        KernelApplication {
            operands: self.operands().clone(),
            operator: self.operator().clone(),
            minlog_type: self.minlog_type(),
        }
    }
}

wrapper_enum::wrapper_enum! {
    #[derive(Clone)]
    pub enum Application {
        Kernel(kernel: Rc<KernelApplication>),
        Native(native: Rc<dyn NativeApplication>),
    }
    
    ext trait TermBody: PrettyPrintable {
        fwd fn minlog_type(&self) -> Rc<MinlogType>
    
        fwd fn normalize(&self, eta: bool, pi: bool) -> MinlogTerm
    
        fwd fn apply_args(&self, args: &Vec<MinlogTerm>) -> Option<MinlogTerm>
    
        fwd fn remove_nulls(&self) -> Option<MinlogTerm>
    
        fwd fn length(&self) -> usize
    
        fwd fn depth(&self) -> usize
    
        fwd fn constructor_pattern(&self) -> bool
    
        fwd fn get_type_variables(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogType>>

        fwd fn get_algebra_types(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogType>>

        fwd fn get_free_variables(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm>
    
        fwd fn get_bound_variables(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm>
    
        fwd fn get_constructors(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm>
    
        fwd fn get_program_terms(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm>
    
        fwd fn alpha_equivalent(&self, other: &MinlogTerm,
            forward: &mut Vec<(TermVariable, TermVariable)>,
            backward: &mut Vec<(TermVariable, TermVariable)>) -> bool
    
        fwd fn substitute(&self, from: &TermSubstEntry, to: &TermSubstEntry) -> MinlogTerm
    
        fwd fn first_conflict_with(&self, other: &MinlogTerm) -> Option<(TermSubstEntry, TermSubstEntry)>
    
        fwd fn match_with(&self, instance: &MinlogTerm) -> MatchOutput<TermSubstEntry>
    }
    
    fwd trait ApplicationForwards {
        pub fwd fn operand_count(&self) -> usize
        
        pub fwd fn operands(&self) -> &Vec<MinlogTerm>
        
        pub fwd fn operand(&self, index: usize) -> Option<&MinlogTerm>
        
        pub fwd fn operator(&self) -> &MinlogTerm
    }
    
    ext trait PrettyPrintable {
        fwd fn to_pp_element(&self, detail: bool) -> PPElement

        fwd fn requires_parens(&self, detail: bool) -> bool

        fwd fn open_paren(&self) -> String

        fwd fn close_paren(&self) -> String
    }
}

impl Application {
    pub fn create(operator: MinlogTerm, operands: Vec<MinlogTerm>) -> MinlogTerm {
        KernelApplication::create(operator, operands)
    }
    
    pub fn into_kernel_application(self) -> Rc<KernelApplication> {
        match self {
            Application::Kernel(k) => k,
            Application::Native(n) => Rc::new(n.to_kernel()),
        }
    }
}

impl Hash for Application {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Application::Kernel(k) => k.hash(state),
            Application::Native(n) => n.to_kernel().hash(state),
        }
    }
}

impl PartialEq for Application {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Application::Kernel(k1), Application::Kernel(k2)) => k1 == k2,
            (Application::Native(n1), Application::Kernel(k2)) => n1.to_kernel() == *k2.as_ref(),
            (Application::Kernel(k1), Application::Native(n2)) => *k1.as_ref() == n2.to_kernel(),
            (Application::Native(n1), Application::Native(n2)) => n1.eq(n2.as_ref()),
        }
    }
}

impl Eq for Application {}

impl From<Rc<KernelApplication>> for Application {
    fn from(k: Rc<KernelApplication>) -> Self {
        Application::Kernel(k)
    }
}

impl From<&Rc<KernelApplication>> for Application {
    fn from(k: &Rc<KernelApplication>) -> Self {
        Application::Kernel(k.clone())
    }
}

impl From<Rc<dyn NativeApplication>> for Application {
    fn from(n: Rc<dyn NativeApplication>) -> Self {
        Application::Native(n)
    }
}

impl From<&Rc<dyn NativeApplication>> for Application {
    fn from(n: &Rc<dyn NativeApplication>) -> Self {
        Application::Native(n.clone())
    }
}