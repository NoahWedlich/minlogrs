
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
    operand: MinlogTerm,
    operator: MinlogTerm,
    minlog_type: Arc<MinlogType>,
}

impl KernelApplication {
    pub fn create(operator: MinlogTerm, operand: MinlogTerm) -> MinlogTerm {
        let operator_type = operator.minlog_type();
        
        if !operator_type.is_arrow() {
            panic!("Tried to create an Application with a non-arrow operator");
        }
        
        let arrow_type = operator_type.to_arrow().unwrap();
        
        if arrow_type.argument() != &operand.minlog_type() {
            panic!("Tried to create an Application with an operand of the wrong type: expected {}, got {}",
                arrow_type.argument().debug_string(),
                operand.minlog_type().debug_string()
            );
        }
        
        let minlog_type = arrow_type.value().clone();
        
        MinlogTerm::Application(Arc::new(KernelApplication { operand, operator, minlog_type, }).into())
    }
    
    pub fn operand(&self) -> &MinlogTerm {
        &self.operand
    }
    
    pub fn all_operands(&self) -> Vec<MinlogTerm> {
        let mut current = &Application::Kernel(Arc::new(self.clone()));
        let mut operands = vec![current.operand().clone()];
        
        while let Some(next_app) = current.operator().to_application() {
            operands.push(next_app.operand().clone());
            current = next_app;
        }
        
        operands.into_iter().rev().collect()
    }
    
    pub fn operand_at(&self, index: usize) -> Option<&MinlogTerm> {
        if index == 0 {
            Some(&self.operand)
        } else if self.operator.is_application() {
            self.operator.to_application().unwrap().operand_at(index - 1)
        } else {
            None
        }
    }
    
    pub fn operator(&self) -> &MinlogTerm {
        &self.operator
    }
    
    pub fn final_operator(&self) -> &MinlogTerm {
        if let Some(next_app) = self.operator.to_application() {
            next_app.final_operator()
        } else {
            &self.operator
        }
    }
}

impl TermBody for KernelApplication {
    fn minlog_type(&self) -> Arc<MinlogType> {
        self.minlog_type.clone()
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> MinlogTerm {
        if pi && (self.operand.is_pair() || self.operand.is_match_term()) {
            println!("Warning: Pi-normalization for Applications is not implemented yet.");
        }
        
        let normalized_operator = self.operator.normalize(eta, pi);
        let normalized_operand = self.operand.normalize(eta, pi);
        
        if let Some(computed) = normalized_operator.apply_arg(self.operand.clone()) {
            computed.normalize(eta, pi)
        } else if let Some(program_term) = self.final_operator().to_program_term() {
            let all_args = self.all_operands();
            let to_match = Application::create_nested(
                self.final_operator().clone(),
                all_args,
            );
            
            if let Some(computed) = program_term.apply_arg(to_match) {
                computed.normalize(eta, pi)
            } else {
                Application::create(normalized_operator, normalized_operand)
            }
        }  else {
            
            Application::create(normalized_operator, normalized_operand)
        }
    }
    
    fn remove_nulls(&self) -> Option<MinlogTerm> {
        if let Some(new_operator) = self.operator.remove_nulls() {
            if let Some(new_operand) = self.operand.remove_nulls() {
                Some(Application::create(new_operator, new_operand))
            } else {
                Some(new_operator)
            }
        } else {
            None
        }
    }
    
    fn length(&self) -> usize {
        1 + self.operand.length() + self.operator.length()
    }
    
    fn depth(&self) -> usize {
        1 + max(
            self.operand.depth(),
            self.operator.depth(),
        )
    }
    
    fn constructor_pattern(&self) -> bool {
        self.operator.is_constructor() && self.operand.constructor_pattern()
    }
    
    fn get_type_variables(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Arc<MinlogType>> {
        self.minlog_type.get_type_variables(&mut IndexSet::new())
    }
    
    fn get_algebra_types(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Arc<MinlogType>> {
        self.minlog_type.get_algebra_types(&mut IndexSet::new())
    }
    
    fn get_free_variables(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        self.operator.get_free_variables(visited)
            .union(&self.operand.get_free_variables(visited))
            .cloned().collect()
    }

    fn get_bound_variables(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        self.operator.get_bound_variables(visited)
            .union(&self.operand.get_bound_variables(visited))
            .cloned().collect()
    }

    fn get_constructors(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        self.operator.get_constructors(visited)
            .union(&self.operand.get_constructors(visited))
            .cloned().collect()
    }

    fn get_program_terms(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        self.operator.get_program_terms(visited)
            .union(&self.operand.get_program_terms(visited))
            .cloned().collect()
    }
    
    fn alpha_equivalent(&self, other: &MinlogTerm,
        forward: &mut Vec<(TermVariable, TermVariable)>,
        backward: &mut Vec<(TermVariable, TermVariable)>) -> bool
    {
        if !other.is_application() {
            return false;
        }
        
        let other = other.to_application().unwrap();
        
        self.operator.alpha_equivalent(other.operator(), forward, backward) &&
            self.operand.alpha_equivalent(other.operand(), forward, backward)
    }

    fn substitute(&self, from: &TermSubstEntry, to: &TermSubstEntry) -> MinlogTerm {
        if let Some(tm) = from.to_term() && tm.is_application() && Application::Kernel(Arc::new(self.clone())) == *tm.to_application().unwrap() {
            to.to_term().unwrap()
        } else {
            let operator = self.operator.substitute(from, to);
            let operand = self.operand.substitute(from, to);
            Application::create(operator, operand)
        }
    }

    fn first_conflict_with(&self, other: &MinlogTerm) -> Option<(TermSubstEntry, TermSubstEntry)> {
        if let Some(conflict) = self.minlog_type.first_conflict_with(&other.minlog_type()) {
            return Some((conflict.0.into(), conflict.1.into()));
        }
        
        if !other.is_application() {
            return Some((MinlogTerm::Application(Arc::new(self.clone()).into()).into(), other.clone().into()));
        }
        
        let other_app = other.to_application().unwrap();
        
        if let Some(conflict) = self.operator.first_conflict_with(other_app.operator()) {
            return Some(conflict);
        }
        
        if let Some(conflict) = self.operand.first_conflict_with(other_app.operand()) {
            return Some(conflict);
        }
        
        None
    }

    fn match_with(&self, instance: &MinlogTerm) -> MatchOutput<TermSubstEntry> {
        if !instance.is_application() {
            return MatchOutput::FailedMatch;
        }
        
        let app_instance = instance.to_application().unwrap();
        
        let conditions = IndexMap::from([
            (self.operator.clone().into(), app_instance.operator().clone().into()),
            (self.operand.clone().into(), app_instance.operand().clone().into()),
        ]);
        
        MatchOutput::Matched(conditions)
    }
}

impl PrettyPrintable for KernelApplication {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        PPElement::group(vec![
            self.operator.to_pp_element(detail),
            PPElement::text(" (".to_string()),
            PPElement::break_elem(1, 4, false),
            self.operand.to_pp_element(detail),
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
    fn operand(&self) -> &MinlogTerm;
    
    fn all_operands(&self) -> Vec<MinlogTerm>;
    
    fn operand_at(&self, index: usize) -> Option<&MinlogTerm>;
    
    fn operator(&self) -> &MinlogTerm;
    
    fn final_operator(&self) -> &MinlogTerm;
    
    fn to_kernel(&self) -> KernelApplication {
        KernelApplication {
            operand: self.operand().clone(),
            operator: self.operator().clone(),
            minlog_type: self.minlog_type(),
        }
    }
}

wrapper_enum::wrapper_enum! {
    #[derive(Clone)]
    pub enum Application {
        Kernel(kernel: Arc<KernelApplication>),
        Native(native: Arc<dyn NativeApplication>),
    }
    
    ext trait TermBody: PrettyPrintable {
        fwd fn minlog_type(&self) -> Arc<MinlogType>
    
        fwd fn normalize(&self, eta: bool, pi: bool) -> MinlogTerm
    
        fwd fn apply_arg(&self, arg: MinlogTerm) -> Option<MinlogTerm>
    
        fwd fn remove_nulls(&self) -> Option<MinlogTerm>
    
        fwd fn length(&self) -> usize
    
        fwd fn depth(&self) -> usize
    
        fwd fn constructor_pattern(&self) -> bool
    
        fwd fn get_type_variables(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Arc<MinlogType>>

        fwd fn get_algebra_types(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Arc<MinlogType>>

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
        pub fwd fn operand(&self) -> &MinlogTerm
        
        pub fwd fn all_operands(&self) -> Vec<MinlogTerm>
        
        pub fwd fn operand_at(&self, index: usize) -> Option<&MinlogTerm>
        
        pub fwd fn operator(&self) -> &MinlogTerm
        
        pub fwd fn final_operator(&self) -> &MinlogTerm
    }
    
    ext trait PrettyPrintable {
        fwd fn to_pp_element(&self, detail: bool) -> PPElement

        fwd fn requires_parens(&self, detail: bool) -> bool

        fwd fn open_paren(&self) -> String

        fwd fn close_paren(&self) -> String
    }
}

impl Application {
    pub fn create(operator: MinlogTerm, operand: MinlogTerm) -> MinlogTerm {
        KernelApplication::create(operator, operand)
    }
    
    pub fn create_nested(operator: MinlogTerm, operands: Vec<MinlogTerm>) -> MinlogTerm {
        let mut current_operator = operator;
        
        for operand in operands.into_iter() {
            current_operator = Application::create(current_operator, operand);
        }
        
        current_operator
    }
    
    pub fn into_kernel_application(self) -> Arc<KernelApplication> {
        match self {
            Application::Kernel(k) => k,
            Application::Native(n) => Arc::new(n.to_kernel()),
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

impl From<Arc<KernelApplication>> for Application {
    fn from(k: Arc<KernelApplication>) -> Self {
        Application::Kernel(k)
    }
}

impl From<&Arc<KernelApplication>> for Application {
    fn from(k: &Arc<KernelApplication>) -> Self {
        Application::Kernel(k.clone())
    }
}

impl From<Arc<dyn NativeApplication>> for Application {
    fn from(n: Arc<dyn NativeApplication>) -> Self {
        Application::Native(n)
    }
}

impl From<&Arc<dyn NativeApplication>> for Application {
    fn from(n: &Arc<dyn NativeApplication>) -> Self {
        Application::Native(n.clone())
    }
}