
use crate::includes::{
    essential::*,
    utils::*,
    core::{
        types::*,
        terms::*,
    }
};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct KernelAbstraction {
    var: MinlogTerm,
    kernel: MinlogTerm,
    minlog_type: Rc<MinlogType>,
}

impl KernelAbstraction {
    pub fn create(var: MinlogTerm, kernel: MinlogTerm) -> MinlogTerm {
        if !var.is_variable() {
            panic!("Tried to create an Abstraction with a non-variable term");
        }
        
        if kernel.get_bound_variables(&mut IndexSet::new()).contains(&var) {
            panic!("Tried to create an Abstraction where the variable is bound in the kernel");
        }
        
        let minlog_type = ArrowType::create(var.minlog_type(), kernel.minlog_type());
        
        MinlogTerm::Abstraction(Rc::new(KernelAbstraction { var, kernel, minlog_type, }).into())
    }
    
    pub fn closure(minlog_term: &MinlogTerm) -> MinlogTerm {
        let vars = minlog_term.get_free_variables(&mut IndexSet::new())
            .into_iter().collect();
        
        Abstraction::create_nested(vars, minlog_term.clone())
    }
    
    pub fn var(&self) -> &MinlogTerm {
        &self.var
    }
    
    pub fn all_vars(&self) -> Vec<MinlogTerm> {
        let mut current = &Abstraction::Kernel(Rc::new(self.clone()));
        let mut vars = vec![current.var().clone()];
        
        while let Some(next_abstraction) = current.kernel().to_abstraction() {
            vars.push(next_abstraction.var().clone());
            current = next_abstraction;
        }
        
        vars
    }
    
    pub fn var_at(&self, index: usize) -> Option<&MinlogTerm> {
        if index == 0 {
            Some(&self.var)
        } else if self.kernel.is_abstraction() {
            self.kernel.to_abstraction().unwrap().var_at(index - 1)
        } else {
            None
        }
    }
    
    pub fn kernel(&self) -> &MinlogTerm {
        &self.kernel
    }
    
    pub fn final_kernel(&self) -> &MinlogTerm {
        if let Some(next_abstraction) = self.kernel.to_abstraction() {
            next_abstraction.final_kernel()
        } else {
            &self.kernel
        }
    }
}

impl TermBody for KernelAbstraction {
    fn minlog_type(&self) -> Rc<MinlogType> {
        self.minlog_type.clone()
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> MinlogTerm {
        let kernel = self.kernel.normalize(eta, pi);
        
        if eta && !kernel.contains_free_variable(&self.var) {
            kernel
        } else {
            Abstraction::create(self.var.clone(), kernel)
        }
    }
    
    fn apply_arg(&self, arg: MinlogTerm) -> Option<MinlogTerm> {
        if self.var.minlog_type() != arg.minlog_type() {
            panic!("Tried to apply argument of incompatible type to abstraction");
        }
        
        if arg.contains_free_variable(&self.var) {
            panic!("Tried to apply argument that contains the bound variable");
        }
        
        let subst = TermSubstitution::from_pairs(vec![
            (self.var.clone().into(), arg.into())
        ]);
        
        Some(subst.substitute(&self.kernel))
    }
    
    fn remove_nulls(&self) -> Option<MinlogTerm> {
        if let Some(new_kernel) = self.kernel.remove_nulls() {
            if let Some(new_var) = self.var.remove_nulls() {
                Some(Abstraction::create(new_var, new_kernel))
            } else {
                Some(new_kernel)
            }
        } else {
            None
        }
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
    
    fn get_free_variables(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        self.kernel.get_free_variables(visited).into_iter()
            .filter(|v| v != &self.var)
            .collect()
    }
    
    fn get_bound_variables(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        self.kernel.get_bound_variables(visited)
            .union(&vec![self.var.clone()].into_iter().collect::<IndexSet<_>>()).cloned().collect()
    }
    
    fn get_constructors(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        self.kernel.get_constructors(visited)
    }
    
    fn get_program_terms(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        self.kernel.get_program_terms(visited)
    }
    
    fn alpha_equivalent(&self, other: &MinlogTerm,
        forward: &mut Vec<(TermVariable, TermVariable)>,
        backward: &mut Vec<(TermVariable, TermVariable)>) -> bool
    {
        if !other.is_abstraction() {
            return false;
        }
        
        let other = other.to_abstraction().unwrap();
        
        if self.var.minlog_type() != other.var().minlog_type() {
            return false;
        }
        
        forward.push((self.var.to_variable().unwrap().clone(), other.var().to_variable().unwrap().clone()));
        backward.push((other.var().to_variable().unwrap().clone(), self.var.to_variable().unwrap().clone()));
        
        self.kernel.alpha_equivalent(other.kernel(), forward, backward)
    }

    fn substitute(&self, from: &TermSubstEntry, to: &TermSubstEntry) -> MinlogTerm {
        if let Some(from_tm) = from.to_term() {
            if from_tm.is_abstraction() && Abstraction::Kernel(Rc::new(self.clone())) == *from_tm.to_abstraction().unwrap() {
                to.to_term().unwrap()
            } else if from_tm.is_variable() && from_tm == self.var {
                MinlogTerm::Abstraction(Rc::new(self.clone()).into())
            } else {
                let new_kernel = self.kernel.substitute(from, to);
                Abstraction::create(self.var.clone(), new_kernel)
            }
        } else {
            let new_var = self.var.substitute(from, to);
            let new_kernel = self.kernel.substitute(from, to);
            
            Abstraction::create(new_var, new_kernel)
        }
    }
    
    fn first_conflict_with(&self, other: &MinlogTerm) -> Option<(TermSubstEntry, TermSubstEntry)> {
        if let Some(conflict) = self.minlog_type.first_conflict_with(&other.minlog_type()) {
            return Some((conflict.0.into(), conflict.1.into()));
        }
        
        if !other.is_abstraction() {
            return Some((MinlogTerm::Abstraction(Rc::new(self.clone()).into()).into(), other.clone().into()));
        }
        
        let other_abs = other.to_abstraction().unwrap();
        
        if self.var.minlog_type() != other_abs.var().minlog_type() {
            return Some((self.var.minlog_type().clone().into(), other_abs.var().minlog_type().clone().into()));
        }
        
        let other_kernel = if self.var == *other_abs.var() {
            other_abs.kernel()
        } else {
            &other_abs.kernel().substitute(
                &other_abs.var().clone().into(),
                &self.var.clone().into()
            )
        };
        
        self.kernel.first_conflict_with(other_kernel)
    }

    fn match_with(&self, instance: &MinlogTerm) -> MatchOutput<TermSubstEntry> {
        if !instance.is_abstraction() {
            return MatchOutput::FailedMatch;
        }
        
        let abs_instance = instance.to_abstraction().unwrap();
        
        let conditions = IndexMap::from([
            (self.var.clone().into(), abs_instance.var().clone().into()),
            (self.kernel.clone().into(), abs_instance.kernel().clone().into()),
        ]);
        
        MatchOutput::Matched(conditions)
    }
}

impl PrettyPrintable for KernelAbstraction {
    fn to_pp_element(&self, detail: bool) -> PPElement {        
        let elements = vec![
            PPElement::group(vec![
                PPElement::text("[".to_string()),
                PPElement::break_elem(1, 4, false),
                self.var.to_pp_element(detail),
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

pub trait NativeAbstraction: NativeTermBody {
    fn var(&self) -> &MinlogTerm;
    
    fn all_vars(&self) -> Vec<MinlogTerm>;
    
    fn var_at(&self, index: usize) -> Option<&MinlogTerm>;
    
    fn kernel(&self) -> &MinlogTerm;
    
    fn final_kernel(&self) -> &MinlogTerm;
    
    fn to_kernel(&self) -> KernelAbstraction {
        KernelAbstraction {
            var: self.var().clone(),
            kernel: self.kernel().clone(),
            minlog_type: self.minlog_type(),
        }
    }
}

wrapper_enum::wrapper_enum! {
    #[derive(Clone)]
    pub enum Abstraction {
        Kernel(kernel: Rc<KernelAbstraction>),
        Native(native: Rc<dyn NativeAbstraction>),
    }
    
    ext trait TermBody: PrettyPrintable {
        fwd fn minlog_type(&self) -> Rc<MinlogType>
    
        fwd fn normalize(&self, eta: bool, pi: bool) -> MinlogTerm
    
        fwd fn apply_arg(&self, arg: MinlogTerm) -> Option<MinlogTerm>
    
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
    
    fwd trait AbstractionForwards {
        pub fwd fn var(&self) -> &MinlogTerm
        
        pub fwd fn all_vars(&self) -> Vec<MinlogTerm>
        
        pub fwd fn var_at(&self, index: usize) -> Option<&MinlogTerm>
        
        pub fwd fn kernel(&self) -> &MinlogTerm
        
        pub fwd fn final_kernel(&self) -> &MinlogTerm
    }
    
    ext trait PrettyPrintable {
        fwd fn to_pp_element(&self, detail: bool) -> PPElement

        fwd fn requires_parens(&self, detail: bool) -> bool

        fwd fn open_paren(&self) -> String

        fwd fn close_paren(&self) -> String
    }
}

impl Abstraction {
    pub fn create(var: MinlogTerm, kernel: MinlogTerm) -> MinlogTerm {
        KernelAbstraction::create(var, kernel)
    }
    
    pub fn create_nested(vars: Vec<MinlogTerm>, kernel: MinlogTerm) -> MinlogTerm {
        let mut result = kernel;
        
        for var in vars.into_iter().rev() {
            result = Abstraction::create(var, result);
        }
        
        result
    }
    
    pub fn into_kernel_abstraction(self) -> Rc<KernelAbstraction> {
        match self {
            Abstraction::Kernel(k) => k,
            Abstraction::Native(n) => Rc::new(n.to_kernel()),
        }
    }
}

impl Hash for Abstraction {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Abstraction::Kernel(k) => k.hash(state),
            Abstraction::Native(n) => n.to_kernel().hash(state),
        }
    }
}

impl PartialEq for Abstraction {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Abstraction::Kernel(k1), Abstraction::Kernel(k2)) => k1 == k2,
            (Abstraction::Native(n1), Abstraction::Kernel(k2)) => n1.to_kernel() == *k2.as_ref(),
            (Abstraction::Kernel(k1), Abstraction::Native(n2)) => *k1.as_ref() == n2.to_kernel(),
            (Abstraction::Native(n1), Abstraction::Native(n2)) => n1.eq(n2.as_ref()),
        }
    }
}

impl Eq for Abstraction {}

impl From<Rc<KernelAbstraction>> for Abstraction {
    fn from(k: Rc<KernelAbstraction>) -> Self {
        Abstraction::Kernel(k)
    }
}

impl From<&Rc<KernelAbstraction>> for Abstraction {
    fn from(k: &Rc<KernelAbstraction>) -> Self {
        Abstraction::Kernel(k.clone())
    }
}

impl From<Rc<dyn NativeAbstraction>> for Abstraction {
    fn from(n: Rc<dyn NativeAbstraction>) -> Self {
        Abstraction::Native(n)
    }
}

impl From<&Rc<dyn NativeAbstraction>> for Abstraction {
    fn from(n: &Rc<dyn NativeAbstraction>) -> Self {
        Abstraction::Native(n.clone())
    }
}