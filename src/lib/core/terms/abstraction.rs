
use crate::includes::{
    essential::*,
    utils::*,
    core::{
        types::*,
        terms::*,
    }
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct KernelAbstraction {
    vars: Vec<MinlogTerm>,
    kernel: MinlogTerm,
    minlog_type: Rc<MinlogType>,
}

impl KernelAbstraction {
    pub fn create(vars: Vec<MinlogTerm>, kernel: MinlogTerm) -> MinlogTerm {
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
        KernelAbstraction::collapse(&MinlogTerm::Abstraction(Rc::new(KernelAbstraction {
            minlog_type: ArrowType::create(var_types, kernel.minlog_type()),
            vars,
            kernel,
        }).into()))
    }
    
    pub fn collapse(minlog_term: &MinlogTerm) -> MinlogTerm {
        if !minlog_term.is_abstraction() || !minlog_term.to_abstraction().unwrap().kernel().is_abstraction() {
            minlog_term.clone()
        } else {
            let mut abstraction = minlog_term.to_abstraction().unwrap();
            let mut vars = abstraction.vars().clone();
            
            while abstraction.kernel().is_abstraction() {
                let next_abstraction = abstraction.kernel().to_abstraction().unwrap();
                vars.extend(next_abstraction.vars().clone());
                abstraction = next_abstraction;
            }
            
            Abstraction::create(vars, abstraction.kernel().clone())
        }
    }
    
    pub fn closure(minlog_term: &MinlogTerm) -> MinlogTerm {
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
    
    pub fn vars(&self) -> &Vec<MinlogTerm> {
        &self.vars
    }
    
    pub fn var(&self, index: usize) -> Option<&MinlogTerm> {
        self.vars.get(index)
    }
    
    pub fn kernel(&self) -> &MinlogTerm {
        &self.kernel
    }
}

impl TermBody for KernelAbstraction {
    fn minlog_type(&self) -> Rc<MinlogType> {
        self.minlog_type.clone()
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> MinlogTerm {
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
    
    fn apply_args(&self, args: &Vec<MinlogTerm >) -> Option<MinlogTerm> {
        let applicable_args = min(args.len(), self.vars.len());
        let mut subst = TermSubstitution::make_empty();
        
        for (var, arg) in self.vars[..applicable_args].iter().zip(args[..applicable_args].iter()) {
            if var.minlog_type() != arg.minlog_type() {
                panic!("Tried to apply argument of incompatible type to abstraction");
            }
            
            if arg.contains_free_variable(var) {
                panic!("Tried to apply argument that contains the bound variable");
            }
            
            subst.extend((var.clone().into(), arg.clone().into()));
        }
        
        let applied_kernel = subst.substitute(&self.kernel);
        
        let remaining_vars = self.vars[applicable_args..].to_vec();
        
        let remaining_abs = if remaining_vars.is_empty() {
            applied_kernel
        } else {
            Abstraction::create(remaining_vars, applied_kernel)
        };
        
        let remaining_args = args[applicable_args..].to_vec();
        
        if remaining_args.is_empty() {
            Some(remaining_abs)
        } else {
            Some(KernelApplication::create(remaining_abs, remaining_args))
        }
    }
    
    fn remove_nulls(&self) -> Option<MinlogTerm> {
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
    
    fn get_free_variables(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        self.kernel.get_free_variables(visited).into_iter()
            .filter(|v| !self.vars.contains(v))
            .collect()
    }
    
    fn get_bound_variables(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        self.kernel.get_bound_variables(visited).union(&self.vars.iter().cloned().collect::<IndexSet<_>>()).cloned().collect()
    }
    
    fn get_constructors(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        self.kernel.get_constructors(visited)
    }
    
    fn get_program_terms(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        self.kernel.get_program_terms(visited)
    }
    
    fn alpha_equivalent(&self, other: &MinlogTerm,
        forward: &mut Vec<(TermVariable, TermVariable)>,
        backward: &mut Vec<(TermVariable, TermVariable)>) -> bool {
        
        if !other.is_abstraction() {
            return false;
        }
        
        let other = other.to_abstraction().unwrap();
        
        if self.vars.len() != other.vars().len() {
            return false;
        }
        
        for (v1, v2) in self.vars.iter().zip(other.vars().iter()) {
            if v1.minlog_type() != v2.minlog_type() {
                return false;
            }
            
            forward.push((v1.to_variable().unwrap().clone(), v2.to_variable().unwrap().clone()));
            backward.push((v2.to_variable().unwrap().clone(), v1.to_variable().unwrap().clone()));
        }
        
        self.kernel.alpha_equivalent(other.kernel(), forward, backward)
    }

    fn substitute(&self, from: &TermSubstEntry, to: &TermSubstEntry) -> MinlogTerm {
        if let Some(from_tm) = from.to_term() {
            if from_tm.is_abstraction() && Abstraction::Kernel(Rc::new(self.clone())) == *from_tm.to_abstraction().unwrap() {
                to.to_term().unwrap()
            } else if from_tm.is_variable() && self.vars.contains(&from_tm) {
                MinlogTerm::Abstraction(Rc::new(self.clone()).into())
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
    
    fn first_conflict_with(&self, other: &MinlogTerm) -> Option<(TermSubstEntry, TermSubstEntry)> {
        if let Some(conflict) = self.minlog_type.first_conflict_with(&other.minlog_type()) {
            return Some((conflict.0.into(), conflict.1.into()));
        }
        
        if !other.is_abstraction() {
            return Some((MinlogTerm::Abstraction(Rc::new(self.clone()).into()).into(), other.clone().into()));
        }
        
        let other_abs = other.to_abstraction().unwrap();

        if self.vars.len() != other_abs.vars().len() {
            return Some((MinlogTerm::Abstraction(Rc::new(self.clone()).into()).into(), other.clone().into()));
        }
        
        let mut subst = TermSubstitution::make_empty();
        
        for (v1, v2) in self.vars.iter().zip(other_abs.vars().iter()) {
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
            self.kernel.first_conflict_with(t.to_abstraction().unwrap().kernel())
        } else {
            panic!("Substitution of abstraction resulted in type.");
        }
    }

    fn match_with(&self, instance: &MinlogTerm) -> MatchOutput<TermSubstEntry> {
        if !instance.is_abstraction() {
            return MatchOutput::FailedMatch;
        }
        
        let abs_instance = instance.to_abstraction().unwrap();
        
        if self.arity() != abs_instance.arity() {
            return MatchOutput::FailedMatch;
        }
        
        let mut conditions = self.vars.iter().zip(abs_instance.vars().iter())
            .filter_map(|(v1, v2)| {
                if v1 != v2 {
                    Some((v1.into(), v2.into()))
                } else {
                    None
                }
            }).collect::<IndexMap<_, _>>();
            
        if self.kernel != *abs_instance.kernel() {
            conditions.insert(self.kernel.clone().into(), abs_instance.kernel().clone().into());
        }
        
        MatchOutput::Matched(conditions)
    }
}

impl PrettyPrintable for KernelAbstraction {
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

pub trait NativeAbstraction: NativeTermBody {
    fn arity(&self) -> usize;
    
    fn vars(&self) -> &Vec<MinlogTerm>;
    
    fn var(&self, index: usize) -> Option<&MinlogTerm>;
    
    fn kernel(&self) -> &MinlogTerm;
    
    fn to_kernel(&self) -> KernelAbstraction {
        KernelAbstraction {
            vars: self.vars().clone(),
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
    
    fwd trait AbstractionForwards {
        pub fwd fn arity(&self) -> usize
        
        pub fwd fn vars(&self) -> &Vec<MinlogTerm>
        
        pub fwd fn var(&self, index: usize) -> Option<&MinlogTerm>
        
        pub fwd fn kernel(&self) -> &MinlogTerm
    }
    
    ext trait PrettyPrintable {
        fwd fn to_pp_element(&self, detail: bool) -> PPElement

        fwd fn requires_parens(&self, detail: bool) -> bool

        fwd fn open_paren(&self) -> String

        fwd fn close_paren(&self) -> String
    }
}

impl Abstraction {
    pub fn create(vars: Vec<MinlogTerm>, kernel: MinlogTerm) -> MinlogTerm {
        KernelAbstraction::create(vars, kernel)
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

impl Debug for Abstraction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Abstraction::Kernel(k) => write!(f, "{:?}", k),
            Abstraction::Native(n) => write!(f, "Native({:?})", n.native_to_minlog()),
        }
    }
}

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