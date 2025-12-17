
use crate::includes::{
    essential::*,
    utils::*,
    core::{
        types::*,
        terms::*,
    }
};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct KernelTermVariable {
    name: String,
    minlog_type: Rc<MinlogType>,
    index: usize,
}

impl KernelTermVariable {
    pub fn create(name: String, minlog_type: Rc<MinlogType>) -> MinlogTerm {
        MinlogTerm::Variable(Rc::new(KernelTermVariable { name, minlog_type, index: 0 }).into())
    }
    
    pub fn unshadow(var: &MinlogTerm) -> MinlogTerm {
        if let Some(tv) = var.to_variable() {
            MinlogTerm::Variable(Rc::new(KernelTermVariable {
                name: tv.name().to_string(),
                minlog_type: Rc::clone(&tv.minlog_type()),
                index: tv.index() + 1,
            }).into())
        } else {
            panic!("Called TermVariable::unshadow on a non-variable term");
        }
    }
    
    pub fn name(&self) -> &str {
        &self.name
    }
    
    pub fn index(&self) -> usize {
        self.index
    }
}

impl TermBody for KernelTermVariable {
    fn minlog_type(&self) -> Rc<MinlogType> {
        self.minlog_type.clone()
    }
    
    fn normalize(&self, _eta: bool, _pi: bool) -> MinlogTerm {
        MinlogTerm::Variable(Rc::new(self.clone()).into())
    }
    
    fn remove_nulls(&self) -> Option<MinlogTerm> {
        self.minlog_type.remove_nulls().map(|new_type| {
            MinlogTerm::Variable(Rc::new(KernelTermVariable {
                name: self.name.clone(),
                minlog_type: new_type,
                index: self.index,
            }).into())
        })
    }
    
    fn length(&self) -> usize {
        1
    }
    
    fn constructor_pattern(&self) -> bool {
        true
    }

    fn get_type_variables(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogType>> {
        self.minlog_type.get_type_variables(&mut IndexSet::new())
    }
    
    fn get_algebra_types(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogType>> {
        self.minlog_type.get_algebra_types(&mut IndexSet::new())
    }
    
    fn get_free_variables(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        IndexSet::from([MinlogTerm::Variable(Rc::new(self.clone()).into())])
    }
    
    fn alpha_equivalent(&self, other: &MinlogTerm,
        forward: &mut Vec<(TermVariable, TermVariable)>,
        backward: &mut Vec<(TermVariable, TermVariable)>) -> bool {
        
        if !other.is_variable() {
            return false;
        }
        
        let other = other.to_variable().unwrap();
        
        let self_as_tv = TermVariable::Kernel(Rc::new(self.clone()));
        
        let forward_pair = forward.iter().find(|(v1, _)| v1 == &self_as_tv);
        let backward_pair = backward.iter().find(|(v2, _)| v2 == other);
        
        match (forward_pair, backward_pair) {
            (Some((f1, f2)), Some((b2, b1))) => f1 == b1 && f2 == b2,
            (None, None) => TermVariable::Kernel(Rc::new(self.clone())) == *other,
            _ => false,
        }
    }

    fn substitute(&self, from: &TermSubstEntry, to: &TermSubstEntry) -> MinlogTerm {
        match from {
            TermSubstEntry::Type(from_t) => {
                MinlogTerm::Variable(Rc::new(KernelTermVariable {
                    name: self.name.clone(),
                    minlog_type: self.minlog_type.substitute(from_t, &to.to_type().unwrap()),
                    index: self.index,
                }).into())
            },
            TermSubstEntry::Term(from_tm) => {
                if from_tm.is_variable() && TermVariable::Kernel(Rc::new(self.clone())) == *from_tm.to_variable().unwrap() {
                    to.to_term().unwrap()
                } else {
                    MinlogTerm::Variable(Rc::new(self.clone()).into())
                }
            }
        }
    }
    
    fn first_conflict_with(&self, other: &MinlogTerm) -> Option<(TermSubstEntry, TermSubstEntry)> {
        if let Some(conflict) = self.minlog_type.first_conflict_with(&other.minlog_type()) {
            return Some((conflict.0.into(), conflict.1.into()));
        }
        
        if other.is_variable() && TermVariable::Kernel(Rc::new(self.clone())) == *other.to_variable().unwrap() {
            None
        } else {
            Some((MinlogTerm::Variable(Rc::new(self.clone()).into()).into(), other.clone().into()))
        }
    }
    
    fn match_with(&self, instance: &MinlogTerm) -> MatchOutput<TermSubstEntry> {
        if self.minlog_type() != instance.minlog_type() {
            MatchOutput::Matched(IndexMap::from([
                (MinlogTerm::Variable(Rc::new(self.clone()).into()).into(), instance.clone().into()),
                (self.minlog_type().into(), instance.minlog_type().into())
            ]))
        } else {
            MatchOutput::Substitution(MinlogTerm::Variable(Rc::new(self.clone()).into()).into(), instance.clone().into())
        }
    }
}

impl PrettyPrintable for KernelTermVariable {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        if detail {
            PPElement::group(vec![
                PPElement::text(self.name.clone()),
                if self.index > 0 {
                    PPElement::text(format!("_{}", self.index))
                } else {
                    PPElement::break_elem(0, 0, false)
                },
                PPElement::text(":".to_string()),
                PPElement::break_elem(1, 0, false),
                self.minlog_type.to_pp_element(false)
            ], BreakType::Flexible, 0)
        } else {
            PPElement::text(if self.index > 0 {
                format!("{}_{}", self.name, self.index)
            } else {
                self.name.clone()
            })
        }
    }
    
    fn requires_parens(&self, detail: bool) -> bool {
        detail
    }
    
    fn open_paren(&self) -> String {
        "(".to_string()
    }
    
    fn close_paren(&self) -> String {
        ")".to_string()
    }
}

pub trait NativeTermVariable: NativeTermBody {
    fn name(&self) -> &str;
    
    fn index(&self) -> usize;
    
    fn to_kernel(&self) -> KernelTermVariable {
        KernelTermVariable {
            name: self.name().to_string(),
            minlog_type: self.minlog_type(),
            index: self.index(),
        }
    }
}

wrapper_enum::wrapper_enum! {
    #[derive(Clone)]
    pub enum TermVariable {
        Kernel(kernel: Rc<KernelTermVariable>),
        Native(native: Rc<dyn NativeTermVariable>),
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
    
    fwd trait VariableForwards {
        pub fwd fn name(&self) -> &str
        
        pub fwd fn index(&self) -> usize
    }
    
    ext trait PrettyPrintable {
        fwd fn to_pp_element(&self, detail: bool) -> PPElement

        fwd fn requires_parens(&self, detail: bool) -> bool

        fwd fn open_paren(&self) -> String

        fwd fn close_paren(&self) -> String
    }
}

impl TermVariable {
    pub fn create(name: String, minlog_type: Rc<MinlogType>) -> MinlogTerm {
        KernelTermVariable::create(name, minlog_type)
    }
    
    pub fn into_kernel_variable(self) -> Rc<KernelTermVariable> {
        match self {
            TermVariable::Kernel(kv) => kv,
            TermVariable::Native(nv) => Rc::new(nv.to_kernel()),
        }
    }
}

impl Hash for TermVariable {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            TermVariable::Kernel(kv) => kv.hash(state),
            TermVariable::Native(nv) => nv.to_kernel().hash(state),
        }
    }
}

impl PartialEq for TermVariable {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TermVariable::Kernel(kv1), TermVariable::Kernel(kv2)) => kv1 == kv2,
            (TermVariable::Native(nv1), TermVariable::Kernel(kv2)) => nv1.to_kernel() == *kv2.as_ref(),
            (TermVariable::Kernel(kv1), TermVariable::Native(nv2)) => *kv1.as_ref() == nv2.to_kernel(),
            (TermVariable::Native(nv1), TermVariable::Native(nv2)) => nv1.eq(nv2.as_ref()),
        }
    }
}

impl Eq for TermVariable {}

impl From<Rc<KernelTermVariable>> for TermVariable {
    fn from(kv: Rc<KernelTermVariable>) -> Self {
        TermVariable::Kernel(kv)
    }
}

impl From<&Rc<KernelTermVariable>> for TermVariable {
    fn from(kv: &Rc<KernelTermVariable>) -> Self {
        TermVariable::Kernel(kv.clone())
    }
}

impl From<Rc<dyn NativeTermVariable>> for TermVariable {
    fn from(nv: Rc<dyn NativeTermVariable>) -> Self {
        TermVariable::Native(nv)
    }
}

impl From<&Rc<dyn NativeTermVariable>> for TermVariable {
    fn from(nv: &Rc<dyn NativeTermVariable>) -> Self {
        TermVariable::Native(nv.clone())
    }
}