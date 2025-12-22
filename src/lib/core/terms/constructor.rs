
use crate::includes::{
    essential::*,
    utils::*,
    core::{
        types::*,
        terms::*,
    }
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct KernelConstructor {
    name: String,
    minlog_type: Rc<MinlogType>,
}

impl KernelConstructor {
    pub fn create(name: String, minlog_type: Rc<MinlogType>) -> MinlogTerm {
        if name.is_empty() {
            panic!("Constructor name cannot be empty");
        }
        
        if !minlog_type.is_algebra() && (!minlog_type.is_arrow() ||
            !minlog_type.to_arrow().unwrap().value().is_algebra()) {
            panic!("Constructor type must be an algebra type or an arrow type ending in an algebra type");
        }
        
        MinlogTerm::Constructor(Rc::new(KernelConstructor {
            name,
            minlog_type,
        }).into())
    }
    
    pub fn name(&self) -> &str {
        &self.name
    }
}

impl TermBody for KernelConstructor {
    fn minlog_type(&self) -> Rc<MinlogType> {
        self.minlog_type.clone()
    }
    
    fn normalize(&self, _eta: bool, _pi: bool) -> MinlogTerm {
        Constructor::create(self.name.clone(), self.minlog_type.clone())
    }
    
    fn remove_nulls(&self) -> Option<MinlogTerm> {
        let algebra_type = if let Some(algebra) = self.minlog_type.to_algebra() {
            algebra
        } else if let Some(arrow) = self.minlog_type.to_arrow() {
            arrow.value().to_algebra()?
        } else {
            return None;
        };
        
        if let Some(reduction) = algebra_type.get_reduction() {
            let new_name = reduction.constructor_mapping.get(&self.name).unwrap().clone();
            let new_type = self.minlog_type.remove_nulls().unwrap();
            Some(Constructor::create(new_name, new_type))
        } else {
            Some(Constructor::create(self.name.clone(), self.minlog_type.clone()))
        }
    }
    
    fn length(&self) -> usize {
        1
    }
    
    fn depth(&self) -> usize {
        0
    }
    
    fn constructor_pattern(&self) -> bool {
        self.minlog_type.is_algebra()
    }
    
    fn get_type_variables(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogType>> {
        self.minlog_type.get_type_variables(&mut IndexSet::new())
    }
    
    fn get_algebra_types(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogType>> {
        self.minlog_type.get_algebra_types(&mut IndexSet::new())
    }
    
    fn get_constructors(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        IndexSet::from([MinlogTerm::Constructor(Rc::new(self.clone()).into())])
    }
    
    fn alpha_equivalent(&self, other: &MinlogTerm,
        _forward: &mut Vec<(TermVariable, TermVariable)>,
        _backward: &mut Vec<(TermVariable, TermVariable)>) -> bool {
        
        if let Some(other_body) = other.to_constructor() {
            self.name == other_body.name() && self.minlog_type == other_body.minlog_type()
        } else {
            false
        }
    }
    
    fn substitute(&self, from: &TermSubstEntry, to: &TermSubstEntry) -> MinlogTerm {
        match from {
            TermSubstEntry::Type(from_t) => {
                let new_type = self.minlog_type.substitute(from_t, &to.to_type().unwrap());
                Constructor::create(self.name.clone(), new_type)
            },
            TermSubstEntry::Term(from_tm) => {
                if from_tm.is_constructor() && Constructor::Kernel(Rc::new(self.clone())) == *from_tm.to_constructor().unwrap() {
                    to.to_term().unwrap()
                } else {
                    Constructor::create(self.name.clone(), self.minlog_type.clone())
                }
            }
        }
    }
    
    fn first_conflict_with(&self, other: &MinlogTerm) -> Option<(TermSubstEntry, TermSubstEntry)> {
        if let Some(conflict) = self.minlog_type.first_conflict_with(&other.minlog_type()) {
            return Some((conflict.0.into(), conflict.1.into()));
        }
        
        if !other.is_constructor() {
            return Some((MinlogTerm::Constructor(Rc::new(self.clone()).into()).into(), other.clone().into()));
        }
        
        let other_constr = other.to_constructor().unwrap();
        
        if self.name != other_constr.name() || self.minlog_type != other_constr.minlog_type() {
            return Some((MinlogTerm::Constructor(Rc::new(self.clone()).into()).into(), other.clone().into()));
        }
        None
    }
    
    fn match_with(&self, instance: &MinlogTerm) -> MatchOutput<TermSubstEntry> {
        if !instance.is_constructor() {
            return MatchOutput::FailedMatch;
        }
        
        let constr_instance = instance.to_constructor().unwrap();
        
        if self.name != constr_instance.name() {
            return MatchOutput::FailedMatch;
        }
        
        let conditions = if self.minlog_type != constr_instance.minlog_type() {
            IndexMap::from([(self.minlog_type.clone().into(), constr_instance.minlog_type().clone().into())])
        } else {
            IndexMap::new()
        };
        
        MatchOutput::Matched(conditions)
    }
}

impl PrettyPrintable for KernelConstructor {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        if detail {
            PPElement::group(vec![
                PPElement::text(self.name.clone()),
                PPElement::text(":".to_string()),
                PPElement::break_elem(1, 4, false),
                self.minlog_type.to_pp_element(true),
            ], BreakType::Flexible, 0)
        } else {
            PPElement::text(self.name.clone())
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

pub trait NativeConstructor: NativeTermBody {
    fn name(&self) -> &str;
    
    fn to_kernel(&self) -> KernelConstructor {
        KernelConstructor {
            name: self.name().to_string(),
            minlog_type: self.minlog_type(),
        }
    }
}

wrapper_enum::wrapper_enum! {
    #[derive(Clone)]
    pub enum Constructor {
        Kernel(kernel: Rc<KernelConstructor>),
        Native(native: Rc<dyn NativeConstructor>),
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
    
    fwd trait ConstructorForwards {
        pub fwd fn name(&self) -> &str
    }
    
    ext trait PrettyPrintable {
        fwd fn to_pp_element(&self, detail: bool) -> PPElement

        fwd fn requires_parens(&self, detail: bool) -> bool

        fwd fn open_paren(&self) -> String

        fwd fn close_paren(&self) -> String
    }
}

impl Constructor {
    pub fn create(name: String, minlog_type: Rc<MinlogType>) -> MinlogTerm {
        KernelConstructor::create(name, minlog_type)
    }
    
    pub fn into_kernel_constructor(self) -> Rc<KernelConstructor> {
        match self {
            Constructor::Kernel(k) => k,
            Constructor::Native(n) => Rc::new(n.to_kernel()),
        }
    }
}

impl Hash for Constructor {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Constructor::Kernel(k) => k.hash(state),
            Constructor::Native(n) => n.to_kernel().hash(state),
        }
    }
}

impl PartialEq for Constructor {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Constructor::Kernel(k1), Constructor::Kernel(k2)) => k1 == k2,
            (Constructor::Native(n1), Constructor::Kernel(k2)) => n1.to_kernel() == *k2.as_ref(),
            (Constructor::Kernel(k1), Constructor::Native(n2)) => *k1.as_ref() == n2.to_kernel(),
            (Constructor::Native(n1), Constructor::Native(n2)) => n1.eq(n2.as_ref()),
        }
    }
}

impl Eq for Constructor {}

impl Debug for Constructor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constructor::Kernel(k) => write!(f, "{:?}", k),
            Constructor::Native(n) => write!(f, "Native({:?})", n.native_to_minlog()),
        }
    }
}

impl From<Rc<KernelConstructor>> for Constructor {
    fn from(k: Rc<KernelConstructor>) -> Self {
        Constructor::Kernel(k)
    }
}

impl From<&Rc<KernelConstructor>> for Constructor {
    fn from(k: &Rc<KernelConstructor>) -> Self {
        Constructor::Kernel(k.clone())
    }
}

impl From<Rc<dyn NativeConstructor>> for Constructor {
    fn from(n: Rc<dyn NativeConstructor>) -> Self {
        Constructor::Native(n)
    }
}

impl From<&Rc<dyn NativeConstructor>> for Constructor {
    fn from(n: &Rc<dyn NativeConstructor>) -> Self {
        Constructor::Native(n.clone())
    }
}