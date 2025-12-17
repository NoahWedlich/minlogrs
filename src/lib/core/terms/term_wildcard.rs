
use crate::includes::{
    essential::*,
    utils::*,
    core::{
        types::*,
        terms::*,
    }
};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct KernelTermWildcard {
    minlog_type: Rc<MinlogType>,
}

impl KernelTermWildcard {
    pub fn create(minlog_type: Rc<MinlogType>) -> MinlogTerm {
        MinlogTerm::Wildcard(Rc::new(KernelTermWildcard { minlog_type }).into())
    }
}

impl TermBody for KernelTermWildcard {
    fn minlog_type(&self) -> Rc<MinlogType> {
        self.minlog_type.clone()
    }
    
    fn normalize(&self, _eta: bool, _pi: bool) -> MinlogTerm {
        MinlogTerm::Wildcard(Rc::new(self.clone()).into())
    }
    
    fn remove_nulls(&self) -> Option<MinlogTerm> {
        self.minlog_type.remove_nulls().map(|new_type| {
            TermWildcard::create(new_type)
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
    
    fn alpha_equivalent(&self, other: &MinlogTerm,
        _forward: &mut Vec<(TermVariable, TermVariable)>,
        _backward: &mut Vec<(TermVariable, TermVariable)>) -> bool
    {
        other.is_wildcard() && self.minlog_type == other.minlog_type()
    }

    fn substitute(&self, from: &TermSubstEntry, to: &TermSubstEntry) -> MinlogTerm {
        match from {
            TermSubstEntry::Type(from_t) => {
                MinlogTerm::Wildcard(Rc::new(KernelTermWildcard {
                    minlog_type: self.minlog_type.substitute(from_t, &to.to_type().unwrap()),
                }).into())
            },
            _ => {
                MinlogTerm::Wildcard(Rc::new(self.clone()).into())
            }
        }
    }
    
    fn first_conflict_with(&self, other: &MinlogTerm) -> Option<(TermSubstEntry, TermSubstEntry)> {
        if let Some(conflict) = self.minlog_type.first_conflict_with(&other.minlog_type()) {
            return Some((conflict.0.into(), conflict.1.into()));
        }
        
        None
    }
    
    fn match_with(&self, instance: &MinlogTerm) -> MatchOutput<TermSubstEntry> {
        let conditions = if self.minlog_type() != instance.minlog_type() {
            IndexMap::from([(self.minlog_type.clone().into(), instance.minlog_type().clone().into())])
        } else {
            IndexMap::new()
        };

        MatchOutput::Matched(conditions)
    }
}

impl PrettyPrintable for KernelTermWildcard {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        if detail {
            PPElement::group(vec![
                PPElement::text("_:".to_string()),
                PPElement::break_elem(1, 0, false),
                self.minlog_type.to_pp_element(false)
            ], BreakType::Flexible, 0)
        } else {
            PPElement::text("_".to_string())
        }
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}

pub trait NativeTermWildcard: NativeTermBody {
    fn to_kernel(&self) -> KernelTermWildcard {
        KernelTermWildcard {
            minlog_type: self.minlog_type(),
        }
    }
}

wrapper_enum::wrapper_enum! {
    #[derive(Clone)]
    pub enum TermWildcard {
        Kernel(kernel: Rc<KernelTermWildcard>),
        Native(native: Rc<dyn NativeTermWildcard>),
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
    
    fwd trait WildcardForwards {
    }
    
    ext trait PrettyPrintable {
        fwd fn to_pp_element(&self, detail: bool) -> PPElement

        fwd fn requires_parens(&self, detail: bool) -> bool

        fwd fn open_paren(&self) -> String

        fwd fn close_paren(&self) -> String
    }
}

impl TermWildcard {
    pub fn create(minlog_type: Rc<MinlogType>) -> MinlogTerm {
        KernelTermWildcard::create(minlog_type)
    }
    
    pub fn into_kernel_wildcard(self) -> Rc<KernelTermWildcard> {
        match self {
            TermWildcard::Kernel(kw) => kw,
            TermWildcard::Native(nw) => Rc::new(nw.to_kernel()),
        }
    }
}

impl Hash for TermWildcard {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            TermWildcard::Kernel(kw) => kw.hash(state),
            TermWildcard::Native(nw) => nw.to_kernel().hash(state),
        }
    }
}

impl PartialEq for TermWildcard {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TermWildcard::Kernel(kw1), TermWildcard::Kernel(kw2)) => kw1 == kw2,
            (TermWildcard::Native(nw1), TermWildcard::Kernel(kw2)) => nw1.to_kernel() == *kw2.as_ref(),
            (TermWildcard::Kernel(kw1), TermWildcard::Native(nw2)) => *kw1.as_ref() == nw2.to_kernel(),
            (TermWildcard::Native(nw1), TermWildcard::Native(nw2)) => nw1.eq(nw2.as_ref()),
        }
    }
}

impl Eq for TermWildcard {}

impl From<Rc<KernelTermWildcard>> for TermWildcard {
    fn from(kw: Rc<KernelTermWildcard>) -> Self {
        TermWildcard::Kernel(kw)
    }
}

impl From<&Rc<KernelTermWildcard>> for TermWildcard {
    fn from(kw: &Rc<KernelTermWildcard>) -> Self {
        TermWildcard::Kernel(kw.clone())
    }
}

impl From<Rc<dyn NativeTermWildcard>> for TermWildcard {
    fn from(nw: Rc<dyn NativeTermWildcard>) -> Self {
        TermWildcard::Native(nw)
    }
}

impl From<&Rc<dyn NativeTermWildcard>> for TermWildcard {
    fn from(nw: &Rc<dyn NativeTermWildcard>) -> Self {
        TermWildcard::Native(nw.clone())
    }
}