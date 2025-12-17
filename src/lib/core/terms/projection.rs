
use crate::includes::{
    essential::*,
    utils::*,
    core::{
        types::*,
        terms::*,
    }
};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct KernelProjection {
    term: MinlogTerm,
    index: usize,
    minlog_type: Rc<MinlogType>,
}

impl KernelProjection {
    pub fn create(term: MinlogTerm, index: usize) -> MinlogTerm {
        if !term.minlog_type().is_tuple() {
            panic!("Tried to create projection from non-tuple term.");
        }
        
        if index >= term.minlog_type().to_tuple().unwrap().types().len() {
            panic!("Tried to create projection with out-of-bounds index.");
        }
        
        let minlog_type = term.minlog_type().to_tuple().unwrap().type_at(index).unwrap().clone();
        MinlogTerm::Projection(Rc::new(KernelProjection { term, index, minlog_type }).into())
    }
    
    pub fn term(&self) -> &MinlogTerm {
        &self.term
    }
    
    pub fn index(&self) -> usize {
        self.index
    }
}

impl TermBody for KernelProjection {
    fn minlog_type(&self) -> Rc<MinlogType> {
        self.minlog_type.clone()
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> MinlogTerm {
        if self.term.is_tuple() {
            let tuple = self.term.to_tuple().unwrap();
            if self.index < tuple.elements().len() {
                return tuple.element(self.index).unwrap().normalize(eta, pi);
            } else {
                panic!("Projection index out of bounds in normalization.");
            }
        }
        
        let new_term = self.term.normalize(eta, pi);
        Projection::create(new_term, self.index)
    }
    
    fn remove_nulls(&self) -> Option<MinlogTerm> {
        if let Some(tuple) = self.term.to_tuple() {
            let mut new_index = self.index;
            
            for i in 0..self.index {
                if tuple.element(i).unwrap().remove_nulls().is_none() {
                    new_index -= 1;
                }
            }
            
            self.term.remove_nulls().map(|new_term| {
                Projection::create(new_term, new_index)
            })
        } else {
            self.term.remove_nulls().map(|new_term| {
                Projection::create(new_term, self.index)
            })
        }
    }
    
    fn length(&self) -> usize {
        1 + self.term.length()
    }
    
    fn depth(&self) -> usize {
        1 + self.term.depth()
    }
    
    fn get_type_variables(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogType>> {
        self.minlog_type.get_type_variables(&mut IndexSet::new())
    }
    
    fn get_algebra_types(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogType>> {
        self.minlog_type.get_algebra_types(&mut IndexSet::new())
    }
    
    fn get_free_variables(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        self.term.get_free_variables(visited)
    }

    fn get_bound_variables(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        self.term.get_bound_variables(visited)
    }
    
    fn get_constructors(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        self.term.get_constructors(visited)
    }

    fn get_program_terms(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        self.term.get_program_terms(visited)
    }
    
    fn alpha_equivalent(&self, other: &MinlogTerm,
        forward: &mut Vec<(TermVariable, TermVariable)>,
        backward: &mut Vec<(TermVariable, TermVariable)>) -> bool {
        
        if !other.is_projection() {
            return false;
        }
        
        let other = other.to_projection().unwrap();
        
        self.index == other.index() && self.term.alpha_equivalent(other.term(), forward, backward)
    }
    
    fn substitute(&self, from: &TermSubstEntry, to: &TermSubstEntry) -> MinlogTerm {
        if let Some(tm) = from.to_term() && tm.is_projection() && Projection::Kernel(Rc::new(self.clone())) == *tm.to_projection().unwrap() {
            to.to_term().unwrap()
        } else {
            let new_term = self.term.substitute(from, to);
            Projection::create(new_term, self.index)
        }
    }
    
    fn first_conflict_with(&self, other: &MinlogTerm) -> Option<(TermSubstEntry, TermSubstEntry)> {
        if let Some(conflict) = self.minlog_type.first_conflict_with(&other.minlog_type()) {
            return Some((conflict.0.into(), conflict.1.into()));
        }
        
        if !other.is_projection() {
            return Some((Projection::create(self.term.clone(), self.index).into(), other.clone().into()));
        }
        
        let other_proj = other.to_projection().unwrap();
        
        if self.index != other_proj.index() {
            return Some((Projection::create(self.term.clone(), self.index).into(), other.clone().into()));
        }
        
        self.term.first_conflict_with(other_proj.term())
    }
    
    fn match_with(&self, instance: &MinlogTerm) -> MatchOutput<TermSubstEntry> {
        if !instance.is_projection() {
            return MatchOutput::FailedMatch;
        }
        
        let proj_instance = instance.to_projection().unwrap();
        
        if self.index != proj_instance.index() {
            return MatchOutput::FailedMatch;
        }
        
        MatchOutput::Matched(
            IndexMap::from([(self.term.clone().into(), proj_instance.term().clone().into())])
        )
    }
}

impl PrettyPrintable for KernelProjection {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        PPElement::group(vec![
            self.term.to_enclosed_pp_element(detail),
            PPElement::break_elem(0, 0, false),
            PPElement::text("_".to_string()),
            PPElement::break_elem(0, 0, false),
            PPElement::text(self.index.to_string()),
        ], BreakType::Flexible, 0)
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

pub trait NativeProjection: NativeTermBody {
    fn term(&self) -> &MinlogTerm;
    
    fn index(&self) -> usize;
    
    fn to_kernel(&self) -> KernelProjection {
        KernelProjection {
            term: self.term().clone(),
            index: self.index(),
            minlog_type: self.minlog_type(),
        }
    }
}

wrapper_enum::wrapper_enum! {
    #[derive(Clone)]
    pub enum Projection {
        Kernel(kernel: Rc<KernelProjection>),
        Native(native: Rc<dyn NativeProjection>),
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
    
    fwd trait ProjectionForwards {
        pub fwd fn term(&self) -> &MinlogTerm
        
        pub fwd fn index(&self) -> usize
    }
    
    ext trait PrettyPrintable {
        fwd fn to_pp_element(&self, detail: bool) -> PPElement

        fwd fn requires_parens(&self, detail: bool) -> bool

        fwd fn open_paren(&self) -> String

        fwd fn close_paren(&self) -> String
    }
}

impl Projection {
    pub fn create(term: MinlogTerm, index: usize) -> MinlogTerm {
        KernelProjection::create(term, index)
    }
    
    pub fn into_kernel_projection(self) -> Rc<KernelProjection> {
        match self {
            Projection::Kernel(k) => k,
            Projection::Native(n) => Rc::new(n.to_kernel()),
        }
    }
}

impl Hash for Projection {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Projection::Kernel(k) => k.hash(state),
            Projection::Native(n) => n.to_kernel().hash(state),
        }
    }
}

impl PartialEq for Projection {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Projection::Kernel(k1), Projection::Kernel(k2)) => k1 == k2,
            (Projection::Native(n1), Projection::Kernel(k2)) => n1.to_kernel() == *k2.as_ref(),
            (Projection::Kernel(k1), Projection::Native(n2)) => *k1.as_ref() == n2.to_kernel(),
            (Projection::Native(n1), Projection::Native(n2)) => n1.eq(n2.as_ref()),
        }
    }
}

impl Eq for Projection {}

impl From<Rc<KernelProjection>> for Projection {
    fn from(k: Rc<KernelProjection>) -> Self {
        Projection::Kernel(k)
    }
}

impl From<&Rc<KernelProjection>> for Projection {
    fn from(k: &Rc<KernelProjection>) -> Self {
        Projection::Kernel(k.clone())
    }
}

impl From<Rc<dyn NativeProjection>> for Projection {
    fn from(n: Rc<dyn NativeProjection>) -> Self {
        Projection::Native(n)
    }
}

impl From<&Rc<dyn NativeProjection>> for Projection {
    fn from(n: &Rc<dyn NativeProjection>) -> Self {
        Projection::Native(n.clone())
    }
}