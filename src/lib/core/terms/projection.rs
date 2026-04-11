
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
    left: bool,
}

impl KernelProjection {
    pub fn create(term: MinlogTerm, left: bool) -> MinlogTerm {
        if !term.minlog_type().is_pair() {
            panic!("Tried to create projection from non-pair term.");
        }
        
        MinlogTerm::Projection(Arc::new(KernelProjection { term, left }).into())
    }
    
    pub fn term(&self) -> &MinlogTerm {
        &self.term
    }
    
    pub fn left(&self) -> bool {
        self.left
    }
    
    pub fn right(&self) -> bool {
        !self.left
    }
}

impl TermBody for KernelProjection {
    fn minlog_type(&self) -> Arc<MinlogType> {
        let term_type = self.term.minlog_type();
        let pair_type = term_type.to_pair().unwrap();
        
        if self.left {
            pair_type.left().clone()
        } else {
            pair_type.right().clone()
        }
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> MinlogTerm {
        if let Some(pair) = self.term.to_pair() {
            if self.left {
                return pair.left().normalize(eta, pi);
            } else {
                return pair.right().normalize(eta, pi);
            }
        }
        
        let new_term = self.term.normalize(eta, pi);
        Projection::create(new_term, self.left)
    }
    
    fn remove_nulls(&self) -> Option<MinlogTerm> {
        if let Some(pair) = self.term.to_pair() {
            if let Some(new_term) = self.term.remove_nulls() && new_term.is_pair() {
                Some(Projection::create(new_term, self.left))
            } else {
                let element = if self.left {
                    pair.left()
                } else {
                    pair.right()
                };
                
                element.remove_nulls()
            }
        } else {
            self.term.remove_nulls().map(|new_term| {
                Projection::create(new_term, self.left)
            })
        }
    }
    
    fn length(&self) -> usize {
        1 + self.term.length()
    }
    
    fn depth(&self) -> usize {
        1 + self.term.depth()
    }
    
    fn get_type_variables(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Arc<MinlogType>> {
        self.minlog_type().get_type_variables(&mut IndexSet::new())
    }
    
    fn get_algebra_types(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Arc<MinlogType>> {
        self.minlog_type().get_algebra_types(&mut IndexSet::new())
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
        
        self.left == other.left() && self.term.alpha_equivalent(other.term(), forward, backward)
    }
    
    fn substitute(&self, from: &TermSubstEntry, to: &TermSubstEntry) -> MinlogTerm {
        if let Some(tm) = from.to_term() && tm.is_projection() && Projection::Kernel(Arc::new(self.clone())) == *tm.to_projection().unwrap() {
            to.to_term().unwrap()
        } else {
            let new_term = self.term.substitute(from, to);
            Projection::create(new_term, self.left)
        }
    }
    
    fn first_conflict_with(&self, other: &MinlogTerm) -> Option<(TermSubstEntry, TermSubstEntry)> {
        if let Some(conflict) = self.minlog_type().first_conflict_with(&other.minlog_type()) {
            return Some((conflict.0.into(), conflict.1.into()));
        }
        
        if !other.is_projection() {
            return Some((Projection::create(self.term.clone(), self.left).into(), other.clone().into()));
        }
        
        let other_proj = other.to_projection().unwrap();
        
        if self.left != other_proj.left() {
            return Some((Projection::create(self.term.clone(), self.left).into(), other.clone().into()));
        }
        
        self.term.first_conflict_with(other_proj.term())
    }
    
    fn match_with(&self, instance: &MinlogTerm) -> MatchOutput<TermSubstEntry> {
        if !instance.is_projection() {
            return MatchOutput::FailedMatch;
        }
        
        let proj_instance = instance.to_projection().unwrap();
        
        if self.left != proj_instance.left() {
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
            PPElement::text(self.left.to_string()),
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
    
    fn left(&self) -> bool;
    
    fn right(&self) -> bool {
        !self.left()
    }
    
    fn to_kernel(&self) -> KernelProjection {
        KernelProjection {
            term: self.term().clone(),
            left: self.left(),
        }
    }
}

wrapper_enum::wrapper_enum! {
    #[derive(Clone)]
    pub enum Projection {
        Kernel(kernel: Arc<KernelProjection>),
        Native(native: Arc<dyn NativeProjection>),
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
    
    fwd trait ProjectionForwards {
        pub fwd fn term(&self) -> &MinlogTerm
        
        pub fwd fn left(&self) -> bool
    }
    
    ext trait PrettyPrintable {
        fwd fn to_pp_element(&self, detail: bool) -> PPElement

        fwd fn requires_parens(&self, detail: bool) -> bool

        fwd fn open_paren(&self) -> String

        fwd fn close_paren(&self) -> String
    }
}

impl Projection {
    pub fn create(term: MinlogTerm, left: bool) -> MinlogTerm {
        KernelProjection::create(term, left)
    }
    
    pub fn into_kernel_projection(self) -> Arc<KernelProjection> {
        match self {
            Projection::Kernel(k) => k,
            Projection::Native(n) => Arc::new(n.to_kernel()),
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

impl From<Arc<KernelProjection>> for Projection {
    fn from(k: Arc<KernelProjection>) -> Self {
        Projection::Kernel(k)
    }
}

impl From<&Arc<KernelProjection>> for Projection {
    fn from(k: &Arc<KernelProjection>) -> Self {
        Projection::Kernel(k.clone())
    }
}

impl From<Arc<dyn NativeProjection>> for Projection {
    fn from(n: Arc<dyn NativeProjection>) -> Self {
        Projection::Native(n)
    }
}

impl From<&Arc<dyn NativeProjection>> for Projection {
    fn from(n: &Arc<dyn NativeProjection>) -> Self {
        Projection::Native(n.clone())
    }
}