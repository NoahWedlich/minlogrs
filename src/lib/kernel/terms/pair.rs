
use crate::includes::{
    essential::*,
    utils::*,
    kernel::{
        types::*,
        terms::*,
    }
};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct KernelPair {
    left: MinlogTerm,
    right: MinlogTerm,
}

impl KernelPair {
    pub fn create(left: MinlogTerm, right: MinlogTerm) -> MinlogTerm {
        MinlogTerm::Pair(Arc::new(KernelPair { left, right }).into())
    }
    
    pub fn left(&self) -> &MinlogTerm {
        &self.left
    }
    
    pub fn right(&self) -> &MinlogTerm {
        &self.right
    }
}

impl TermBody for KernelPair {
    fn minlog_type(&self) -> Arc<MinlogType> {
        PairType::create(self.left.minlog_type(), self.right.minlog_type())
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> MinlogTerm {
        if eta {
            if self.left.is_projection() && self.right.is_projection() {
                println!("Warning: Eta-reduction for pairs of projections not implemented yet.");
            }
            
            if self.left.is_match_term() && self.right.is_match_term() {
                println!("Warning: Eta-reduction for pairs of match terms not implemented yet.");
            }
        }
        
        let new_left = self.left.normalize(eta, pi);
        let new_right = self.right.normalize(eta, pi);
        Pair::create(new_left, new_right)
    }
    
    fn remove_nulls(&self) -> Option<MinlogTerm> {
        let new_left = self.left.remove_nulls();
        let new_right = self.right.remove_nulls();
        
        match (new_left, new_right) {
            (Some(l), Some(r)) => Some(Pair::create(l, r)),
            (Some(l), None) => Some(l),
            (None, Some(r)) => Some(r),
            (None, None) => None,
        }
    }
    
    fn length(&self) -> usize {
        1 + self.left.length() + self.right.length()
    }
    
    fn depth(&self) -> usize {
        1 + self.left.depth().max(self.right.depth())
    }
    
    fn get_type_variables(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Arc<MinlogType>> {
        self.minlog_type().get_type_variables(&mut IndexSet::new())
    }
    
    fn get_algebra_types(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Arc<MinlogType>> {
        self.minlog_type().get_algebra_types(&mut IndexSet::new())
    }
    
    fn get_free_variables(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        self.left.get_free_variables(visited).into_iter()
            .chain(self.right.get_free_variables(visited))
            .collect()
    }

    fn get_bound_variables(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        self.left.get_bound_variables(visited).into_iter()
            .chain(self.right.get_bound_variables(visited))
            .collect()
    }

    fn get_constructors(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        self.left.get_constructors(visited).into_iter()
            .chain(self.right.get_constructors(visited))
            .collect()
    }

    fn get_program_terms(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        self.left.get_program_terms(visited).into_iter()
            .chain(self.right.get_program_terms(visited))
            .collect()
    }
    
    fn alpha_equivalent(&self, other: &MinlogTerm,
        forward: &mut Vec<(TermVariable, TermVariable)>,
        backward: &mut Vec<(TermVariable, TermVariable)>) -> bool
    {
        
        if !other.is_pair() {
            return false;
        }
        
        let other = other.to_pair().unwrap();
        
        if !self.left.alpha_equivalent(other.left(), forward, backward) {
            return false;
        }
        
        if !self.right.alpha_equivalent(other.right(), forward, backward) {
            return false;
        }
        
        true
    }
    
    fn substitute(&self, from: &TermSubstEntry, to: &TermSubstEntry) -> MinlogTerm {
        if let Some(tm) = from.to_term() && tm.is_pair() && Pair::Kernel(Arc::new(self.clone())) == *tm.to_pair().unwrap() {
            to.to_term().unwrap()
        } else {
            let new_left = self.left.substitute(from, to);
            let new_right = self.right.substitute(from, to);
            Pair::create(new_left, new_right)
        }
    }
    
    fn first_conflict_with(&self, other: &MinlogTerm) -> Option<(TermSubstEntry, TermSubstEntry)> {
        if let Some(conflict) = self.minlog_type().first_conflict_with(&other.minlog_type()) {
            return Some((conflict.0.into(), conflict.1.into()));
        }
        
        if !other.is_pair() {
            return Some((Pair::create(self.left.clone(), self.right.clone()).into(), other.clone().into()));
        }
        
        let other_tup = other.to_pair().unwrap();

        if let Some(conflict) = self.left.first_conflict_with(other_tup.left()) {
            return Some(conflict);
        }
        
        if let Some(conflict) = self.right.first_conflict_with(other_tup.right()) {
            return Some(conflict);
        }
        
        None
    }

    fn match_with(&self, instance: &MinlogTerm) -> MatchOutput<TermSubstEntry> {
        if !instance.is_pair() {
            return MatchOutput::FailedMatch;
        }
        
        let tup_instance = instance.to_pair().unwrap();
        
        let conditions = IndexMap::from([
            (self.left.clone().into(), tup_instance.left().clone().into()),
            (self.right.clone().into(), tup_instance.right().clone().into()),
        ]);
        
        MatchOutput::Matched(conditions)
    }
}

impl PrettyPrintable for KernelPair {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        let elements = PPElement::list(
            vec![self.left.to_pp_element(detail), self.right.to_pp_element(detail)],
            PPElement::break_elem(0, 4, false),
            PPElement::text(",".to_string()),
            PPElement::break_elem(1, 4, false),
            BreakType::Flexible
        );
        
        PPElement::group(vec![
            PPElement::text("(".to_string()),
            PPElement::break_elem(1, 4, false),
            elements,
            PPElement::break_elem(1, 0, false),
            PPElement::text(")".to_string())
        ], BreakType::Consistent, 0)
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}

pub trait NativePair: NativeTermBody {
    fn left(&self) -> &MinlogTerm;
    fn right(&self) -> &MinlogTerm;
    
    fn to_kernel(&self) -> KernelPair {
        KernelPair {
            left: self.left().clone(),
            right: self.right().clone(),
        }
    }
}

wrapper_enum::wrapper_enum! {
    #[derive(Clone)]
    pub enum Pair {
        Kernel(kernel: Arc<KernelPair>),
        Native(native: Arc<dyn NativePair>),
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
    
    fwd trait PairForwards {
        pub fwd fn left(&self) -> &MinlogTerm
        
        pub fwd fn right(&self) -> &MinlogTerm
    }
    
    ext trait PrettyPrintable {
        fwd fn to_pp_element(&self, detail: bool) -> PPElement

        fwd fn requires_parens(&self, detail: bool) -> bool

        fwd fn open_paren(&self) -> String

        fwd fn close_paren(&self) -> String
    }
}

impl Pair {
    pub fn create(left: MinlogTerm, right: MinlogTerm) -> MinlogTerm {
        KernelPair::create(left, right)
    }
    
    pub fn into_kernel_pair(self) -> Arc<KernelPair> {
        match self {
            Pair::Kernel(k) => k,
            Pair::Native(n) => Arc::new(n.to_kernel()),
        }
    }
}

impl Hash for Pair {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Pair::Kernel(k) => k.hash(state),
            Pair::Native(n) => n.to_kernel().hash(state),
        }
    }
}

impl PartialEq for Pair {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Pair::Kernel(k1), Pair::Kernel(k2)) => k1 == k2,
            (Pair::Native(n1), Pair::Kernel(k2)) => n1.to_kernel() == *k2.as_ref(),
            (Pair::Kernel(k1), Pair::Native(n2)) => *k1.as_ref() == n2.to_kernel(),
            (Pair::Native(n1), Pair::Native(n2)) => n1.eq(n2.as_ref()),
        }
    }
}

impl Eq for Pair {}

impl From<Arc<KernelPair>> for Pair {
    fn from(k: Arc<KernelPair>) -> Self {
        Pair::Kernel(k)
    }
}

impl From<&Arc<KernelPair>> for Pair {
    fn from(k: &Arc<KernelPair>) -> Self {
        Pair::Kernel(k.clone())
    }
}

impl From<Arc<dyn NativePair>> for Pair {
    fn from(n: Arc<dyn NativePair>) -> Self {
        Pair::Native(n)
    }
}

impl From<&Arc<dyn NativePair>> for Pair {
    fn from(n: &Arc<dyn NativePair>) -> Self {
        Pair::Native(n.clone())
    }
}