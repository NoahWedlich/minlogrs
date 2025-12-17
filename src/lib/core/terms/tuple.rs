
use crate::includes::{
    essential::*,
    utils::*,
    core::{
        types::*,
        terms::*,
    }
};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct KernelTuple {
    elements: Vec<MinlogTerm>,
    minlog_type: Rc<MinlogType>,
}

impl KernelTuple {
    pub fn create(elements: Vec<MinlogTerm>) -> MinlogTerm {
        let element_types: Vec<Rc<MinlogType>> = elements.iter().map(|e| e.minlog_type()).collect();
        let minlog_type = TupleType::create(element_types);
        MinlogTerm::Tuple(Rc::new(KernelTuple { elements, minlog_type }).into())
    }
    
    pub fn elements(&self) -> &Vec<MinlogTerm> {
        &self.elements
    }
    
    pub fn element(&self, index: usize) -> Option<&MinlogTerm> {
        self.elements.get(index)
    }
}

impl TermBody for KernelTuple {
    fn minlog_type(&self) -> Rc<MinlogType> {
        self.minlog_type.clone()
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> MinlogTerm {
        if eta {
            if self.elements.iter().all(|e| e.is_projection()) {
                println!("Warning: Eta-reduction for tuples of projections not implemented yet.");
            }
            
            if self.elements.iter().all(|e| e.is_match_term()) {
                println!("Warning: Eta-reduction for tuples of match terms not implemented yet.");
            }
        }
        
        let new_elements = self.elements.iter().map(|e| e.normalize(eta, pi)).collect();
        Tuple::create(new_elements)
    }
    
    fn remove_nulls(&self) -> Option<MinlogTerm> {
        let new_elements = self.elements.iter()
            .filter_map(|e| e.remove_nulls())
            .collect::<Vec<_>>();
        
        if new_elements.is_empty() {
            None
        } else {
            Some(Tuple::create(new_elements))
        }
    }
    
    fn length(&self) -> usize {
        1 + self.elements.iter().map(|e| e.length()).sum::<usize>()
    }
    
    fn depth(&self) -> usize {
        1 + self.elements.iter().map(|e| e.depth()).max().unwrap_or(0)
    }
    
    fn get_type_variables(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogType>> {
        self.minlog_type.get_type_variables(&mut IndexSet::new())
    }
    
    fn get_algebra_types(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogType>> {
        self.minlog_type.get_algebra_types(&mut IndexSet::new())
    }
    
    fn get_free_variables(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        self.elements.iter().flat_map(|e| e.get_free_variables(visited)).collect()
    }

    fn get_bound_variables(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        self.elements.iter().flat_map(|e| e.get_bound_variables(visited)).collect()
    }

    fn get_constructors(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        self.elements.iter().flat_map(|e| e.get_constructors(visited)).collect()
    }

    fn get_program_terms(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        self.elements.iter().flat_map(|e| e.get_program_terms(visited)).collect()
    }
    
    fn alpha_equivalent(&self, other: &MinlogTerm,
        forward: &mut Vec<(TermVariable, TermVariable)>,
        backward: &mut Vec<(TermVariable, TermVariable)>) -> bool
    {
        
        if !other.is_tuple() {
            return false;
        }
        
        let other = other.to_tuple().unwrap();
        
        if self.elements.len() != other.elements().len() {
            return false;
        }
        
        for (e1, e2) in self.elements.iter().zip(other.elements().iter()) {
            if !e1.alpha_equivalent(e2, forward, backward) {
                return false;
            }
        }
        
        true
    }
    
    fn substitute(&self, from: &TermSubstEntry, to: &TermSubstEntry) -> MinlogTerm {
        if let Some(tm) = from.to_term() && tm.is_tuple() && Tuple::Kernel(Rc::new(self.clone())) == *tm.to_tuple().unwrap() {
            to.to_term().unwrap()
        } else {
            let new_elements = self.elements.iter().map(|e| e.substitute(from, to)).collect();
            Tuple::create(new_elements)
        }
    }
    
    fn first_conflict_with(&self, other: &MinlogTerm) -> Option<(TermSubstEntry, TermSubstEntry)> {
        if let Some(conflict) = self.minlog_type.first_conflict_with(&other.minlog_type()) {
            return Some((conflict.0.into(), conflict.1.into()));
        }
        
        if !other.is_tuple() {
            return Some((Tuple::create(self.elements.clone()).into(), other.clone().into()));
        }
        
        let other_tup = other.to_tuple().unwrap();

        if self.elements.len() != other_tup.elements().len() {
            return Some((Tuple::create(self.elements.clone()).into(), other.clone().into()));
        }

        for (e1, e2) in self.elements.iter().zip(other_tup.elements().iter()) {
            if let Some(conflict) = e1.first_conflict_with(e2) {
                return Some(conflict);
            }
        }
        
        None
    }

    fn match_with(&self, instance: &MinlogTerm) -> MatchOutput<TermSubstEntry> {
        if !instance.is_tuple() {
            return MatchOutput::FailedMatch;
        }
        
        let tup_instance = instance.to_tuple().unwrap();
        
        if self.elements.len() != tup_instance.elements().len() {
            return MatchOutput::FailedMatch;
        }
        
        let conditions = self.elements.iter().zip(tup_instance.elements().iter())
            .filter_map(|(e1, e2)| {
                if e1 != e2 {
                    Some((e1.into(), e2.into()))
                } else {
                    None
                }
            })
            .collect::<IndexMap<_, _>>();
        
        MatchOutput::Matched(conditions)
    }
}

impl PrettyPrintable for KernelTuple {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        let elements = PPElement::list(
            self.elements.iter().map(|e| e.minlog_type().to_pp_element(detail)).collect(),
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

pub trait NativeTuple: NativeTermBody {
    fn elements(&self) -> &Vec<MinlogTerm>;
    
    fn element(&self, index: usize) -> Option<&MinlogTerm>;
    
    fn to_kernel(&self) -> KernelTuple {
        KernelTuple {
            elements: self.elements().clone(),
            minlog_type: self.minlog_type(),
        }
    }
}

wrapper_enum::wrapper_enum! {
    #[derive(Clone)]
    pub enum Tuple {
        Kernel(kernel: Rc<KernelTuple>),
        Native(native: Rc<dyn NativeTuple>),
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
        pub fwd fn elements(&self) -> &Vec<MinlogTerm>
        
        pub fwd fn element(&self, index: usize) -> Option<&MinlogTerm>
    }
    
    ext trait PrettyPrintable {
        fwd fn to_pp_element(&self, detail: bool) -> PPElement

        fwd fn requires_parens(&self, detail: bool) -> bool

        fwd fn open_paren(&self) -> String

        fwd fn close_paren(&self) -> String
    }
}

impl Tuple {
    pub fn create(elements: Vec<MinlogTerm>) -> MinlogTerm {
        KernelTuple::create(elements)
    }
    
    pub fn into_kernel_tuple(self) -> Rc<KernelTuple> {
        match self {
            Tuple::Kernel(k) => k,
            Tuple::Native(n) => Rc::new(n.to_kernel()),
        }
    }
}

impl Hash for Tuple {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Tuple::Kernel(k) => k.hash(state),
            Tuple::Native(n) => n.to_kernel().hash(state),
        }
    }
}

impl PartialEq for Tuple {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Tuple::Kernel(k1), Tuple::Kernel(k2)) => k1 == k2,
            (Tuple::Native(n1), Tuple::Kernel(k2)) => n1.to_kernel() == *k2.as_ref(),
            (Tuple::Kernel(k1), Tuple::Native(n2)) => *k1.as_ref() == n2.to_kernel(),
            (Tuple::Native(n1), Tuple::Native(n2)) => n1.eq(n2.as_ref()),
        }
    }
}

impl Eq for Tuple {}

impl From<Rc<KernelTuple>> for Tuple {
    fn from(k: Rc<KernelTuple>) -> Self {
        Tuple::Kernel(k)
    }
}

impl From<&Rc<KernelTuple>> for Tuple {
    fn from(k: &Rc<KernelTuple>) -> Self {
        Tuple::Kernel(k.clone())
    }
}

impl From<Rc<dyn NativeTuple>> for Tuple {
    fn from(n: Rc<dyn NativeTuple>) -> Self {
        Tuple::Native(n)
    }
}

impl From<&Rc<dyn NativeTuple>> for Tuple {
    fn from(n: &Rc<dyn NativeTuple>) -> Self {
        Tuple::Native(n.clone())
    }
}