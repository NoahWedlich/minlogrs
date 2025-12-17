
use crate::includes::{
    essential::*,
    utils::*,
    core::{
        types::*,
        terms::*,
    }
};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct KernelMatchTerm {
    minlog_type: Rc<MinlogType>,
    branches: Vec<(MinlogTerm, MinlogTerm)>,
}

// TODO: Allow anonymous recursion
impl KernelMatchTerm {
    pub fn create(minlog_type: Rc<MinlogType>, branches: Vec<(MinlogTerm, MinlogTerm)>) -> MinlogTerm {
        if let Some(arr_type) = minlog_type.to_arrow() {
            let argument_type = arr_type.arguments()[0].clone();
            let return_type = ArrowType::create(
                arr_type.arguments()[1..].to_vec(),
                arr_type.value().clone(),
            );
            
            for (pattern, result) in branches.iter() {
                if pattern.minlog_type() != argument_type {
                    panic!("Match term branch pattern type does not match match term argument type");
                }
                
                if result.minlog_type() != return_type {
                    panic!("Match term branch result type does not match match term return type");
                }
            }
            
            MinlogTerm::MatchTerm(Rc::new(KernelMatchTerm {
                minlog_type,
                branches,
            }).into())
        } else {
            panic!("Match term must have an arrow type");
        }
    }
    
    pub fn branches(&self) -> &Vec<(MinlogTerm, MinlogTerm)> {
        &self.branches
    }
}

impl TermBody for KernelMatchTerm {
    fn minlog_type(&self) -> Rc<MinlogType> {
        self.minlog_type.clone()
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> MinlogTerm {
        let normalized_branches = self.branches.iter().map(|(pattern, result)| {
            (
                pattern.normalize(eta, pi),
                result.normalize(eta, pi)
            )
        }).collect::<Vec<_>>();
        
        MatchTerm::create(self.minlog_type.clone(), normalized_branches)
    }
    
    fn apply_args(&self, args: &Vec<MinlogTerm >) -> Option<MinlogTerm> {
        if let Some(to_match) = args.first() {
            for (pattern, instance) in self.branches.iter() {
                if let Some(subst) = TermSubstitution::match_with(&pattern.clone().into(), &to_match.clone().into()) {
                    let substituted_instance = subst.substitute::<TermSubstEntry>(&instance.clone().into()).to_term().unwrap();
                    
                    let remaining_args = args[1..].to_vec();
                    
                    if remaining_args.is_empty() {
                        return Some(substituted_instance.clone());
                    } else {
                        return Some(Application::create(substituted_instance.clone(), remaining_args.clone()));
                    }
                }
            }
            
            None
        } else {
            None
        }
    }
    
    fn remove_nulls(&self) -> Option<MinlogTerm> {
        let new_branches = self.branches.iter().filter_map(|(pattern, result)| {
            let new_pattern = pattern.remove_nulls();
            let new_result = result.remove_nulls();
            
            if new_pattern.is_none() || new_result.is_none() {
                return None;
            }
            
            Some((new_pattern.unwrap(), new_result.unwrap()))
        }).collect::<Vec<_>>();
        
        if new_branches.is_empty() {
            None
        } else {
            Some(MatchTerm::create(self.minlog_type.clone(), new_branches))
        }
    }
    
    fn length(&self) -> usize {
        1
    }
    
    fn depth(&self) -> usize {
        0
    }
    
    fn get_type_variables(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogType>> {
        if visited.contains(&MinlogTerm::MatchTerm(Rc::new(self.clone()).into())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogTerm::MatchTerm(Rc::new(self.clone()).into()));
            
            self.branches.iter()
                .flat_map(|(p, i)|
                    p.get_type_variables(visited)
                        .union(&i.get_type_variables(visited))
                        .cloned().collect::<IndexSet<_>>())
                .collect()
        }
    }
    
    fn get_algebra_types(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogType>> {
        if visited.contains(&MinlogTerm::MatchTerm(Rc::new(self.clone()).into())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogTerm::MatchTerm(Rc::new(self.clone()).into()));
            
            self.branches.iter()
                .flat_map(|(p, i)| {
                    p.get_algebra_types(visited)
                        .union(&i.get_algebra_types(visited))
                        .cloned().collect::<IndexSet<_>>()
                })
                .collect()
        }
    }
    
    fn get_free_variables(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        if visited.contains(&MinlogTerm::MatchTerm(Rc::new(self.clone()).into())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogTerm::MatchTerm(Rc::new(self.clone()).into()));
            
            self.branches.iter()
                .flat_map(|(p, i)| {
                    p.get_free_variables(visited)
                        .union(&i.get_free_variables(visited))
                        .cloned().collect::<IndexSet<_>>()
                })
                .collect()
        }
    }
    
    fn get_bound_variables(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        if visited.contains(&MinlogTerm::MatchTerm(Rc::new(self.clone()).into())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogTerm::MatchTerm(Rc::new(self.clone()).into()));
            
            self.branches.iter()
                .flat_map(|(p, i)| {
                    p.get_bound_variables(visited)
                        .union(&i.get_bound_variables(visited))
                        .cloned().collect::<IndexSet<_>>()
                })
                .collect()
        }
    }
    
    fn get_constructors(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        if visited.contains(&MinlogTerm::MatchTerm(Rc::new(self.clone()).into())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogTerm::MatchTerm(Rc::new(self.clone()).into()));
            
            self.branches.iter()
                .flat_map(|(p, i)| {
                    p.get_constructors(visited)
                        .union(&i.get_constructors(visited))
                        .cloned().collect::<IndexSet<_>>()
                })
                .collect()
        }
    }
    
    fn get_program_terms(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        if visited.contains(&MinlogTerm::MatchTerm(Rc::new(self.clone()).into())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogTerm::MatchTerm(Rc::new(self.clone()).into()));
            
            self.branches.iter()
                .flat_map(|(p, i)| {
                    p.get_program_terms(visited)
                        .union(&i.get_program_terms(visited))
                        .cloned().collect::<IndexSet<_>>()
                })
                .collect()
        }
    }
    
    fn alpha_equivalent(&self, other: &MinlogTerm,
        forward: &mut Vec<(TermVariable, TermVariable)>,
        backward: &mut Vec<(TermVariable, TermVariable)>) -> bool
    {
        if let Some(other_match) = other.to_match_term() {
            let mut remaining_other_branches = other_match.branches().clone();
            for (pattern, instance) in self.branches.iter() {
                let mut found_equivalent = false;
                for (i, (other_pattern, other_instance)) in remaining_other_branches.iter().enumerate() {
                    if pattern.alpha_equivalent(other_pattern, forward, backward)
                        && instance.alpha_equivalent(other_instance, forward, backward)
                    {
                        found_equivalent = true;
                        remaining_other_branches.remove(i);
                        break;
                    }
                }
                
                if !found_equivalent {
                    return false;
                }
            }
            
            remaining_other_branches.is_empty()
        } else {
            false
        }
    }
    
    fn substitute(&self, from: &TermSubstEntry, to: &TermSubstEntry) -> MinlogTerm {
        if let Some(tm) = from.to_term() && let Some(match_term) = tm.to_match_term()
            && MatchTerm::Kernel(Rc::new(self.clone())) == *match_term
        {
            to.to_term().unwrap()
        } else {
            let substituted_branches = self.branches.iter().map(|(pattern, instance)| {
                (
                    pattern.substitute(from, to),
                    instance.substitute(from, to)
                )
            }).collect::<Vec<_>>();
            
            MatchTerm::create(self.minlog_type.clone(), substituted_branches)
        }
    }
    
    fn first_conflict_with(&self, other: &MinlogTerm) -> Option<(TermSubstEntry, TermSubstEntry)> {
        if let Some(other_match) = other.to_match_term() {
            if self.branches.len() != other_match.branches().len() {
                return Some((
                    TermSubstEntry::Term(MinlogTerm::MatchTerm(Rc::new(self.clone()).into())),
                    TermSubstEntry::Term(other.clone())
                ));
            }
            
            for ((pattern, instance), (other_pattern, other_instance))
                in self.branches.iter().zip(other_match.branches().iter())
            {
                if let Some(conflict) = pattern.first_conflict_with(other_pattern) {
                    return Some(conflict);
                } else if let Some(conflict) = instance.first_conflict_with(other_instance) {
                    return Some(conflict);
                }
            }
            
            None
        } else {
            Some((
                TermSubstEntry::Term(MinlogTerm::MatchTerm(Rc::new(self.clone()).into())),
                TermSubstEntry::Term(other.clone())
            ))
        }
    }
    
    fn match_with(&self, instance: &MinlogTerm) -> MatchOutput<TermSubstEntry> {
        if !instance.is_match_term() {
            return MatchOutput::FailedMatch;
        }
        
        let match_instance = instance.to_match_term().unwrap();
        
        if self.branches.len() != match_instance.branches().len() {
            return MatchOutput::FailedMatch;
        }
        
        let conditions = self.branches.iter().zip(match_instance.branches().iter())
            .flat_map(|((p_pattern, i_pattern), (p_instance, i_instance))| {
                let mut cond = vec![];
                
                if p_pattern != p_instance {
                    cond.push((p_pattern.into(), p_instance.into()));
                }
                
                if i_pattern != i_instance {
                    cond.push((i_pattern.into(), i_instance.into()));
                }
                
                cond
            }
        ).collect::<IndexMap<_, _>>();
        
        MatchOutput::Matched(conditions)
    }
}

impl PrettyPrintable for KernelMatchTerm {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        let name = if detail {
            PPElement::group(vec![
                PPElement::text("match:".to_string()),
                PPElement::break_elem(1, 4, false),
                self.minlog_type.to_pp_element(detail),
            ], BreakType::Flexible, 0)
        } else {
            PPElement::text("match".to_string())
        };
        
        let branches = PPElement::list(
            self.branches.iter().map(|(pattern, instance)| {
                PPElement::group(vec![
                    pattern.to_pp_element(detail),
                    PPElement::break_elem(1, 4, false),
                    PPElement::text("->".to_string()),
                    PPElement::break_elem(1, 4, false),
                    instance.to_pp_element(detail),
                ], BreakType::Flexible, 0)
            }).collect(),
            PPElement::break_elem(0, 0, false),
            PPElement::text(",".to_string()),
            PPElement::break_elem(1, 0, false),
            BreakType::Flexible,
        );
        
        PPElement::group(vec![
            name,
            PPElement::text(" {".to_string()),
            PPElement::break_elem(1, 4, false),
            branches,
            PPElement::break_elem(1, 0, false),
            PPElement::text("}".to_string()),
        ], BreakType::Consistent, 0)
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}

pub trait NativeMatchTerm: NativeTermBody {
    fn branches(&self) -> &Vec<(MinlogTerm, MinlogTerm)>;
    
    fn to_kernel(&self) -> KernelMatchTerm {
        KernelMatchTerm {
            minlog_type: self.minlog_type(),
            branches: self.branches().clone(),
        }
    }
}

wrapper_enum::wrapper_enum! {
    #[derive(Clone)]
    pub enum MatchTerm {
        Kernel(kernel: Rc<KernelMatchTerm>),
        Native(native: Rc<dyn NativeMatchTerm>),
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
    
    fwd trait MatchTermForwards {
        pub fwd fn branches(&self) -> &Vec<(MinlogTerm, MinlogTerm)>
    }
    
    ext trait PrettyPrintable {
        fwd fn to_pp_element(&self, detail: bool) -> PPElement

        fwd fn requires_parens(&self, detail: bool) -> bool

        fwd fn open_paren(&self) -> String

        fwd fn close_paren(&self) -> String
    }
}

impl MatchTerm {
    pub fn create(minlog_type: Rc<MinlogType>, branches: Vec<(MinlogTerm, MinlogTerm)>) -> MinlogTerm {
        KernelMatchTerm::create(minlog_type, branches)
    }
    
    pub fn into_kernel_match_term(self) -> Rc<KernelMatchTerm> {
        match self {
            MatchTerm::Kernel(k) => k,
            MatchTerm::Native(n) => Rc::new(n.to_kernel()),
        }
    }
}

impl Hash for MatchTerm {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            MatchTerm::Kernel(k) => k.hash(state),
            MatchTerm::Native(n) => n.to_kernel().hash(state),
        }
    }
}

impl PartialEq for MatchTerm {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (MatchTerm::Kernel(k1), MatchTerm::Kernel(k2)) => k1 == k2,
            (MatchTerm::Native(n1), MatchTerm::Kernel(k2)) => n1.to_kernel() == *k2.as_ref(),
            (MatchTerm::Kernel(k1), MatchTerm::Native(n2)) => *k1.as_ref() == n2.to_kernel(),
            (MatchTerm::Native(n1), MatchTerm::Native(n2)) => n1.eq(n2.as_ref()),
        }
    }
}

impl Eq for MatchTerm {}

impl From<Rc<KernelMatchTerm>> for MatchTerm {
    fn from(k: Rc<KernelMatchTerm>) -> Self {
        MatchTerm::Kernel(k)
    }
}

impl From<&Rc<KernelMatchTerm>> for MatchTerm {
    fn from(k: &Rc<KernelMatchTerm>) -> Self {
        MatchTerm::Kernel(k.clone())
    }
}

impl From<Rc<dyn NativeMatchTerm>> for MatchTerm {
    fn from(n: Rc<dyn NativeMatchTerm>) -> Self {
        MatchTerm::Native(n)
    }
}

impl From<&Rc<dyn NativeMatchTerm>> for MatchTerm {
    fn from(n: &Rc<dyn NativeMatchTerm>) -> Self {
        MatchTerm::Native(n.clone())
    }
}