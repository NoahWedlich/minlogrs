
use crate::includes::{
    essential::*,
    utils::*,
    core::{
        structures::*,
        types::*,
        terms::*,
    }
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct KernelProgramTerm {
    pconst: Rc<ProgramConstant>,
    parameters: TermSubstitution,
}

impl KernelProgramTerm {
    pub fn create(pconst: Rc<ProgramConstant>, mut parameters: TermSubstitution) -> MinlogTerm {
        let pconst_vars: Vec<TermSubstEntry> = pconst.get_type_variables(&mut IndexSet::new()).into_iter().map(|tv| tv.into())
            .chain(pconst.get_free_variables(&mut IndexSet::new()).into_iter().map(|fv| fv.into()))
            .collect::<Vec<_>>();
        
        parameters.restrict(|from| pconst_vars.contains(from));
        
        MinlogTerm::ProgramTerm(Rc::new(KernelProgramTerm { pconst, parameters }).into())
    }
    
    pub fn pconst(&self) -> &Rc<ProgramConstant> {
        &self.pconst
    }
    
    pub fn name(&self) -> &str {
        self.pconst.name()
    }
    
    pub fn computation_rules(&self) -> Vec<Rc<RewriteRule>> {
        self.pconst.computation_rules().iter()
            .map(|r| self.parameters.substitute(r))
            .collect()
    }
    
    pub fn rewrite_rules(&self) -> Vec<Rc<RewriteRule>> {
        self.pconst.rewrite_rules().iter()
            .map(|r| self.parameters.substitute(r))
            .collect()
    }
    
    pub fn parameters(&self) -> &TermSubstitution {
        &self.parameters
    }
}

impl TermBody for KernelProgramTerm {
    fn minlog_type(&self) -> Rc<MinlogType> {
        let pc_type = self.pconst.minlog_type();
        self.parameters.substitute::<TermSubstEntry>(&pc_type.into()).to_type().unwrap()
    }
    
    fn normalize(&self, _eta: bool, _pi: bool) -> MinlogTerm {
        ProgramTerm::create(self.pconst.clone(), self.parameters.clone())
    }
    
    fn apply_args(&self, args: &Vec<MinlogTerm >) -> Option<MinlogTerm> {
        for rule in self.computation_rules().iter().chain(self.rewrite_rules().iter()) {
            let applicable_args = min(args.len(), rule.arity());
            let (args_to_apply, remaining_args) = args.split_at(applicable_args);
            let to_match = Application::create(MinlogTerm::ProgramTerm(Rc::new(self.clone()).into()), args_to_apply.to_vec());
            
            if let Some(subst) = TermSubstitution::match_with(&rule.pattern().into(), &to_match.into()) {
                let result = subst.substitute::<TermSubstEntry>(&rule.result().into()).to_term().unwrap();
                
                if remaining_args.is_empty() {
                    return Some(result);
                } else {
                    return Some(Application::create(result, remaining_args.to_vec()));
                }
            }
        }
        
        None
    }
    
    fn remove_nulls(&self) -> Option<MinlogTerm> {
        let new_parameters = TermSubstitution::from_pairs(
            self.parameters.pairs().iter().filter_map(|(from, to)| {
                match to {
                    TermSubstEntry::Type(to_t) => {
                        to_t.remove_nulls().map(|new_type|
                            (from.clone(), TermSubstEntry::Type(new_type))
                        )
                    },
                    TermSubstEntry::Term(to_tm) => {
                        to_tm.remove_nulls().map(|new_term|
                            (from.clone(), TermSubstEntry::Term(new_term))
                        )
                    }
                }
            }
        ).collect());
        
        Some(ProgramTerm::create(self.pconst.clone(), new_parameters))
    }
    
    fn length(&self) -> usize {
        1
    }
    
    fn depth(&self) -> usize {
        0
    }
    
    fn get_type_variables(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogType>> {
        if visited.contains(&MinlogTerm::ProgramTerm(Rc::new(self.clone()).into())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogTerm::ProgramTerm(Rc::new(self.clone()).into()));
            
            self.computation_rules().iter().chain(self.rewrite_rules().iter())
                .flat_map(|r| r.get_type_variables(visited))
                .collect()
        }
    }
    
    fn get_algebra_types(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogType>> {
        if visited.contains(&MinlogTerm::ProgramTerm(Rc::new(self.clone()).into())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogTerm::ProgramTerm(Rc::new(self.clone()).into()));
            
            self.computation_rules().iter().chain(self.rewrite_rules().iter())
                .flat_map(|r| r.get_algebra_types(visited))
                .collect()
        }
    }
    
    fn get_free_variables(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        if visited.contains(&MinlogTerm::ProgramTerm(Rc::new(self.clone()).into())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogTerm::ProgramTerm(Rc::new(self.clone()).into()));
            
            self.computation_rules().iter().chain(self.rewrite_rules().iter())
                .flat_map(|r| r.get_free_variables(visited))
                .collect()
        }
    }
    
    fn get_bound_variables(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        if visited.contains(&MinlogTerm::ProgramTerm(Rc::new(self.clone()).into())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogTerm::ProgramTerm(Rc::new(self.clone()).into()));
            
            self.computation_rules().iter().chain(self.rewrite_rules().iter())
                .flat_map(|r| r.get_bound_variables(visited))
                .collect()
        }
    }
    
    fn get_program_terms(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<MinlogTerm> {
        IndexSet::from([MinlogTerm::ProgramTerm(Rc::new(self.clone()).into())])
    }
    
    fn alpha_equivalent(&self, other: &MinlogTerm,
        _forward: &mut Vec<(TermVariable, TermVariable)>,
        _backward: &mut Vec<(TermVariable, TermVariable)>) -> bool {
            
        if let Some(other_body) = other.to_program_term() {
            self.pconst == *other_body.pconst() && self.parameters == *other_body.parameters()
        } else {
            false
        }
    }
    
    fn substitute(&self, from: &TermSubstEntry, to: &TermSubstEntry) -> MinlogTerm {
        if let Some(tm) = from.to_term() && tm.is_program_term() && ProgramTerm::Kernel(Rc::new(self.clone())) == *tm.to_program_term().unwrap() {
            to.to_term().unwrap()
        } else {
            let mut new_params = self.parameters.clone();
            new_params.extend((from.clone(), to.clone()));
            
            ProgramTerm::create(self.pconst.clone(), new_params)
        }
    }
    
    fn first_conflict_with(&self, other: &MinlogTerm) -> Option<(TermSubstEntry, TermSubstEntry)> {
        if !other.is_program_term() {
            panic!("Tried to find conflict between incompatible TermSubstEntry types");
        }
        
        let other_pterm = other.to_program_term().unwrap();
        
        if self.pconst != *other_pterm.pconst() {
            return Some((MinlogTerm::ProgramTerm(Rc::new(self.clone()).into()).into(), other.clone().into()));
        }
        
        for (from, to) in self.parameters.pairs().iter() {
            let other_to = other_pterm.parameters().substitute::<TermSubstEntry>(from);
            match (to, other_to) {
                (TermSubstEntry::Type(to_t), TermSubstEntry::Type(other_to_t)) => {
                    if let Some(conflict) = to_t.first_conflict_with(&other_to_t) {
                        return Some((conflict.0.into(), conflict.1.into()));
                    }
                },
                (TermSubstEntry::Term(to_tm), TermSubstEntry::Term(other_to_tm)) => {
                    if let Some(conflict) = to_tm.first_conflict_with(&other_to_tm) {
                        return Some(conflict);
                    }
                },
                _ => {
                    panic!("Tried to find conflict between incompatible TermSubstEntry types");
                }
            }
        }
        
        None
    }
    
    fn match_with(&self, instance: &MinlogTerm) -> MatchOutput<TermSubstEntry> {
        if !instance.is_program_term() {
            return MatchOutput::FailedMatch;
        }
        
        let pterm_instance = instance.to_program_term().unwrap();
        
        if self.pconst != *pterm_instance.pconst() {
            return MatchOutput::FailedMatch;
        }
        
        let conditions = TermSubstitution::collect_match_conditions(
            &self.parameters,
            pterm_instance.parameters(),
        );
        
        MatchOutput::Matched(conditions)
    }
}

impl PrettyPrintable for KernelProgramTerm {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        let tvars = self.pconst.get_type_variables(&mut IndexSet::new());
        let tmvars = self.pconst.get_free_variables(&mut IndexSet::new());
        
        let has_tvars = !tvars.is_empty();
        let has_tmvars = !tmvars.is_empty();
        
        if !has_tvars && !has_tmvars {
            PPElement::text(self.pconst.name().clone())
        } else {
            let tvars = PPElement::list(
                tvars.iter().map(|tv| {
                    let substituted = self.parameters.substitute::<TermSubstEntry>(&tv.clone().into()).to_type().unwrap();
                    if detail && &substituted != tv {
                        PPElement::group(vec![
                            tv.to_pp_element(detail),
                            PPElement::break_elem(1, 4, false),
                            PPElement::text("=".to_string()),
                            PPElement::break_elem(1, 4, false),
                            substituted.to_enclosed_pp_element(detail)
                        ], BreakType::Flexible, 0)
                    } else {
                        tv.to_enclosed_pp_element(detail)
                    }
                }).collect(),
                PPElement::break_elem(0, 4, false),
                PPElement::text(",".to_string()),
                PPElement::break_elem(1, 4, false),
                BreakType::Flexible,
            );
            
            let tmvars = PPElement::list(
                tmvars.iter().map(|tv| {
                    let substituted = self.parameters.substitute::<TermSubstEntry>(&tv.clone().into()).to_term().unwrap();
                    if detail && &substituted != tv {
                        PPElement::group(vec![
                            tv.to_pp_element(detail),
                            PPElement::break_elem(1, 4, false),
                            PPElement::text("=".to_string()),
                            PPElement::break_elem(1, 4, false),
                            substituted.to_enclosed_pp_element(detail)
                        ], BreakType::Flexible, 0)
                    } else {
                        tv.to_enclosed_pp_element(detail)
                    }
                }).collect(),
                PPElement::break_elem(0, 4, false),
                PPElement::text(",".to_string()),
                PPElement::break_elem(1, 4, false),
                BreakType::Flexible,
            );
            
            let mut variable_list = vec![];
            if has_tvars { variable_list.push(tvars) };
            if has_tmvars { variable_list.push(tmvars) };
            
            let variables = PPElement::list(
                variable_list,
                PPElement::break_elem(1, 0, false),
                PPElement::text("|".to_string()),
                PPElement::break_elem(1, 0, false),
                BreakType::Consistent
            );
            
            let type_object = if detail {
                PPElement::group(vec![
                    PPElement::text(":".to_string()),
                    PPElement::break_elem(1, 4, false),
                    self.minlog_type().to_enclosed_pp_element(detail)
                ], BreakType::Flexible, 0)
            } else {
                PPElement::text("".to_string())
            };
            
            PPElement::group(vec![
                PPElement::text(self.pconst.name().clone()),
                PPElement::text("<".to_string()),
                PPElement::break_elem(1, 4, false),
                variables,
                PPElement::break_elem(1, 0, false),
                PPElement::text(">".to_string()),
                type_object,
            ], BreakType::Consistent, 0)
        }
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}

pub trait NativeProgramTerm: NativeTermBody {
    fn pconst(&self) -> &Rc<ProgramConstant>;
    
    fn name(&self) -> &str;
    
    fn computation_rules(&self) -> Vec<Rc<RewriteRule>>;
    
    fn rewrite_rules(&self) -> Vec<Rc<RewriteRule>>;
    
    fn parameters(&self) -> &TermSubstitution;
    
    fn to_kernel(&self) -> KernelProgramTerm {
        KernelProgramTerm {
            pconst: self.pconst().clone(),
            parameters: self.parameters().clone(),
        }
    }
}

wrapper_enum::wrapper_enum! {
    #[derive(Clone)]
    pub enum ProgramTerm {
        Kernel(kernel: Rc<KernelProgramTerm>),
        Native(native: Rc<dyn NativeProgramTerm>),
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
        pub fwd fn pconst(&self) -> &Rc<ProgramConstant>
        
        pub fwd fn name(&self) -> &str
        
        pub fwd fn computation_rules(&self) -> Vec<Rc<RewriteRule>>
        
        pub fwd fn rewrite_rules(&self) -> Vec<Rc<RewriteRule>>
        
        pub fwd fn parameters(&self) -> &TermSubstitution
    }
    
    ext trait PrettyPrintable {
        fwd fn to_pp_element(&self, detail: bool) -> PPElement

        fwd fn requires_parens(&self, detail: bool) -> bool

        fwd fn open_paren(&self) -> String

        fwd fn close_paren(&self) -> String
    }
}

impl ProgramTerm {
    pub fn create(pconst: Rc<ProgramConstant>, parameters: TermSubstitution) -> MinlogTerm {
        KernelProgramTerm::create(pconst, parameters)
    }
    
    pub fn into_kernel_program_term(self) -> Rc<KernelProgramTerm> {
        match self {
            ProgramTerm::Kernel(kp) => kp,
            ProgramTerm::Native(np) => Rc::new(np.to_kernel()),
        }
    }
}

impl Hash for ProgramTerm {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            ProgramTerm::Kernel(kp) => kp.hash(state),
            ProgramTerm::Native(np) => np.to_kernel().hash(state),
        }
    }
}

impl PartialEq for ProgramTerm {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ProgramTerm::Kernel(kp1), ProgramTerm::Kernel(kp2)) => kp1 == kp2,
            (ProgramTerm::Native(np1), ProgramTerm::Kernel(kp2)) => np1.to_kernel() == *kp2.as_ref(),
            (ProgramTerm::Kernel(kp1), ProgramTerm::Native(np2)) => *kp1.as_ref() == np2.to_kernel(),
            (ProgramTerm::Native(np1), ProgramTerm::Native(np2)) => np1.eq(np2.as_ref()),
        }
    }
}

impl Eq for ProgramTerm {}

impl Debug for ProgramTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ProgramTerm::Kernel(k) => write!(f, "{:?}", k),
            ProgramTerm::Native(n) => write!(f, "Native({:?})", n.native_to_minlog()),
        }
    }
}

impl From<Rc<KernelProgramTerm>> for ProgramTerm {
    fn from(kp: Rc<KernelProgramTerm>) -> Self {
        ProgramTerm::Kernel(kp)
    }
}

impl From<&Rc<KernelProgramTerm>> for ProgramTerm {
    fn from(kp: &Rc<KernelProgramTerm>) -> Self {
        ProgramTerm::Kernel(kp.clone())
    }
}

impl From<Rc<dyn NativeProgramTerm>> for ProgramTerm {
    fn from(np: Rc<dyn NativeProgramTerm>) -> Self {
        ProgramTerm::Native(np)
    }
}

impl From<&Rc<dyn NativeProgramTerm>> for ProgramTerm {
    fn from(np: &Rc<dyn NativeProgramTerm>) -> Self {
        ProgramTerm::Native(np.clone())
    }
}