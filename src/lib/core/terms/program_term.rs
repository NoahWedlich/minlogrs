
use std::rc::Rc;
use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};

use crate::core::substitution::{MatchContext, MatchOutput};

use crate::core::types::minlog_type::MinlogType;

use crate::core::terms::minlog_term::{TermBody, MinlogTerm, Totality};
use crate::core::terms::term_variable::TermVariable;

use crate::core::terms::term_substitution::TermSubstitution;

use crate::core::terms::term_substitution::TermSubstEntry;

use crate::core::structures::program_constant::{ProgramConstant, RewriteRule};

#[derive(Clone, PartialEq, Eq)]
pub struct ProgramTerm {
    pconst: Rc<ProgramConstant>,
    parameters: Vec<(Rc<MinlogType>, Rc<MinlogType>)>,
}

impl ProgramTerm {
    pub fn create(pconst: Rc<ProgramConstant>, parameters: Vec<(Rc<MinlogType>, Rc<MinlogType>)>) -> Rc<MinlogTerm> {
        let expected_vars = pconst.get_type_variables();
        
        for (from, _) in parameters.iter() {
            if !expected_vars.iter().any(|v| v == from) {
                panic!("Type variable '{}' not expected in parameters for program constant '{}'", from.debug_string(), pconst.name());
            }
        }
        
        for var in expected_vars {
            if !parameters.iter().any(|(from, _)| from == var) {
                panic!("Missing type variable '{}' in parameters for program constant '{}'", var.debug_string(), pconst.name());
            }
        }
        
        Rc::new(MinlogTerm::ProgramTerm(ProgramTerm { pconst, parameters }))
    }
    
    pub fn pconst(&self) -> &Rc<ProgramConstant> {
        &self.pconst
    }
    
    pub fn name(&self) -> &String {
        self.pconst.name()
    }
    
    pub fn computation_rules(&self) -> Vec<Rc<RewriteRule>> {
        self.pconst.computation_rules().iter()
            .map(|r| self.substitution().substitute(r))
            .collect()
    }
    
    pub fn rewrite_rules(&self) -> Vec<Rc<RewriteRule>> {
        self.pconst.rewrite_rules().iter()
            .map(|r| self.substitution().substitute(r))
            .collect()
    }
    
    pub fn parameters(&self) -> &Vec<(Rc<MinlogType>, Rc<MinlogType>)> {
        &self.parameters
    }
    
    pub fn substitution(&self) -> TermSubstitution {
        TermSubstitution::from_pairs(self.parameters.iter()
            .map(|(from, to)| (from.into(), to.into()))
            .collect())
    }
}

impl TermBody for ProgramTerm {
    fn minlog_type(&self) -> Rc<MinlogType> {
        let pc_type = self.pconst.minlog_type();
        self.substitution().substitute::<TermSubstEntry>(&pc_type.into()).to_type().unwrap()
    }
    
    fn normalize(&self, _eta: bool, _pi: bool) -> Rc<MinlogTerm> {
        ProgramTerm::create(self.pconst.clone(), self.parameters.clone())
    }
    
    fn length(&self) -> usize {
        1
    }
    
    fn depth(&self) -> usize {
        0
    }
    
    fn get_program_terms(&self) -> Vec<Rc<MinlogTerm>> {
        vec![Rc::new(MinlogTerm::ProgramTerm(self.clone()))]
    }
    
    fn alpha_equivalent(&self, other: &Rc<MinlogTerm>,
        _forward: &mut Vec<(TermVariable, TermVariable)>,
        _backward: &mut Vec<(TermVariable, TermVariable)>) -> bool {
            
        if let Some(other_body) = other.to_program_term() {
            self.pconst == other_body.pconst && self.parameters == other_body.parameters
        } else {
            false
        }
    }
    
    fn totality(&self, _bound: &mut Vec<TermVariable>) -> Totality {
        self.pconst.totality()
    }
    
    fn substitute(&self, from: &TermSubstEntry, to: &TermSubstEntry) -> Rc<MinlogTerm> {
        match (from, to) {
            (TermSubstEntry::Type(_), TermSubstEntry::Type(_)) => {
                let mut subst = self.substitution();
                subst.extend((from.clone(), to.clone()));
                let mut new_params = subst.pairs().iter()
                    .map(|(f, t)| (f.to_type().unwrap(), t.to_type().unwrap()))
                    .collect::<Vec<_>>();
                
                for (f, _) in self.parameters.iter() {
                    if !new_params.iter().any(|(nf, _)| nf == f) {
                        new_params.push((f.clone(), f.clone()));
                    }
                }
                
                ProgramTerm::create(self.pconst.clone(), new_params)
            },
            (TermSubstEntry::Term(_), TermSubstEntry::Term(_)) => {
                ProgramTerm::create(self.pconst.clone(), self.parameters.clone())
            },
            _ => {
                panic!("Tried to substitute between incompatible TermSubstEntry types");
            }
        }
    }
    
    fn first_conflict_with(&self, other: &Rc<MinlogTerm>) -> Option<(TermSubstEntry, TermSubstEntry)> {
        if !other.is_program_term() {
            panic!("Tried to find conflict between incompatible TermSubstEntry types");
        }
        
        let other_pterm = other.to_program_term().unwrap();
        
        if self.pconst != other_pterm.pconst {
            return Some((Rc::new(MinlogTerm::ProgramTerm(self.clone())).into(), other.clone().into()));
        }
        
        for (from, to) in self.parameters.iter() {
            let other_to = self.substitution().substitute::<TermSubstEntry>(&from.into()).to_type().unwrap();
            if let Some(conflict) = to.first_conflict_with(&other_to) {
                return Some((conflict.0.into(), conflict.1.into()));
            }
        }
        
        for (from, to) in other_pterm.parameters.iter() {
            let self_to = other_pterm.substitution().substitute::<TermSubstEntry>(&from.into()).to_type().unwrap();
            if let Some(conflict) = to.first_conflict_with(&self_to) {
                return Some((conflict.0.into(), conflict.1.into()));
            }
        }
        
        None
    }
    
    fn match_with(&self, ctx: &mut impl MatchContext<TermSubstEntry>) -> MatchOutput<TermSubstEntry> {
        let pattern = ctx.next_pattern().unwrap();
        let instance = ctx.next_instance().unwrap();
        
        match (pattern, instance) {
            (TermSubstEntry::Term(p), TermSubstEntry::Term(i)) => {
                if !p.is_program_term() || !i.is_program_term() {
                    return MatchOutput::FailedMatch;
                }
                
                let pterm_pattern = p.to_program_term().unwrap();
                let pterm_instance = i.to_program_term().unwrap();
                
                if pterm_pattern.pconst != pterm_instance.pconst {
                    return MatchOutput::FailedMatch;
                }
                
                for (from, to) in pterm_pattern.parameters.iter() {
                    let instance_to = pterm_pattern.substitution().substitute::<TermSubstEntry>(&from.into()).to_type().unwrap();
                    if from != &instance_to {
                        ctx.extend(&to.into(), &instance_to.into());
                    }
                }
                MatchOutput::Matched
            },
            _ => MatchOutput::FailedMatch,
        }
    }
}

impl PrettyPrintable for ProgramTerm {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        if self.parameters.is_empty() {
            PPElement::text(self.pconst.name().clone())
        } else {
            let mut params = vec![];
            for (i, (from, to)) in self.parameters.iter().enumerate() {
                let param_elem = if detail && from != to {
                    PPElement::group(vec![
                        from.to_pp_element(detail),
                        PPElement::break_elem(1, 4, false),
                        PPElement::text("=".to_string()),
                        PPElement::break_elem(1, 4, false),
                        to.to_enclosed_pp_element(detail)
                    ], BreakType::Flexible, 0)
                } else {
                    to.to_enclosed_pp_element(detail)
                };
                
                params.push(
                    if i < self.parameters.iter().len() - 1 {
                        PPElement::group(vec![
                            param_elem,
                            PPElement::break_elem(0, 4, false),
                            PPElement::text(",".to_string())
                        ], BreakType::Flexible, 0)
                    } else {
                        param_elem
                    }
                );
                
                if i < self.parameters.iter().len() - 1 {
                    params.push(PPElement::break_elem(1, 4, false));
                }
            }
            
            PPElement::group(vec![
                PPElement::text(self.pconst.name().clone()),
                PPElement::text("<".to_string()),
                PPElement::break_elem(1, 4, false),
                PPElement::group(params, BreakType::Flexible, 0),
                PPElement::break_elem(1, 0, false),
                PPElement::text(">".to_string())
            ], BreakType::Consistent, 0)
        }
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}