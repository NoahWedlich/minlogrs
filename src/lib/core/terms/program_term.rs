
use indexmap::IndexSet;
use std::rc::Rc;
use crate::{core::substitution::Substitutable, utils::pretty_printer::{BreakType, PPElement, PrettyPrintable}};

use crate::core::substitution::{MatchContext, MatchOutput};

use crate::core::types::minlog_type::MinlogType;

use crate::core::terms::minlog_term::{TermBody, MinlogTerm};
use crate::core::terms::term_variable::TermVariable;

use crate::core::terms::term_substitution::{TermSubstitution, TermSubstEntry};

use crate::core::structures::program_constant::{ProgramConstant, RewriteRule};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ProgramTerm {
    pconst: Rc<ProgramConstant>,
    parameters: TermSubstitution,
}

impl ProgramTerm {
    pub fn create(pconst: Rc<ProgramConstant>, mut parameters: TermSubstitution) -> Rc<MinlogTerm> {
        let pconst_vars: Vec<TermSubstEntry> = pconst.get_type_variables(&mut IndexSet::new()).into_iter().map(|tv| tv.into())
            .chain(pconst.get_free_variables(&mut IndexSet::new()).into_iter().map(|fv| fv.into()))
            .collect::<Vec<_>>();
        
        parameters.restrict(|from| pconst_vars.contains(from));
        
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
            .map(|r| self.parameters.substitute(r))
            .collect()
    }
    
    pub fn rewrite_rules(&self) -> Vec<Rc<RewriteRule>> {
        self.pconst.rewrite_rules().iter()
            .map(|r| self.parameters.substitute(r))
            .collect()
    }
    
    pub fn compute(&self, term: &Rc<MinlogTerm>) -> (Rc<MinlogTerm>, bool) {
        for rule in self.computation_rules().iter() {
            if let Some(subst) = TermSubstitution::match_with(&rule.pattern().into(), &term.into()) {
                return (subst.substitute::<TermSubstEntry>(&rule.result().into()).to_term().unwrap(), true);
            }
        }
        
        for rule in self.rewrite_rules().iter() {
            if let Some(subst) = TermSubstitution::match_with(&rule.pattern().into(), &term.into()) {
                return (subst.substitute::<TermSubstEntry>(&rule.result().into()).to_term().unwrap(), true);
            }
        }
        
        (term.clone(), false)
    }
    
    pub fn parameters(&self) -> &TermSubstitution {
        &self.parameters
    }
}

impl TermBody for ProgramTerm {
    fn minlog_type(&self) -> Rc<MinlogType> {
        let pc_type = self.pconst.minlog_type();
        self.parameters.substitute::<TermSubstEntry>(&pc_type.into()).to_type().unwrap()
    }
    
    fn normalize(&self, _eta: bool, _pi: bool) -> Rc<MinlogTerm> {
        ProgramTerm::create(self.pconst.clone(), self.parameters.clone())
    }
    
    fn remove_nulls(&self) -> Option<Rc<MinlogTerm>> {
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
        if visited.contains(&MinlogTerm::ProgramTerm(self.clone())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogTerm::ProgramTerm(self.clone()));
            
            self.computation_rules().iter().chain(self.rewrite_rules().iter())
                .flat_map(|r| r.get_type_variables(visited))
                .collect()
        }
    }
    
    fn get_algebra_types(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogType>> {
        if visited.contains(&MinlogTerm::ProgramTerm(self.clone())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogTerm::ProgramTerm(self.clone()));
            
            self.computation_rules().iter().chain(self.rewrite_rules().iter())
                .flat_map(|r| r.get_algebra_types(visited))
                .collect()
        }
    }
    
    fn get_free_variables(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogTerm>> {
        if visited.contains(&MinlogTerm::ProgramTerm(self.clone())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogTerm::ProgramTerm(self.clone()));
            
            self.computation_rules().iter().chain(self.rewrite_rules().iter())
                .flat_map(|r| r.get_free_variables(visited))
                .collect()
        }
    }
    
    fn get_bound_variables(&self, visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogTerm>> {
        if visited.contains(&MinlogTerm::ProgramTerm(self.clone())) {
            IndexSet::new()
        } else {
            visited.insert(MinlogTerm::ProgramTerm(self.clone()));
            
            self.computation_rules().iter().chain(self.rewrite_rules().iter())
                .flat_map(|r| r.get_bound_variables(visited))
                .collect()
        }
    }
    
    fn get_program_terms(&self, _visited: &mut IndexSet<MinlogTerm>) -> IndexSet<Rc<MinlogTerm>> {
        IndexSet::from([Rc::new(MinlogTerm::ProgramTerm(self.clone()))])
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
    
    fn substitute(&self, from: &TermSubstEntry, to: &TermSubstEntry) -> Rc<MinlogTerm> {
        if let Some(tm) = from.to_term() && tm.is_program_term() && self == tm.to_program_term().unwrap() {
            to.to_term().unwrap()
        } else {
            let mut new_params = self.parameters.clone();
            new_params.extend((from.clone(), to.clone()));
            
            ProgramTerm::create(self.pconst.clone(), new_params)
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
        
        for (from, to) in self.parameters.pairs().iter() {
            let other_to = other_pterm.parameters.substitute::<TermSubstEntry>(from);
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
                
                pterm_pattern.parameters.add_subst_match(&pterm_instance.parameters, ctx);
                
                MatchOutput::Matched
            },
            _ => MatchOutput::FailedMatch,
        }
    }
}

impl PrettyPrintable for ProgramTerm {
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