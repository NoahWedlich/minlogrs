
use std::{rc::Rc, cell::RefCell, hash::{Hash, Hasher}, collections::HashSet};
use crate::{core::substitution::Substitutable, utils::pretty_printer::{BreakType, PPElement, PrettyPrintable}};

use crate::core::substitution::{MatchContext, MatchOutput};

use crate::core::types::minlog_type::MinlogType;

use crate::core::terms::minlog_term::{TermBody, MinlogTerm, Totality};
use crate::core::terms::term_variable::TermVariable;

use crate::core::terms::term_substitution::{TermSubstitution, TermSubstEntry};

use crate::core::structures::program_constant::{ProgramConstant, RewriteRule};

#[derive(Clone, PartialEq, Eq)]
pub struct ProgramTerm {
    pconst: Rc<ProgramConstant>,
    parameters: TermSubstitution,
    blocked_collection: RefCell<bool>,
}

impl ProgramTerm {
    pub fn create(pconst: Rc<ProgramConstant>, mut parameters: TermSubstitution) -> Rc<MinlogTerm> {
        let pconst_vars: Vec<TermSubstEntry> = pconst.get_type_variables().into_iter().map(|tv| tv.into())
            .chain(pconst.get_free_variables().into_iter().map(|fv| fv.into()))
            .collect::<Vec<_>>();
        parameters.restrict(|from| pconst_vars.contains(from));
        
        Rc::new(MinlogTerm::ProgramTerm(ProgramTerm { pconst, parameters, blocked_collection: RefCell::new(false) })
        )
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
    
    fn length(&self) -> usize {
        1
    }
    
    fn depth(&self) -> usize {
        0
    }
    
    fn get_type_variables(&self) -> HashSet<Rc<MinlogType>> {
        self.computation_rules().iter().chain(self.rewrite_rules().iter())
            .flat_map(|r| r.get_type_variables())
            .collect()
    }
    
    fn get_algebra_types(&self) -> HashSet<Rc<MinlogType>> {
        self.computation_rules().iter().chain(self.rewrite_rules().iter())
            .flat_map(|r| r.get_algebra_types())
            .collect()
    }
    
    fn get_free_variables(&self) -> HashSet<Rc<MinlogTerm>> {
        // Get rid of these hacks to prevent infinite recursion
        if *self.blocked_collection.borrow() {
            return HashSet::new();
        }
        
        *self.blocked_collection.borrow_mut() = true;
        
        let result = self.computation_rules().iter().chain(self.rewrite_rules().iter())
            .flat_map(|r| r.get_free_variables())
            .collect();
        
        *self.blocked_collection.borrow_mut() = false;
        
        result
    }
    
    fn get_bound_variables(&self) -> HashSet<Rc<MinlogTerm>> {
        if *self.blocked_collection.borrow() {
            return HashSet::new();
        }
        
        *self.blocked_collection.borrow_mut() = true;
        
        let result = self.computation_rules().iter().chain(self.rewrite_rules().iter())
            .flat_map(|r| r.get_bound_variables())
            .collect();
        
        *self.blocked_collection.borrow_mut() = false;
        
        result
    }
    
    fn get_program_terms(&self) -> HashSet<Rc<MinlogTerm>> {
        HashSet::from([Rc::new(MinlogTerm::ProgramTerm(self.clone()))])
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
    
    fn totality(&self, _bound: &mut HashSet<TermVariable>) -> Totality {
        self.pconst.totality()
    }
    
    fn substitute(&self, from: &TermSubstEntry, to: &TermSubstEntry) -> Rc<MinlogTerm> {
        if let Some(tm) = from.to_term() && tm.is_program_term() && self == tm.to_program_term().unwrap() {
            to.to_term().unwrap()
        } else {
            let new_params = TermSubstitution::from_pairs(
                self.parameters.pairs().iter()
                    .map(|(f, t)| (f.substitute(from, to), t.substitute(from, to)))
                    .collect()
            );
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
        let tvars = self.pconst.get_type_variables();
        let tmvars = self.pconst.get_free_variables();
        
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
            
            let mut vars = vec![];
            
            if has_tvars {
                vars.push(tvars);
                if has_tmvars {
                    vars.push(PPElement::break_elem(4, 0, false));
                }
            }
            
            if has_tmvars {
                vars.push(tmvars);
            }
            
            PPElement::group(vec![
                PPElement::text(self.pconst.name().clone()),
                PPElement::text("<".to_string()),
                PPElement::break_elem(1, 4, false),
                
                PPElement::break_elem(1, 0, false),
                PPElement::text(">".to_string())
            ], BreakType::Consistent, 0)
        }
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}

impl Hash for ProgramTerm {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.pconst.hash(state);
        self.parameters.hash(state);
    }
}