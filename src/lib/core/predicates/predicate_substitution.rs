
use indexmap::IndexSet;
use std::rc::Rc;
use crate::utils::pretty_printer::PrettyPrintable;

use crate::core::substitution::{MatchOutput,
    Substitutable, SubstitutableWith, Substitution};

use crate::core::types::minlog_type::MinlogType;
use crate::core::terms::minlog_term::MinlogTerm;
use crate::core::predicates::minlog_predicate::MinlogPredicate;

use crate::core::terms::term_substitution::TermSubstEntry;

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum PredSubstEntry {
    Type(Rc<MinlogType>),
    Term(Rc<MinlogTerm>),
    Predicate(Rc<MinlogPredicate>),
}

impl PredSubstEntry {
    pub fn is_type(&self) -> bool {
        matches!(self, PredSubstEntry::Type(_))
    }
    
    pub fn is_term(&self) -> bool {
        matches!(self, PredSubstEntry::Term(_))
    }
    
    pub fn is_predicate(&self) -> bool {
        matches!(self, PredSubstEntry::Predicate(_))
    }
    
    pub fn is_term_subst_entry(&self) -> bool {
        matches!(self, PredSubstEntry::Type(_) | PredSubstEntry::Term(_))
    }

    pub fn to_type(&self) -> Option<Rc<MinlogType>> {
        if let PredSubstEntry::Type(t) = self {
            Some(t.clone())
        } else {
            None
        }
    }
    
    pub fn to_term(&self) -> Option<Rc<MinlogTerm>> {
        if let PredSubstEntry::Term(tm) = self {
            Some(tm.clone())
        } else {
            None
        }
    }
    
    pub fn to_predicate(&self) -> Option<Rc<MinlogPredicate>> {
        if let PredSubstEntry::Predicate(p) = self {
            Some(p.clone())
        } else {
            None
        }
    }
    
    pub fn to_term_subst_entry(&self) -> Option<TermSubstEntry> {
        match self {
            PredSubstEntry::Type(t) => Some(TermSubstEntry::Type(t.clone())),
            PredSubstEntry::Term(tm) => Some(TermSubstEntry::Term(tm.clone())),
            _ => None,
        }
    }
}

impl Substitutable for PredSubstEntry {
    fn substitute(&self, from: &Self, to: &Self) -> Self {
        match (self, from, to) {
            (PredSubstEntry::Type(t), PredSubstEntry::Type(from_t), PredSubstEntry::Type(to_t)) => {
                PredSubstEntry::Type(t.substitute(from_t, to_t))
            },
            (PredSubstEntry::Term(tm), _, _) if from.is_term_subst_entry() && to.is_term_subst_entry() => {
                PredSubstEntry::Term(tm.substitute(&from.to_term_subst_entry().unwrap(), &to.to_term_subst_entry().unwrap()))
            },
            (PredSubstEntry::Predicate(p), f, t) => {
                let substituted = p.substitute(from, to);
                match (f, t) {
                    (PredSubstEntry::Predicate(from_pred), PredSubstEntry::Predicate(to_pred)) => {
                        substituted.substitute(
                            &from_pred.extracted_type_pattern().into(),
                            &to_pred.extracted_type_pattern().into()
                        ).into()
                    },
                    _ => {
                        substituted.into()
                    }
                }
            },
            _ => {
                self.clone()
            }
        }
    }
    
    fn first_conflict_with(&self, other: &Self) -> Option<(Self, Self)> {
        match (self, other) {
            (PredSubstEntry::Type(t1), PredSubstEntry::Type(t2)) => {
                t1.first_conflict_with(t2).map(|(f, o)| (f.into(), o.into()))
            },
            (PredSubstEntry::Term(tm1), PredSubstEntry::Term(tm2)) => {
                tm1.first_conflict_with(tm2).map(|(f, o)| (f.into(), o.into()))
            },
            (PredSubstEntry::Predicate(p1), PredSubstEntry::Predicate(p2)) => {
                p1.first_conflict_with(p2)
            },
            _ => {
                panic!("Tried to find conflict between incompatible PredSubstEntry types");
            }
        }
    }
    
    fn valid_substitution(&self, to: &Self) -> bool {
        match (self, to) {
            (PredSubstEntry::Predicate(p), PredSubstEntry::Predicate(to_p)) => {
                p.is_variable() && !to_p.contains_predicate_variable(p)
            },
            (_, _) if self.is_term_subst_entry() && to.is_term_subst_entry() => {
                let from_tse = self.to_term_subst_entry().unwrap();
                let to_tse = to.to_term_subst_entry().unwrap();
                from_tse.valid_substitution(&to_tse)
            },
            _ => {
                false
            }
        }
    }
    
    fn match_with(&self, instance: &Self) -> MatchOutput<PredSubstEntry> {
        if let Some(pat_tse) = self.to_term_subst_entry() && let Some(inst_tse) = instance.to_term_subst_entry() {
            pat_tse.match_with(&inst_tse).into()
        } else if let Some(pat_p) = self.to_predicate() && let Some(inst_p) = instance.to_predicate() {
            pat_p.match_with(&inst_p)
        } else {
            MatchOutput::FailedMatch
        }
    }
}

impl PrettyPrintable for PredSubstEntry {
    fn to_pp_element(&self, detail: bool) -> crate::utils::pretty_printer::PPElement {
        match self {
            PredSubstEntry::Type(t) => t.to_pp_element(detail),
            PredSubstEntry::Term(tm) => tm.to_pp_element(detail),
            PredSubstEntry::Predicate(p) => p.to_pp_element(detail),
        }
    }
    
    fn requires_parens(&self, detail: bool) -> bool {
        match self {
            PredSubstEntry::Type(t) => t.requires_parens(detail),
            PredSubstEntry::Term(tm) => tm.requires_parens(detail),
            PredSubstEntry::Predicate(p) => p.requires_parens(detail),
        }
    }
    
    fn open_paren(&self) -> String {
        match self {
            PredSubstEntry::Type(t) => t.open_paren(),
            PredSubstEntry::Term(tm) => tm.open_paren(),
            PredSubstEntry::Predicate(p) => p.open_paren(),
        }
    }
    
    fn close_paren(&self) -> String {
        match self {
            PredSubstEntry::Type(t) => t.close_paren(),
            PredSubstEntry::Term(tm) => tm.close_paren(),
            PredSubstEntry::Predicate(p) => p.close_paren(),
        }
    }
}

impl From<Rc<MinlogType>> for PredSubstEntry {
    fn from(t: Rc<MinlogType>) -> Self {
        PredSubstEntry::Type(t)
    }
}

impl From<&Rc<MinlogType>> for PredSubstEntry {
    fn from(t: &Rc<MinlogType>) -> Self {
        PredSubstEntry::Type(Rc::clone(t))
    }
}

impl From<Rc<MinlogTerm>> for PredSubstEntry {
    fn from(tm: Rc<MinlogTerm>) -> Self {
        PredSubstEntry::Term(tm)
    }
}

impl From<&Rc<MinlogTerm>> for PredSubstEntry {
    fn from(tm: &Rc<MinlogTerm>) -> Self {
        PredSubstEntry::Term(Rc::clone(tm))
    }
}

impl From<Rc<MinlogPredicate>> for PredSubstEntry {
    fn from(p: Rc<MinlogPredicate>) -> Self {
        PredSubstEntry::Predicate(p)
    }
}

impl From<&Rc<MinlogPredicate>> for PredSubstEntry {
    fn from(p: &Rc<MinlogPredicate>) -> Self {
        PredSubstEntry::Predicate(Rc::clone(p))
    }
}

impl From<TermSubstEntry> for PredSubstEntry {
    fn from(tse: TermSubstEntry) -> Self {
        match tse {
            TermSubstEntry::Type(t) => PredSubstEntry::Type(t),
            TermSubstEntry::Term(tm) => PredSubstEntry::Term(tm),
        }
    }
}

impl From<&TermSubstEntry> for PredSubstEntry {
    fn from(tse: &TermSubstEntry) -> Self {
        match tse {
            TermSubstEntry::Type(t) => PredSubstEntry::Type(Rc::clone(t)),
            TermSubstEntry::Term(tm) => PredSubstEntry::Term(Rc::clone(tm)),
        }
    }
}

pub type PredicateSubstitution = Substitution<PredSubstEntry>;

impl PredicateSubstitution {
    pub fn admissible_term(&self, term: &Rc<MinlogTerm>) -> bool {
        for free_var in term.get_free_variables(&mut IndexSet::new()) {
            let substituted = self.substitute(&PredSubstEntry::Term(free_var.clone()));
            if let PredSubstEntry::Term(t) = substituted {
                if self.substitute(&free_var.minlog_type()) != t.minlog_type() {
                    return false;
                }
            } else {
                panic!("Substitution of term variable resulted in non-term.");
            }
        }
        true
    }
    
    pub fn admissible(&self, predicate: &Rc<MinlogPredicate>) -> bool {
        for pred_var in predicate.get_predicate_variables(&mut IndexSet::new()) {
            let substituted = self.substitute(&PredSubstEntry::Predicate(pred_var.clone()));
            if let PredSubstEntry::Predicate(p) = substituted {
                if self.substitute(&pred_var.arity()) != p.arity() {
                    return false;
                }
            } else {
                panic!("Substitution of predicate variable resulted in non-predicate.");
            }
        }
        true
    }
}

impl SubstitutableWith<PredSubstEntry> for Rc<MinlogPredicate> {
    fn substitute_with(&self, from: &PredSubstEntry, to: &PredSubstEntry) -> Self {
        self.substitute(from, to)
    }
}

impl <T: SubstitutableWith<TermSubstEntry>> SubstitutableWith<PredSubstEntry> for T {
    fn substitute_with(&self, from: &PredSubstEntry, to: &PredSubstEntry) -> Self {
        match (from, to) {
            (_, _) if from.is_term_subst_entry() && to.is_term_subst_entry() => {
                let from_tse = from.to_term_subst_entry().unwrap();
                let to_tse = to.to_term_subst_entry().unwrap();
                self.substitute_with(&from_tse, &to_tse)
            },
            (PredSubstEntry::Predicate(from_p), PredSubstEntry::Predicate(to_p)) => {
                self.substitute_with(&from_p.extracted_type_pattern().into(), &to_p.extracted_type_pattern().into())
            },
            _ => {
                self.clone()
            }            
        }
    }
}

impl From<MatchOutput<TermSubstEntry>> for MatchOutput<PredSubstEntry> {
    fn from(output: MatchOutput<TermSubstEntry>) -> Self {
        match output {
            MatchOutput::Substitution(p, i) => {
                MatchOutput::Substitution(p.into(), i.into())
            },
            MatchOutput::Matched(conditions) => {
                MatchOutput::Matched(conditions.into_iter().map(|(f, o)| (f.into(), o.into())).collect())
            }
            MatchOutput::FailedMatch => MatchOutput::FailedMatch,
        }
    }
}