
use std::rc::Rc;

use crate::utils::pretty_printer::PrettyPrintable;

use crate::core::substitution::{MatchContext, MatchContextImpl, MatchOutput,
    Substitutable, SubstitutableWith, Substitution};

use crate::core::types::minlog_type::MinlogType;
use crate::core::terms::minlog_term::MinlogTerm;
use crate::core::predicates::minlog_predicate::MinlogPredicate;
use crate::core::formulas::minlog_formula::MinlogFormula;

use crate::core::terms::term_substitution::TermSubstEntry;

#[derive(Clone, PartialEq, Eq)]
pub enum PredSubstEntry {
    Type(Rc<MinlogType>),
    Term(Rc<MinlogTerm>),
    Predicate(Rc<MinlogPredicate>),
    Formula(Rc<MinlogFormula>),
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

    pub fn is_formula(&self) -> bool {
        matches!(self, PredSubstEntry::Formula(_))
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
    
    pub fn to_formula(&self) -> Option<Rc<MinlogFormula>> {
        if let PredSubstEntry::Formula(f) = self {
            Some(f.clone())
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
            (PredSubstEntry::Predicate(p), _, _) => {
                PredSubstEntry::Predicate(p.substitute(from, to))
            },
            (PredSubstEntry::Formula(f), _, _) => {
                PredSubstEntry::Formula(f.substitute(from, to))
            },
            _ => {
                panic!("Tried to substitute between incompatible PredSubstEntry types");
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
            (PredSubstEntry::Formula(f1), PredSubstEntry::Formula(f2)) => {
                f1.first_conflict_with(f2)
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
    
    fn match_with(&self, ctx: &mut impl MatchContext<PredSubstEntry>) -> MatchOutput<PredSubstEntry> {
        match self {
            PredSubstEntry::Predicate(p) => {
                p.match_with(ctx)
            },
            PredSubstEntry::Formula(f) => {
                f.match_with(ctx)
            },
            _ if self.is_term_subst_entry() => {
                match self.to_term_subst_entry().unwrap().match_with(ctx) {
                    MatchOutput::Substitution(p, i) => {
                        MatchOutput::Substitution(p.into(), i.into())
                    },
                    MatchOutput::Matched => MatchOutput::Matched,
                    MatchOutput::FailedMatch => MatchOutput::FailedMatch,
                }
            },
            _ => MatchOutput::FailedMatch,
        }
    }
}

impl PrettyPrintable for PredSubstEntry {
    fn to_pp_element(&self, detail: bool) -> crate::utils::pretty_printer::PPElement {
        match self {
            PredSubstEntry::Type(t) => t.to_pp_element(detail),
            PredSubstEntry::Term(tm) => tm.to_pp_element(detail),
            PredSubstEntry::Predicate(p) => p.to_pp_element(detail),
            PredSubstEntry::Formula(f) => f.to_pp_element(detail),
        }
    }
    
    fn requires_parens(&self, detail: bool) -> bool {
        match self {
            PredSubstEntry::Type(t) => t.requires_parens(detail),
            PredSubstEntry::Term(tm) => tm.requires_parens(detail),
            PredSubstEntry::Predicate(p) => p.requires_parens(detail),
            PredSubstEntry::Formula(f) => f.requires_parens(detail),
        }
    }
    
    fn open_paren(&self) -> String {
        match self {
            PredSubstEntry::Type(t) => t.open_paren(),
            PredSubstEntry::Term(tm) => tm.open_paren(),
            PredSubstEntry::Predicate(p) => p.open_paren(),
            PredSubstEntry::Formula(f) => f.open_paren(),
        }
    }
    
    fn close_paren(&self) -> String {
        match self {
            PredSubstEntry::Type(t) => t.close_paren(),
            PredSubstEntry::Term(tm) => tm.close_paren(),
            PredSubstEntry::Predicate(p) => p.close_paren(),
            PredSubstEntry::Formula(f) => f.close_paren(),
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

impl From<Rc<MinlogFormula>> for PredSubstEntry {
    fn from(f: Rc<MinlogFormula>) -> Self {
        PredSubstEntry::Formula(f)
    }
}

impl From<&Rc<MinlogFormula>> for PredSubstEntry {
    fn from(f: &Rc<MinlogFormula>) -> Self {
        PredSubstEntry::Formula(Rc::clone(f))
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
pub type PredicateMatchContext = MatchContextImpl<PredSubstEntry>;

impl PredicateSubstitution {
    pub fn admissible_term(&self, term: &Rc<MinlogTerm>) -> bool {
        for free_var in term.get_free_variables() {
            let substituted = self.apply(&PredSubstEntry::Term(free_var.clone()));
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
        for pred_var in predicate.get_predicate_variables() {
            let substituted = self.apply(&PredSubstEntry::Predicate(pred_var.clone()));
            if let PredSubstEntry::Predicate(p) = substituted {
                if self.substitute_all(&pred_var.arity()) != p.arity() {
                    return false;
                }
            } else {
                panic!("Substitution of predicate variable resulted in non-predicate.");
            }
        }
        true
    }
}

impl<T: MatchContext<PredSubstEntry>> MatchContext<TermSubstEntry> for T {
    fn new(patterns: &mut Vec<TermSubstEntry>, instances: &mut Vec<TermSubstEntry>) -> Self {
        let mut pred_patterns = patterns.iter()
            .map(|p| p.into()).collect::<Vec<_>>();
        let mut pred_instances = instances.iter()
            .map(|i| i.into()).collect::<Vec<_>>();
        T::new(&mut pred_patterns, &mut pred_instances)
    }
    
    fn extend(&mut self, pattern: &TermSubstEntry, instance: &TermSubstEntry) {
        self.extend(&pattern.into(), &instance.into());
    }
    
    fn next_pattern(&mut self) -> Option<TermSubstEntry> {
        if let Some(p) = self.next_pattern() {
            if let Some(tse) = p.to_term_subst_entry() {
                Some(tse)
            } else {
                println!("Warning: Expected term pattern but found predicate pattern while matching.");
                None
            }
        } else {
            None
        }
    }
    
    fn next_instance(&mut self) -> Option<TermSubstEntry> {
        if let Some(i) = self.next_instance() {
            if let Some(tse) = i.to_term_subst_entry() {
                Some(tse)
            } else {
                println!("Warning: Expected term instance but found predicate instance while matching.");
                None
            }
        } else {
            None
        }
    }
    
    fn skip_current(&mut self) {
        self.skip_current();
    }
    
    fn restrict(&mut self, element: &TermSubstEntry) {
        self.restrict(&element.into());
    }
    
    fn is_restricted(&self, element: &TermSubstEntry) -> bool {
        self.is_restricted(&element.into())
    }
    
    fn substitute(&mut self, from: &TermSubstEntry, to: &TermSubstEntry) {
        self.substitute(&from.into(), &to.into());
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
            _ => {
                self.clone()
            }
        }
    }
}