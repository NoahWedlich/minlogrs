
use indexmap::IndexSet;
use std::rc::Rc;

use crate::utils::pretty_printer::{PrettyPrintable, PPElement};

use crate::core::substitution::{MatchContext, MatchContextImpl, MatchOutput,
    Substitutable, SubstitutableWith, Substitution};
    
use crate::core::types::minlog_type::MinlogType;
use crate::core::terms::minlog_term::MinlogTerm;
use crate::core::predicates::minlog_predicate::MinlogPredicate;
use crate::core::proofs::minlog_proof::MinlogProof;

use crate::core::terms::term_substitution::TermSubstEntry;
use crate::core::predicates::predicate_substitution::PredSubstEntry;

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum ProofSubstEntry {
    Type(Rc<MinlogType>),
    Term(Rc<MinlogTerm>),
    Predicate(Rc<MinlogPredicate>),
    Proof(Rc<MinlogProof>),
}

impl ProofSubstEntry {
    pub fn is_type(&self) -> bool {
        matches!(self, ProofSubstEntry::Type(_))
    }
    
    pub fn is_term(&self) -> bool {
        matches!(self, ProofSubstEntry::Term(_))
    }
    
    pub fn is_predicate(&self) -> bool {
        matches!(self, ProofSubstEntry::Predicate(_))
    }
    
    pub fn is_proof(&self) -> bool {
        matches!(self, ProofSubstEntry::Proof(_))
    }
    
    pub fn is_term_subst_entry(&self) -> bool {
        matches!(self, ProofSubstEntry::Type(_) | ProofSubstEntry::Term(_))
    }
    
    pub fn is_pred_subst_entry(&self) -> bool {
        !matches!(self, ProofSubstEntry::Proof(_))
    }
    
    pub fn to_type(&self) -> Option<Rc<MinlogType>> {
        match self {
            ProofSubstEntry::Type(t) => Some(t.clone()),
            _ => None,
        }
    }

    pub fn to_term(&self) -> Option<Rc<MinlogTerm>> {
        match self {
            ProofSubstEntry::Term(t) => Some(t.clone()),
            _ => None,
        }
    }

    pub fn to_predicate(&self) -> Option<Rc<MinlogPredicate>> {
        match self {
            ProofSubstEntry::Predicate(p) => Some(p.clone()),
            _ => None,
        }
    }

    pub fn to_proof(&self) -> Option<Rc<MinlogProof>> {
        match self {
            ProofSubstEntry::Proof(p) => Some(p.clone()),
            _ => None,
        }
    }
    
    pub fn to_term_subst_entry(&self) -> Option<TermSubstEntry> {
        match self {
            ProofSubstEntry::Type(t) => Some(TermSubstEntry::Type(t.clone())),
            ProofSubstEntry::Term(t) => Some(TermSubstEntry::Term(t.clone())),
            _ => None,
        }
    }
    
    pub fn to_pred_subst_entry(&self) -> Option<PredSubstEntry> {
        match self {
            ProofSubstEntry::Type(t) => Some(PredSubstEntry::Type(t.clone())),
            ProofSubstEntry::Term(t) => Some(PredSubstEntry::Term(t.clone())),
            ProofSubstEntry::Predicate(p) => Some(PredSubstEntry::Predicate(p.clone())),
            _ => None,
        }
    }
}

impl Substitutable for ProofSubstEntry {
    fn substitute(&self, from: &Self, to: &Self) -> Self {
        match self {
            ProofSubstEntry::Type(t) if from.is_type() && to.is_type() => {
                ProofSubstEntry::Type(t.substitute(&from.to_type().unwrap(), &to.to_type().unwrap()))
            },
            ProofSubstEntry::Term(tm) if from.is_term_subst_entry() && to.is_term_subst_entry() => {
                ProofSubstEntry::Term(tm.substitute(&from.to_term_subst_entry().unwrap(), &to.to_term_subst_entry().unwrap()))
            },
            ProofSubstEntry::Predicate(p) if from.is_pred_subst_entry() && to.is_pred_subst_entry() => {
                ProofSubstEntry::Predicate(p.substitute(&from.to_pred_subst_entry().unwrap(), &to.to_pred_subst_entry().unwrap()))
            },
            ProofSubstEntry::Proof(pr) => {
                ProofSubstEntry::Proof(pr.substitute(from, to))
            },
            _ => {
                self.clone()
            }
        }
    }
    
    fn first_conflict_with(&self, other: &Self) -> Option<(Self, Self)> {
        match (self, other) {
            (ProofSubstEntry::Type(t1), ProofSubstEntry::Type(t2)) => {
                t1.first_conflict_with(t2).map(|(c1, c2)| (c1.into(), c2.into()))
            },
            (ProofSubstEntry::Term(tm1), ProofSubstEntry::Term(tm2)) => {
                tm1.first_conflict_with(tm2).map(|(c1, c2)| (c1.into(), c2.into()))
            },
            (ProofSubstEntry::Predicate(p1), ProofSubstEntry::Predicate(p2)) => {
                p1.first_conflict_with(p2).map(|(c1, c2)| (c1.into(), c2.into()))
            },
            (ProofSubstEntry::Proof(p1), ProofSubstEntry::Proof(p2)) => {
                p1.first_conflict_with(p2)
            },
            _ => {
                panic!("Tried to find conflict between incompatible ProofSubstEntry types");
            }
        }
    }
    
    fn valid_substitution(&self, to: &Self) -> bool {
        match (self, to) {
            (ProofSubstEntry::Proof(p1), ProofSubstEntry::Proof(p2)) => {
                p1.is_goal() && !p2.contains_goal(p1)
                && p2.get_assumptions(&mut IndexSet::new()).is_subset(&p1.get_assumptions(&mut IndexSet::new()))
                && p2.get_free_variables(&mut IndexSet::new()).is_subset(&p1.get_free_variables(&mut IndexSet::new()))
            },
            (_, _) if self.is_pred_subst_entry() && to.is_pred_subst_entry() => {
                let from_pse = self.to_pred_subst_entry().unwrap();
                let to_pse = to.to_pred_subst_entry().unwrap();
                from_pse.valid_substitution(&to_pse)
            },
            _ => {
                false
            }
        }
    }
    
    fn match_with(&self, ctx: &mut impl MatchContext<Self>) -> MatchOutput<Self> {
        match self {
            ProofSubstEntry::Proof(p) => {
                p.match_with(ctx)
            },
            _ if self.is_pred_subst_entry() => {
                match self.to_pred_subst_entry().unwrap().match_with(ctx) {
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

impl PrettyPrintable for ProofSubstEntry {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        match self {
            ProofSubstEntry::Type(t) => t.to_pp_element(detail),
            ProofSubstEntry::Term(tm) => tm.to_pp_element(detail),
            ProofSubstEntry::Predicate(p) => p.to_pp_element(detail),
            ProofSubstEntry::Proof(pr) => pr.to_pp_element(detail),
        }
    }
    
    fn requires_parens(&self, detail: bool) -> bool {
        match self {
            ProofSubstEntry::Type(t) => t.requires_parens(detail),
            ProofSubstEntry::Term(tm) => tm.requires_parens(detail),
            ProofSubstEntry::Predicate(p) => p.requires_parens(detail),
            ProofSubstEntry::Proof(pr) => pr.requires_parens(detail),
        }
    }
    
    fn open_paren(&self) -> String {
        match self {
            ProofSubstEntry::Type(t) => t.open_paren(),
            ProofSubstEntry::Term(tm) => tm.open_paren(),
            ProofSubstEntry::Predicate(p) => p.open_paren(),
            ProofSubstEntry::Proof(pr) => pr.open_paren(),
        }
    }
    
    fn close_paren(&self) -> String {
        match self {
            ProofSubstEntry::Type(t) => t.close_paren(),
            ProofSubstEntry::Term(tm) => tm.close_paren(),
            ProofSubstEntry::Predicate(p) => p.close_paren(),
            ProofSubstEntry::Proof(pr) => pr.close_paren(),
        }
    }
}

impl From<Rc<MinlogType>> for ProofSubstEntry {
    fn from(t: Rc<MinlogType>) -> Self {
        ProofSubstEntry::Type(t)
    }
}

impl From<&Rc<MinlogType>> for ProofSubstEntry {
    fn from(t: &Rc<MinlogType>) -> Self {
        ProofSubstEntry::Type(t.clone())
    }
}

impl From<Rc<MinlogTerm>> for ProofSubstEntry {
    fn from(tm: Rc<MinlogTerm>) -> Self {
        ProofSubstEntry::Term(tm)
    }
}

impl From<&Rc<MinlogTerm>> for ProofSubstEntry {
    fn from(tm: &Rc<MinlogTerm>) -> Self {
        ProofSubstEntry::Term(tm.clone())
    }
}

impl From<Rc<MinlogPredicate>> for ProofSubstEntry {
    fn from(p: Rc<MinlogPredicate>) -> Self {
        ProofSubstEntry::Predicate(p)
    }
}

impl From<&Rc<MinlogPredicate>> for ProofSubstEntry {
    fn from(p: &Rc<MinlogPredicate>) -> Self {
        ProofSubstEntry::Predicate(p.clone())
    }
}

impl From<Rc<MinlogProof>> for ProofSubstEntry {
    fn from(pr: Rc<MinlogProof>) -> Self {
        ProofSubstEntry::Proof(pr)
    }
}

impl From<&Rc<MinlogProof>> for ProofSubstEntry {
    fn from(pr: &Rc<MinlogProof>) -> Self {
        ProofSubstEntry::Proof(pr.clone())
    }
}

impl From<TermSubstEntry> for ProofSubstEntry {
    fn from(tse: TermSubstEntry) -> Self {
        match tse {
            TermSubstEntry::Type(t) => ProofSubstEntry::Type(t),
            TermSubstEntry::Term(tm) => ProofSubstEntry::Term(tm),
        }
    }
}

impl From<&TermSubstEntry> for ProofSubstEntry {
    fn from(tse: &TermSubstEntry) -> Self {
        match tse {
            TermSubstEntry::Type(t) => ProofSubstEntry::Type(t.clone()),
            TermSubstEntry::Term(tm) => ProofSubstEntry::Term(tm.clone()),
        }
    }
}

impl From<PredSubstEntry> for ProofSubstEntry {
    fn from(pse: PredSubstEntry) -> Self {
        match pse {
            PredSubstEntry::Type(t) => ProofSubstEntry::Type(t),
            PredSubstEntry::Term(tm) => ProofSubstEntry::Term(tm),
            PredSubstEntry::Predicate(p) => ProofSubstEntry::Predicate(p),
        }
    }
}

impl From<&PredSubstEntry> for ProofSubstEntry {
    fn from(pse: &PredSubstEntry) -> Self {
        match pse {
            PredSubstEntry::Type(t) => ProofSubstEntry::Type(t.clone()),
            PredSubstEntry::Term(tm) => ProofSubstEntry::Term(tm.clone()),
            PredSubstEntry::Predicate(p) => ProofSubstEntry::Predicate(p.clone()),
        }
    }
}

pub type ProofSubstitution = Substitution<ProofSubstEntry>;
pub type ProofMatchContext = MatchContextImpl<ProofSubstEntry>;

impl ProofSubstitution {
    pub fn admissible_term(&self, term: &Rc<MinlogTerm>) -> bool {
        for free_var in term.get_free_variables(&mut IndexSet::new()) {
            let substituted = self.substitute(&ProofSubstEntry::Term(free_var.clone()));
            if let ProofSubstEntry::Term(t) = substituted {
                if self.substitute(&free_var.minlog_type()) != t.minlog_type() {
                    return false;
                }
            } else {
                panic!("Substitution of term variable resulted in non-term.");
            }
        }
        
        true
    }
    
    pub fn admissible_predicate(&self, predicate: &Rc<MinlogPredicate>) -> bool {
        for pred_var in predicate.get_predicate_variables(&mut IndexSet::new()) {
            let substituted = self.substitute(&ProofSubstEntry::Predicate(pred_var.clone()));
            if let ProofSubstEntry::Predicate(p) = substituted {
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

impl<T: MatchContext<ProofSubstEntry>> MatchContext<PredSubstEntry> for T {
    fn new(patterns: &mut Vec<PredSubstEntry>, instances: &mut Vec<PredSubstEntry>) -> Self {
        let mut pred_patterns = patterns.iter()
            .map(|p| p.into()).collect::<Vec<_>>();
        let mut pred_instances = instances.iter()
            .map(|i| i.into()).collect::<Vec<_>>();
        T::new(&mut pred_patterns, &mut pred_instances)
    }
    
    fn extend(&mut self, pattern: &PredSubstEntry, instance: &PredSubstEntry) {
        self.extend(&pattern.into(), &instance.into());
    }
    
    fn next_pattern(&mut self) -> Option<PredSubstEntry> {
        if let Some(p) = self.next_pattern() {
            if let Some(pse) = p.to_pred_subst_entry() {
                Some(pse)
            } else {
                println!("Warning: Expected predicate pattern but found proof pattern while matching.");
                None
            }
        } else {
            None
        }
    }
    
    fn next_instance(&mut self) -> Option<PredSubstEntry> {
        if let Some(i) = self.next_instance() {
            if let Some(pse) = i.to_pred_subst_entry() {
                Some(pse)
            } else {
                println!("Warning: Expected predicate instance but found proof instance while matching.");
                None
            }
        } else {
            None
        }
    }
    
    fn skip_current(&mut self) {
        self.skip_current();
    }
    
    fn restrict(&mut self, element: &PredSubstEntry) {
        self.restrict(&element.into());
    }
    
    fn is_restricted(&self, element: &PredSubstEntry) -> bool {
        self.is_restricted(&element.into())
    }
    
    fn substitute(&mut self, from: &PredSubstEntry, to: &PredSubstEntry) {
        self.substitute(&from.into(), &to.into());
    }
}

impl<T: SubstitutableWith<PredSubstEntry>> SubstitutableWith<ProofSubstEntry> for T {
    fn substitute_with(&self, from: &ProofSubstEntry, to: &ProofSubstEntry) -> Self {
        match (from, to) {
            (_, _) if from.is_pred_subst_entry() && to.is_pred_subst_entry() => {
                let from_pse = from.to_pred_subst_entry().unwrap();
                let to_pse = to.to_pred_subst_entry().unwrap();
                self.substitute_with(&from_pse, &to_pse)
            },
            _ => {
                self.clone()
            }
        }
    }
}