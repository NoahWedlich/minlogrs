
use crate::includes::{
    essential::*,
    utils::*,
    core::{
        types::*,
        terms::*,
    }
};

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum TermSubstEntry {
    Type(Rc<MinlogType>),
    Term(Rc<MinlogTerm>),
}

impl TermSubstEntry {
    pub fn is_type(&self) -> bool {
        matches!(self, TermSubstEntry::Type(_))
    }
    
    pub fn is_term(&self) -> bool {
        matches!(self, TermSubstEntry::Term(_))
    }
    
    pub fn to_type(&self) -> Option<Rc<MinlogType>> {
        if let TermSubstEntry::Type(t) = self {
            Some(t.clone())
        } else {
            None
        }
    }
    
    pub fn to_term(&self) -> Option<Rc<MinlogTerm>> {
        if let TermSubstEntry::Term(tm) = self {
            Some(tm.clone())
        } else {
            None
        }
    }
}

impl Substitutable for TermSubstEntry {
    fn substitute(&self, from: &Self, to: &Self) -> Self {
        match (self, from, to) {
            (TermSubstEntry::Type(t), TermSubstEntry::Type(from_t), TermSubstEntry::Type(to_t)) => {
                TermSubstEntry::Type(t.substitute(from_t, to_t))
            },
            (TermSubstEntry::Term(tm), _, _) => {
                TermSubstEntry::Term(tm.substitute(from, to))
            },
            _ => {
                self.clone()
            }
        }
    }
    
    fn first_conflict_with(&self, other: &Self) -> Option<(Self, Self)> {
        match (self, other) {
            (TermSubstEntry::Type(t1), TermSubstEntry::Type(t2)) => {
                t1.first_conflict_with(t2).map(|(f, o)| (f.into(), o.into()))
            },
            (TermSubstEntry::Term(tm1), TermSubstEntry::Term(tm2)) => {
                tm1.first_conflict_with(tm2)
            },
            _ => {
                panic!("Tried to find conflict between incompatible TermSubstEntry types");
            }
        }
    }
    
    fn valid_substitution(&self, to: &Self) -> bool {
        match (self, to) {
            (TermSubstEntry::Type(t), TermSubstEntry::Type(to_t)) => {
                t.valid_substitution(to_t)
            },
            (TermSubstEntry::Term(tm), TermSubstEntry::Term(to_tm)) => {
                tm.is_variable() && !to_tm.contains_free_variable(tm)
            },
            _ => {
                false
            }
        }
    }
    
    fn match_with(&self, instance: &Self) -> MatchOutput<Self> {
        match (self, instance) {
            (TermSubstEntry::Type(pat_t), TermSubstEntry::Type(inst_t)) => {
                pat_t.match_with(inst_t).into()
            },
            (TermSubstEntry::Term(pat_tm), TermSubstEntry::Term(inst_tm)) => {
                pat_tm.match_with(inst_tm)
            },
            _ => MatchOutput::FailedMatch,
        }
    }
}

impl PrettyPrintable for TermSubstEntry {
    fn to_pp_element(&self, detail: bool) -> crate::utils::pretty_printer::PPElement {
        match self {
            TermSubstEntry::Type(t) => t.to_pp_element(detail),
            TermSubstEntry::Term(tm) => tm.to_pp_element(detail),
        }
    }
    
    fn requires_parens(&self, detail: bool) -> bool {
        match self {
            TermSubstEntry::Type(t) => t.requires_parens(detail),
            TermSubstEntry::Term(tm) => tm.requires_parens(detail),
        }
    }
    
    fn open_paren(&self) -> String {
        match self {
            TermSubstEntry::Type(t) => t.open_paren(),
            TermSubstEntry::Term(tm) => tm.open_paren(),
        }
    }
    
    fn close_paren(&self) -> String {
        match self {
            TermSubstEntry::Type(t) => t.close_paren(),
            TermSubstEntry::Term(tm) => tm.close_paren(),
        }
    }
}

impl From<Rc<MinlogType>> for TermSubstEntry {
    fn from(t: Rc<MinlogType>) -> Self {
        TermSubstEntry::Type(t)
    }
}

impl From<&Rc<MinlogType>> for TermSubstEntry {
    fn from(t: &Rc<MinlogType>) -> Self {
        TermSubstEntry::Type(t.clone())
    }
}

impl From<Rc<MinlogTerm>> for TermSubstEntry {
    fn from(tm: Rc<MinlogTerm>) -> Self {
        TermSubstEntry::Term(tm)
    }
}

impl From<&Rc<MinlogTerm>> for TermSubstEntry {
    fn from(tm: &Rc<MinlogTerm>) -> Self {
        TermSubstEntry::Term(tm.clone())
    }
}

pub type TermSubstitution = Substitution<TermSubstEntry>;

impl TermSubstitution {
    pub fn admissible(&self, term: &Rc<MinlogTerm>) -> bool {
        for free_var in term.get_free_variables(&mut IndexSet::new()) {
            let substituted = self.substitute(&free_var.clone().into());
            if let TermSubstEntry::Term(t) = substituted {
                if self.substitute(&free_var.minlog_type()) != t.minlog_type() {
                    return false;
                }
            } else {
                panic!("Substitution of term variable resulted in type.");
            }
        }
        
        true
    }
}

impl SubstitutableWith<TermSubstEntry> for Rc<MinlogTerm> {
    fn substitute_with(&self, from: &TermSubstEntry, to: &TermSubstEntry) -> Self {
        self.substitute(from, to)
    }
}

impl<T: SubstitutableWith<Rc<MinlogType>>> SubstitutableWith<TermSubstEntry> for T {
    fn substitute_with(&self, from: &TermSubstEntry, to: &TermSubstEntry) -> Self {
        match (from, to) {
            (TermSubstEntry::Type(from_t), TermSubstEntry::Type(to_t)) => {
                self.substitute_with(from_t, to_t)
            },
            _ => self.clone(),
        }
    }
}

impl From<MatchOutput<Rc<MinlogType>>> for MatchOutput<TermSubstEntry> {
    fn from(output: MatchOutput<Rc<MinlogType>>) -> Self {
        match output {
            MatchOutput::Substitution(from, to) => {
                MatchOutput::Substitution(from.into(), to.into())
            },
            MatchOutput::Matched(conditions) => {
                MatchOutput::Matched(conditions.into_iter().map(|(f, o)| (f.into(), o.into())).collect())
            },
            MatchOutput::FailedMatch => MatchOutput::FailedMatch,
        }
    }
}