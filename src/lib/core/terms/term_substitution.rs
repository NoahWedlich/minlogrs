
use core::panic;
use std::rc::Rc;
use crate::core::substitution::{Substitutable, Substitution, MatchContext, MatchContextImpl};
use crate::core::types::minlog_type::MinlogType;
use crate::core::terms::minlog_term::MinlogTerm;
use crate::utils::pretty_printer::PrettyPrintable;

#[derive(Clone, PartialEq, Eq)]
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
            (TermSubstEntry::Term(tm), TermSubstEntry::Term(_), TermSubstEntry::Term(_)) => {
                TermSubstEntry::Term(tm.substitute(from, to))
            },
            (TermSubstEntry::Term(tm), TermSubstEntry::Type(_), TermSubstEntry::Type(_)) => {
                TermSubstEntry::Term(tm.substitute(from, to))
            },
            _ => {
                panic!("Tried to substitute between incompatible TermSubstEntry types");
            }
        }
    }
    
    fn first_conflict_with(&self, other: &Self) -> Option<(Self, Self)> {
        match (self, other) {
            (TermSubstEntry::Type(t1), TermSubstEntry::Type(t2)) => {
                t1.first_conflict_with(t2).map(|(f, o)| (TermSubstEntry::Type(f), TermSubstEntry::Type(o)))
            },
            (TermSubstEntry::Term(tm1), TermSubstEntry::Term(tm2)) => {
                tm1.first_conflict_with(tm2).map(|(f, o)| (f, o))
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
                tm.is_variable() && !MinlogTerm::contains_free_variable(to_tm, tm)
            },
            _ => {
                false
            }
        }
    }
    
    fn match_with(&self, ctx: &mut impl MatchContext<Self>) -> Result<Option<(Self, Self)>, ()> {
        match self {
            TermSubstEntry::Type(t) => {
                t.match_with(ctx).map(|opt| {
                    opt.map(|(p, i)| (TermSubstEntry::Type(p), TermSubstEntry::Type(i)))
                })
            },
            TermSubstEntry::Term(tm) => {
                tm.match_with(ctx)
            }
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
    
    fn requires_parens(&self, _detail: bool) -> bool {
        match self {
            TermSubstEntry::Type(t) => t.requires_parens(_detail),
            TermSubstEntry::Term(tm) => tm.requires_parens(_detail),
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
pub type TermMatchContext = MatchContextImpl<TermSubstEntry>;

impl TermSubstitution {
    pub fn admissible(&self, term: &Rc<MinlogTerm>) -> bool {
        for free_var in MinlogTerm::get_free_variables(term) {
            let substituted = self.apply(&TermSubstEntry::Term(free_var.clone()));
            if let TermSubstEntry::Term(t) = substituted {
                if free_var.minlog_type() != t.minlog_type() {
                    return false;
                }
            } else {
                panic!("Substitution of term variable resulted in type.");
            }
        }
        
        true
    }
}

impl<T: MatchContext<TermSubstEntry>> MatchContext<Rc<MinlogType>> for T {
    fn new(patterns: &mut Vec<Rc<MinlogType>>, instances: &mut Vec<Rc<MinlogType>>) -> Self {
        let mut term_patterns = patterns.iter()
            .map(|p| TermSubstEntry::Type(p.clone())).collect::<Vec<_>>();
        let mut term_instances = instances.iter()
            .map(|i| TermSubstEntry::Type(i.clone())).collect::<Vec<_>>();

        <Self as MatchContext<TermSubstEntry>>::new(&mut term_patterns, &mut term_instances)
    }

    fn extend(&mut self, pattern: &Rc<MinlogType>, instance: &Rc<MinlogType>) {
        <Self as MatchContext<TermSubstEntry>>::extend(self,
            &TermSubstEntry::Type(pattern.clone()),
            &TermSubstEntry::Type(instance.clone())
        );
    }
    
    fn next_pattern(&mut self) -> Option<Rc<MinlogType>> {
        if let Some(entry) = <Self as MatchContext<TermSubstEntry>>::next_pattern(self) {
            if let TermSubstEntry::Type(t) = entry {
                return Some(t);
            } else {
                println!("Warning: Expected type pattern but found term pattern while matching.");
            }
        }
        
        None
    }

    fn next_instance(&mut self) -> Option<Rc<MinlogType>> {
        if let Some(entry) = <Self as MatchContext<TermSubstEntry>>::next_instance(self) {
            if let TermSubstEntry::Type(t) = entry {
                return Some(t);
            } else {
                println!("Warning: Expected type instance but found term instance while matching.");
            }
        }
        
        None
    }
    
    fn skip_current(&mut self) {
        <Self as MatchContext<TermSubstEntry>>::skip_current(self);
    }
    
    fn restrict(&mut self, element: &Rc<MinlogType>) {
        <Self as MatchContext<TermSubstEntry>>::restrict(self, &TermSubstEntry::Type(element.clone()));
    }

    fn is_restricted(&self, element: &Rc<MinlogType>) -> bool {
        <Self as MatchContext<TermSubstEntry>>::is_restricted(self, &TermSubstEntry::Type(element.clone()))
    }

    fn substitute(&mut self, from: &Rc<MinlogType>, to: &Rc<MinlogType>) {
        <Self as MatchContext<TermSubstEntry>>::substitute(self,
            &TermSubstEntry::Type(from.clone()),
            &TermSubstEntry::Type(to.clone())
        );
    }
}