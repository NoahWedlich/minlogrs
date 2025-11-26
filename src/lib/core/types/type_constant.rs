
use std::rc::Rc;
use indexmap::IndexMap;

use crate::utils::pretty_printer::{PrettyPrintable, PPElement};

use crate::core::substitution::MatchOutput;

use crate::core::types::minlog_type::{TypeBody, MinlogType};

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum TypeConstant {
    NullType,
    Atomic,
    Existential,
    Proposition,
    Wildcard,
}

impl TypeConstant {
    pub fn create_null() -> Rc<MinlogType> {
        Rc::new(MinlogType::NullType(TypeConstant::NullType))
    }
    
    pub fn create_atomic() -> Rc<MinlogType> {
        Rc::new(MinlogType::Atomic(TypeConstant::Atomic))
    }
    
    pub fn create_existential() -> Rc<MinlogType> {
        Rc::new(MinlogType::Existential(TypeConstant::Existential))
    }
    
    pub fn create_proposition() -> Rc<MinlogType> {
        Rc::new(MinlogType::Proposition(TypeConstant::Proposition))
    }
    
    pub fn create_wildcard() -> Rc<MinlogType> {
        Rc::new(MinlogType::Wildcard(TypeConstant::Wildcard))
    }
}

impl TypeBody for TypeConstant {
    fn remove_nulls(&self) -> Option<Rc<MinlogType>> {
        match self {
            TypeConstant::NullType => None,
            TypeConstant::Atomic => Some(TypeConstant::create_atomic()),
            TypeConstant::Existential => Some(TypeConstant::create_existential()),
            TypeConstant::Proposition => Some(TypeConstant::create_proposition()),
            TypeConstant::Wildcard => Some(TypeConstant::create_wildcard()),
        }
    }
    
    fn substitute(&self, from: &Rc<MinlogType>, to: &Rc<MinlogType>) -> Rc<MinlogType> {
        if (from.is_null() && matches!(self, TypeConstant::NullType)) ||
              (from.is_atomic() && matches!(self, TypeConstant::Atomic)) ||
              (from.is_existential() && matches!(self, TypeConstant::Existential)) ||
              (from.is_proposition() && matches!(self, TypeConstant::Proposition)) {
            to.clone()
        } else {
            match self {
                TypeConstant::NullType => TypeConstant::create_null(),
                TypeConstant::Atomic => TypeConstant::create_atomic(),
                TypeConstant::Existential => TypeConstant::create_existential(),
                TypeConstant::Proposition => TypeConstant::create_proposition(),
                TypeConstant::Wildcard => TypeConstant::create_wildcard(),
            }
        }
    }
    
    fn first_conflict_with(&self, other: &Rc<MinlogType>) -> Option<(Rc<MinlogType>, Rc<MinlogType>)> {
        match self {
            TypeConstant::NullType => if other.is_null() {
                None
            } else {
                Some((TypeConstant::create_null(), other.clone()))
            },
            TypeConstant::Atomic => if other.is_atomic() {
                None
            } else {
                Some((TypeConstant::create_atomic(), other.clone()))
            },
            TypeConstant::Existential => if other.is_existential() {
                None
            } else {
                Some((TypeConstant::create_existential(), other.clone()))
            },
            TypeConstant::Proposition => if other.is_proposition() {
                None
            } else {
                Some((TypeConstant::create_proposition(), other.clone()))
            },
            TypeConstant::Wildcard => None,
        }
    }
    
    fn match_with(&self, instance: &Rc<MinlogType>) -> MatchOutput<Rc<MinlogType>> {
        match (self, instance) {
            (TypeConstant::Wildcard, _) => {
                MatchOutput::Matched(IndexMap::new())
            },
            (TypeConstant::NullType, t) if t.is_null() => {
                MatchOutput::Matched(IndexMap::new())
            },
            (TypeConstant::Atomic, t) if t.is_atomic() => {
                MatchOutput::Matched(IndexMap::new())
            },
            (TypeConstant::Existential, t) if t.is_existential() => {
                MatchOutput::Matched(IndexMap::new())
            },
            (TypeConstant::Proposition, t) if t.is_proposition() => {
                MatchOutput::Matched(IndexMap::new())
            },
            _ => MatchOutput::FailedMatch,
        }
    }
}

impl PrettyPrintable for TypeConstant {
    fn to_pp_element(&self, _detail: bool) -> PPElement {
        match self {
            TypeConstant::NullType => PPElement::text("null".to_string()),
            TypeConstant::Atomic => PPElement::text("atomic".to_string()),
            TypeConstant::Existential => PPElement::text("existential".to_string()),
            TypeConstant::Proposition => PPElement::text("proposition".to_string()),
            TypeConstant::Wildcard => PPElement::text("_".to_string()),
        }
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}