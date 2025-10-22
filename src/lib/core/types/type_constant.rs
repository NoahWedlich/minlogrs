
use std::rc::Rc;
use crate::utils::pretty_printer::{PrettyPrintable, PPElement};

use crate::core::substitution::{MatchContext, MatchOutput};

use crate::core::types::minlog_type::{TypeBody, MinlogType};

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum TypeConstant {
    NullType,
    Atomic,
    Existential,
    Proposition,
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
}

impl TypeBody for TypeConstant {
    fn remove_nulls(&self) -> Option<Rc<MinlogType>> {
        match self {
            TypeConstant::NullType => None,
            TypeConstant::Atomic => Some(TypeConstant::create_atomic()),
            TypeConstant::Existential => Some(TypeConstant::create_existential()),
            TypeConstant::Proposition => Some(TypeConstant::create_proposition()),
        }
    }
    
    fn substitute(&self, _from: &Rc<MinlogType>, _to: &Rc<MinlogType>) -> Rc<MinlogType> {
        match self {
            TypeConstant::NullType => TypeConstant::create_null(),
            TypeConstant::Atomic => TypeConstant::create_atomic(),
            TypeConstant::Existential => TypeConstant::create_existential(),
            TypeConstant::Proposition => TypeConstant::create_proposition(),
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
        }
    }
    
    fn match_with(&self, ctx: &mut impl MatchContext<Rc<MinlogType>>) -> MatchOutput<Rc<MinlogType>> {
        let pattern = ctx.next_pattern().unwrap();
        let instance = ctx.next_instance().unwrap();
        
        if pattern == instance {
            MatchOutput::Matched
        } else {
            MatchOutput::FailedMatch
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
        }
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}