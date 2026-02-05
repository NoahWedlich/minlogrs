
use crate::includes::{
    essential::*,
    utils::*,
    kernel::{
        types::*,
    }
};

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum TypeConstant {
    NullType,
    UnitType,
    Wildcard,
}

impl TypeConstant {
    pub fn create_null() -> Arc<MinlogType> {
        Arc::new(MinlogType::NullType(TypeConstant::NullType))
    }
    
    pub fn create_unit() -> Arc<MinlogType> {
        Arc::new(MinlogType::UnitType(TypeConstant::UnitType))
    }
    
    pub fn create_wildcard() -> Arc<MinlogType> {
        Arc::new(MinlogType::Wildcard(TypeConstant::Wildcard))
    }
}

impl TypeBody for TypeConstant {
    fn remove_nulls(&self) -> Option<Arc<MinlogType>> {
        match self {
            TypeConstant::NullType => None,
            TypeConstant::UnitType => Some(TypeConstant::create_unit()),
            TypeConstant::Wildcard => Some(TypeConstant::create_wildcard()),
        }
    }
    
    fn substitute(&self, from: &Arc<MinlogType>, to: &Arc<MinlogType>) -> Arc<MinlogType> {
        if (from.is_null() && matches!(self, TypeConstant::NullType)) ||
              (from.is_unit() && matches!(self, TypeConstant::UnitType)) ||
              (from.is_wildcard() && matches!(self, TypeConstant::Wildcard)) {
            to.clone()
        } else {
            match self {
                TypeConstant::NullType => TypeConstant::create_null(),
                TypeConstant::UnitType => TypeConstant::create_unit(),
                TypeConstant::Wildcard => TypeConstant::create_wildcard(),
            }
        }
    }
    
    fn first_conflict_with(&self, other: &Arc<MinlogType>) -> Option<(Arc<MinlogType>, Arc<MinlogType>)> {
        match self {
            TypeConstant::NullType => if other.is_null() {
                None
            } else {
                Some((TypeConstant::create_null(), other.clone()))
            },
            TypeConstant::UnitType => if other.is_unit() {
                None
            } else {
                Some((TypeConstant::create_unit(), other.clone()))
            },
            TypeConstant::Wildcard => None,
        }
    }
    
    fn match_with(&self, instance: &Arc<MinlogType>) -> MatchOutput<Arc<MinlogType>> {
        match (self, instance) {
            (TypeConstant::Wildcard, _) => {
                MatchOutput::Matched(IndexMap::new())
            },
            (TypeConstant::NullType, t) if t.is_null() => {
                MatchOutput::Matched(IndexMap::new())
            },
            (TypeConstant::UnitType, t) if t.is_unit() => {
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
            TypeConstant::UnitType => PPElement::text("unit".to_string()),
            TypeConstant::Wildcard => PPElement::text("_".to_string()),
        }
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}