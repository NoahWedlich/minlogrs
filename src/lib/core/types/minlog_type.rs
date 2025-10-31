
use std::{rc::Rc, hash::Hash, collections::HashSet};

use crate::utils::pretty_printer::{PrettyPrintable, PPElement};

use crate::core::substitution::{MatchContext, MatchOutput};
use crate::core::polarity::{Polarity, Polarized};

use crate::core::types::type_constant::TypeConstant;
use crate::core::types::type_variable::TypeVariable;
use crate::core::types::algebra_type::AlgebraType;
use crate::core::types::arrow_type::ArrowType;
use crate::core::types::tuple_type::TupleType;

crate::wrapper_enum! {
    
    @default { TypeConstant }
    pub trait TypeBody: PrettyPrintable, Clone, PartialEq, Eq, Hash {
        pub fn remove_nulls(&Self) -> Option<Rc<MinlogType>>
        
        pub fn is_object_type(&Self) -> bool {
            false
        }
        
        pub fn arity(&Self) -> usize {
            0
        }
        
        pub fn level(&Self) -> usize {
            0
        }
        
        pub fn get_polarized_tvars(&Self, _current: Polarity) -> HashSet<Polarized<Rc<MinlogType>>> {
            HashSet::new()
        }

        pub fn get_polarized_algebras(&Self, _current: Polarity) -> HashSet<Polarized<Rc<MinlogType>>> {
            HashSet::new()
        }
        
        pub fn substitute(&Self, from: &Rc<MinlogType>, to: &Rc<MinlogType>) -> Rc<MinlogType>
        
        pub fn first_conflict_with(&Self, other: &Rc<MinlogType>) -> Option<(Rc<MinlogType>, Rc<MinlogType>)>
        
        pub fn match_with(&Self, ctx: &mut impl MatchContext<Rc<MinlogType>>) -> MatchOutput<Rc<MinlogType>>
    }
    
    #[derive(PartialEq, Eq, Hash)]
    pub enum MinlogType {
        NullType(|null|),
        Atomic(|atomic|),
        Existential(|existential|),
        Proposition(|proposition|),
        Variable(||variable|| TypeVariable),
        Algebra(||algebra|| AlgebraType),
        Arrow(||arrow|| ArrowType),
        Tuple(||tuple|| TupleType),
    }
    
    impl PrettyPrintable {
        fn to_pp_element(&Self, detail: bool) -> PPElement;

        fn requires_parens(&Self, detail: bool) -> bool;

        fn open_paren(&Self) -> String;

        fn close_paren(&Self) -> String;
    }
    
}

impl MinlogType {
    pub fn is_constant(&self) -> bool {
        matches!(self, MinlogType::NullType(_)
            | MinlogType::Atomic(_)
            | MinlogType::Existential(_)
            | MinlogType::Proposition(_))
    }
    
    pub fn is_ground_type(&self) -> bool {
        self.is_constant() || matches!(self, MinlogType::Variable(_) | MinlogType::Algebra(_))
    }
    
    pub fn get_type_variables(&self) -> HashSet<Rc<MinlogType>> {
        self.get_polarized_tvars(Polarity::Unknown)
            .into_iter().map(|p| p.value).collect()
    }
    
    pub fn get_algebra_types(&self) -> HashSet<Rc<MinlogType>> {
        self.get_polarized_algebras(Polarity::Unknown)
            .into_iter().map(|p| p.value).collect()
    }
    
    pub fn contains_type_variable(&self, var: &Rc<MinlogType>) -> bool {
        var.is_variable() && self.get_type_variables().contains(var)
    }
    
    pub fn contains_algebra_type(&self, alg: &Rc<MinlogType>) -> bool {
        alg.is_algebra() && self.get_algebra_types().contains(alg)
    }
}