
use std::rc::Rc;

use crate::utils::pretty_printer::{PrettyPrintable, PPElement};

use crate::core::substitution::{MatchContext, MatchOutput};

use crate::core::types::type_constant::TypeConstant;
use crate::core::types::type_variable::TypeVariable;
use crate::core::types::algebra_type::AlgebraType;
use crate::core::types::arrow_type::ArrowType;
use crate::core::types::star_type::StarType;

crate::wrapper_enum! {
    
    @default { TypeConstant }
    pub trait TypeBody: PrettyPrintable, Clone, PartialEq, Eq {
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
        
        pub fn get_type_variables(&Self) -> Vec<Rc<MinlogType>> {
            vec![]
        }
        
        pub fn get_algebra_types(&Self) -> Vec<Rc<MinlogType>> {
            vec![]
        }
        
        pub fn substitute(&Self, from: &Rc<MinlogType>, to: &Rc<MinlogType>) -> Rc<MinlogType>
        
        pub fn first_conflict_with(&Self, other: &Rc<MinlogType>) -> Option<(Rc<MinlogType>, Rc<MinlogType>)>
        
        pub fn match_with(&Self, ctx: &mut impl MatchContext<Rc<MinlogType>>) -> MatchOutput<Rc<MinlogType>>
    }
    
    #[derive(PartialEq, Eq)]
    pub enum MinlogType {
        NullType(|null|),
        Atomic(|atomic|),
        Existential(|existential|),
        Proposition(|proposition|),
        Variable(||variable|| TypeVariable),
        Algebra(||algebra|| AlgebraType),
        Arrow(||arrow|| ArrowType),
        Star(||star|| StarType),
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
    
    pub fn contains_type_variable(minlog_type: &Rc<MinlogType>, var: &Rc<MinlogType>) -> bool {
        var.is_variable() && minlog_type.get_type_variables().contains(var)
    }
    
    pub fn contains_algebra_type(minlog_type: &Rc<MinlogType>, alg: &Rc<MinlogType>) -> bool {
        alg.is_algebra() && minlog_type.get_algebra_types().contains(alg)
    }
}