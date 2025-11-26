
use crate::includes::{
    essential::*,
    utils::*,
    core::{
        types::*,
    }
};

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
        
        pub fn get_polarized_tvars(&Self, _current: Polarity, _visited: &mut IndexSet<MinlogType>) -> IndexSet<Polarized<Rc<MinlogType>>> {
            IndexSet::new()
        }

        pub fn get_polarized_algebras(&Self, _current: Polarity, _visited: &mut IndexSet<MinlogType>) -> IndexSet<Polarized<Rc<MinlogType>>> {
            IndexSet::new()
        }
        
        pub fn substitute(&Self, from: &Rc<MinlogType>, to: &Rc<MinlogType>) -> Rc<MinlogType>
        
        pub fn first_conflict_with(&Self, other: &Rc<MinlogType>) -> Option<(Rc<MinlogType>, Rc<MinlogType>)>
        
        pub fn match_with(&Self, instance: &Rc<MinlogType>) -> MatchOutput<Rc<MinlogType>>
    }
    
    #[derive(PartialEq, Eq, Hash)]
    pub enum MinlogType {
        NullType(|null|),
        Atomic(|atomic|),
        Existential(|existential|),
        Proposition(|proposition|),
        Wildcard(|wildcard|),
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
    
    pub fn is_unit(&self) -> bool {
        if let MinlogType::Tuple(tup) = self {
            tup.types().is_empty()
        } else {
            false
        }
    }
    
    pub fn is_ground_type(&self) -> bool {
        self.is_constant() || matches!(self, MinlogType::Variable(_) | MinlogType::Algebra(_))
    }
    
    pub fn get_type_variables(&self, visited: &mut IndexSet<MinlogType>) -> IndexSet<Rc<MinlogType>> {
        self.get_polarized_tvars(Polarity::Unknown, visited)
            .into_iter().map(|p| p.value).collect()
    }
    
    pub fn get_algebra_types(&self, visited: &mut IndexSet<MinlogType>) -> IndexSet<Rc<MinlogType>> {
        self.get_polarized_algebras(Polarity::Unknown, visited)
            .into_iter().map(|p| p.value).collect()
    }
    
    pub fn contains_type_variable(&self, var: &Rc<MinlogType>) -> bool {
        var.is_variable() && self.get_type_variables(&mut IndexSet::new()).contains(var)
    }
    
    pub fn contains_algebra_type(&self, alg: &Rc<MinlogType>) -> bool {
        alg.is_algebra() && self.get_algebra_types(&mut IndexSet::new()).contains(alg)
    }
}