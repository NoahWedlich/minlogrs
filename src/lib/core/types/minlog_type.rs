
use crate::includes::{
    essential::*,
    utils::*,
    core::{
        types::*,
    }
};

wrapper_enum::wrapper_enum! {
    pub fwd bnd trait TypeBody: PrettyPrintable + Clone + PartialEq + Eq + Hash {
        pub fwd fn remove_nulls(&self) -> Option<Rc<MinlogType>>
        
        pub fwd fn is_object_type(&self) -> bool {
            false
        }
        
        pub fwd fn arity(&self) -> usize {
            0
        }
        
        pub fwd fn level(&self) -> usize {
            0
        }
        
        pub fwd fn get_polarized_tvars(&self, _current: Polarity, _visited: &mut IndexSet<MinlogType>) -> IndexSet<Polarized<Rc<MinlogType>>> {
            IndexSet::new()
        }

        pub fwd fn get_polarized_algebras(&self, _current: Polarity, _visited: &mut IndexSet<MinlogType>) -> IndexSet<Polarized<Rc<MinlogType>>> {
            IndexSet::new()
        }
        
        pub fwd fn substitute(&self, from: &Rc<MinlogType>, to: &Rc<MinlogType>) -> Rc<MinlogType>
        
        pub fwd fn first_conflict_with(&self, other: &Rc<MinlogType>) -> Option<(Rc<MinlogType>, Rc<MinlogType>)>
        
        pub fwd fn match_with(&self, instance: &Rc<MinlogType>) -> MatchOutput<Rc<MinlogType>>
    }
    
    #[derive(PartialEq, Eq, Hash)]
    pub enum MinlogType {
        NullType(null: TypeConstant),
        Atomic(atomic: TypeConstant),
        Existential(existential: TypeConstant),
        Proposition(proposition: TypeConstant),
        Wildcard(wildcard: TypeConstant),
        Variable(variable: TypeVariable),
        Algebra(algebra: AlgebraType),
        Arrow(arrow: ArrowType),
        Tuple(tuple: TupleType),
    }
    
    ext bnd trait PrettyPrintable {
        fwd fn to_pp_element(&self, detail: bool) -> PPElement

        fwd fn requires_parens(&self, detail: bool) -> bool

        fwd fn open_paren(&self) -> String
        fwd fn close_paren(&self) -> String
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