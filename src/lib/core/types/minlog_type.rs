
use crate::includes::{
    essential::*,
    utils::*,
    core::{
        types::*,
    }
};

wrapper_enum::wrapper_enum! {
    pub fwd bnd trait TypeBody: PrettyPrintable + Clone + PartialEq + Eq + Hash + Sync + Send {
        pub fwd fn remove_nulls(&self) -> Option<Arc<MinlogType>>
        
        pub fwd fn is_object_type(&self) -> bool {
            false
        }
        
        pub fwd fn arity(&self) -> usize {
            0
        }
        
        pub fwd fn level(&self) -> usize {
            0
        }
        
        pub fwd fn get_polarized_tvars(&self, _current: Polarity, _visited: &mut IndexSet<MinlogType>) -> IndexSet<Polarized<Arc<MinlogType>>> {
            IndexSet::new()
        }

        pub fwd fn get_polarized_algebras(&self, _current: Polarity, _visited: &mut IndexSet<MinlogType>) -> IndexSet<Polarized<Arc<MinlogType>>> {
            IndexSet::new()
        }
        
        pub fwd fn substitute(&self, from: &Arc<MinlogType>, to: &Arc<MinlogType>) -> Arc<MinlogType>
        
        pub fwd fn first_conflict_with(&self, other: &Arc<MinlogType>) -> Option<(Arc<MinlogType>, Arc<MinlogType>)>
        
        pub fwd fn match_with(&self, instance: &Arc<MinlogType>) -> MatchOutput<Arc<MinlogType>>
    }
    
    #[derive(PartialEq, Eq, Hash)]
    pub enum MinlogType {
        NullType(null: TypeConstant),
        UnitType(unit: TypeConstant),
        Wildcard(wildcard: TypeConstant),
        Variable(variable: TypeVariable),
        Algebra(algebra: AlgebraType),
        Arrow(arrow: ArrowType),
        Pair(pair: PairType),
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
            | MinlogType::UnitType(_)
            | MinlogType::Wildcard(_))
    }
    
    pub fn is_ground_type(&self) -> bool {
        self.is_constant() || matches!(self, MinlogType::Variable(_) | MinlogType::Algebra(_))
    }
    
    pub fn get_type_variables(&self, visited: &mut IndexSet<MinlogType>) -> IndexSet<Arc<MinlogType>> {
        self.get_polarized_tvars(Polarity::Unknown, visited)
            .into_iter().map(|p| p.value).collect()
    }
    
    pub fn get_algebra_types(&self, visited: &mut IndexSet<MinlogType>) -> IndexSet<Arc<MinlogType>> {
        self.get_polarized_algebras(Polarity::Unknown, visited)
            .into_iter().map(|p| p.value).collect()
    }
    
    pub fn contains_type_variable(&self, var: &Arc<MinlogType>) -> bool {
        var.is_variable() && self.get_type_variables(&mut IndexSet::new()).contains(var)
    }
    
    pub fn contains_algebra_type(&self, alg: &Arc<MinlogType>) -> bool {
        alg.is_algebra() && self.get_algebra_types(&mut IndexSet::new()).contains(alg)
    }
}