
pub mod essential {
    pub use std::{
        sync::{Arc, RwLock},
        cmp::{min, max},
        hash::{Hash, Hasher},
        any::Any,
    };
    
    pub use indexmap::{IndexMap, IndexSet};
}

pub mod utils {
    pub use crate::utils::{
        pretty_printer::*,
        proof_tree_display::*,
    };
}

pub mod kernel {
    pub use crate::kernel::{
        substitution::*,
        polarity::*,
    };
    
    pub mod structures {
        pub use super::*;
        
        pub use crate::kernel::structures::{
            algebra::*,
            program_constant::*,
            inductive_constant::*,
        };
    }
    
    pub mod types {
        pub use super::*;
        
        pub use crate::kernel::types::{
            minlog_type::*,
            type_constant::*,
            type_variable::*,
            algebra_type::*,
            arrow_type::*,
            pair_type::*,
            type_substitution::*,
        };
    }
    
    pub mod terms {
        pub use super::*;
        
        pub use crate::kernel::terms::{
            minlog_term::*,
            term_wildcard::*,
            term_variable::*,
            constructor::*,
            program_term::*,
            abstraction::*,
            application::*,
            pair::*,
            projection::*,
            match_term::*,
            term_substitution::*,
        };
    }
    
    pub mod predicates {
        pub use super::*;
        
        pub use crate::kernel::predicates::{
            minlog_predicate::*,
            predicate_wildcard::*,
            predicate_variable::*,
            comprehension_term::*,
            inductive_predicate::*,
            prime_formula::*,
            implication::*,
            all_quantifier::*,
            predicate_substitution::*,
        };
    }
    
    pub mod proofs {
        pub use super::*;
        
        pub use crate::kernel::proofs::{
            minlog_proof::*,
            proof_wildcard::*,
            goal::*,
            assumption::*,
            axiom::*,
            theorem::*,
            implication_intro::*,
            implication_elim::*,
            universal_intro::*,
            universal_elim::*,
            bundled_proof::*,
            proof_substitution::*,
            proof_context::*,
        };
    }
    
    pub mod all {
        pub use super::*;
        
        pub use super::types::*;
        pub use super::terms::*;
        pub use super::predicates::*;
        pub use super::proofs::*;
    }
}

pub mod runtime {
    pub mod semantics {
        pub use crate::runtime::semantics::{
            elimination::*,
            totality::*,
        };
    }
    
    pub mod proof_construction {
        pub use crate::runtime::proof_construction::{
            by_assume::*,
            by_intro::*,
            by_elim::*,
            by_use::*,
        };
    }
    
    pub mod all {
        pub use super::semantics::*;
        pub use super::proof_construction::*;
    }
}


pub mod all {
    pub use super::essential::*;
    pub use super::kernel::all::*;
    pub use super::runtime::all::*;
    pub use super::utils::*;
}