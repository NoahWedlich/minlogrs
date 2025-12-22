
pub mod essential {
    pub use std::{
        rc::Rc,
        cmp::{min, max},
        hash::{Hash, Hasher},
        fmt::Debug,
        cell::RefCell,
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

pub mod core {
    pub use crate::core::{
        substitution::*,
        polarity::*,
    };
    
    pub mod structures {
        pub use super::*;
        
        pub use crate::core::structures::{
            algebra::*,
            program_constant::*,
            inductive_constant::*,
        };
    }
    
    pub mod types {
        pub use super::*;
        
        pub use crate::core::types::{
            minlog_type::*,
            type_constant::*,
            type_variable::*,
            algebra_type::*,
            arrow_type::*,
            tuple_type::*,
            type_substitution::*,
        };
    }
    
    pub mod terms {
        pub use super::*;
        
        pub use crate::core::terms::{
            minlog_term::*,
            term_wildcard::*,
            term_variable::*,
            constructor::*,
            program_term::*,
            abstraction::*,
            application::*,
            tuple::*,
            projection::*,
            match_term::*,
            term_substitution::*,
        };
    }
    
    pub mod predicates {
        pub use super::*;
        
        pub use crate::core::predicates::{
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
        
        pub use crate::core::proofs::{
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

pub mod proof_generation {
    pub use crate::proof_generation::{
        by_assume::*,
        by_intro::*,
        by_elim::*,
        by_use::*,
    };
}

pub mod builtin {
    pub use crate::builtin::{
        elimination::*,
        totality::*,
    };
}

pub mod frontend {
    pub mod source_management {
        pub use crate::frontend::source_management::{
            source_provider::*,
            file_source::*,
        };
    }
    
    pub mod lexing {
        pub use crate::frontend::lexing::{
            token::*,
            source_span::*,
            lexer::*,
        };
    }
    
    pub mod parsing {
        pub use crate::frontend::parsing::{
            token_term::*,
            command::*,
            parser::*,
        };
    }
    
    pub mod all {
        pub use super::source_management::*;
        pub use super::lexing::*;
        pub use super::parsing::*;
    }
}

pub mod all {
    pub use super::essential::*;
    pub use super::core::all::*;
    pub use super::proof_generation::*;
    pub use super::builtin::*;
}