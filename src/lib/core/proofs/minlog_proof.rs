
use std::{rc::Rc, hash::Hash, collections::HashSet};
use crate::utils::pretty_printer::{PrettyPrintable, PPElement};
use crate::utils::proof_tree_display::{ProofTreeDisplayable, ProofTreeNode};

use crate::core::types::minlog_type::MinlogType;
use crate::core::terms::minlog_term::MinlogTerm;
use crate::core::formulas::minlog_formula::MinlogFormula;
use crate::core::predicates::minlog_predicate::MinlogPredicate;

use crate::core::proofs::assumption::Assumption;

crate::wrapper_enum! {
    
    @default { EmptyProofBody }
    pub trait ProofBody: PrettyPrintable, Clone, PartialEq, Eq, Hash {
        pub fn proved_formula(&Self) -> Rc<MinlogFormula>
        
        pub fn length(&Self) -> usize {
            1
        }
        
        pub fn normalize(&Self, eta: bool, pi: bool) -> Rc<MinlogProof>
        
        pub fn get_type_variables(&Self) -> HashSet<Rc<MinlogType>> {
            HashSet::new()
        }
        
        pub fn get_algebra_types(&Self) -> HashSet<Rc<MinlogType>> {
            HashSet::new()
        }

        pub fn get_free_variables(&Self) -> HashSet<Rc<MinlogTerm>> {
            HashSet::new()
        }
        
        pub fn get_bound_variables(&Self) -> HashSet<Rc<MinlogTerm>> {
            HashSet::new()
        }
        
        pub fn get_predicate_variables(&Self) -> HashSet<Rc<MinlogPredicate>> {
            HashSet::new()
        }
        
        pub fn get_comprehension_terms(&Self) -> HashSet<Rc<MinlogPredicate>> {
            HashSet::new()
        }
        
        pub fn get_inductive_predicates(&Self) -> HashSet<Rc<MinlogPredicate>> {
            HashSet::new()
        }
        
        pub fn get_prime_formulas(&Self) -> HashSet<Rc<MinlogFormula>> {
            HashSet::new()
        }
        
        pub fn get_assumptions(&Self) -> HashSet<Rc<MinlogProof>> {
            HashSet::new()
        }
        
        pub fn get_axioms(&Self) -> HashSet<Rc<MinlogProof>> {
            HashSet::new()
        }
        
        pub fn get_theorems(&Self) -> HashSet<Rc<MinlogProof>> {
            HashSet::new()
        }
    }
    
    #[derive(PartialEq, Eq, Hash)]
    pub enum MinlogProof {
        Assumption(||assumption|| Assumption),
        Axiom(||axiom||),
        Theorem(||theorem||),
        ImplicationIntroduction(||implication_introduction||),
        ImplicationElimination(||implication_elimination||),
        ConjunctionIntroduction(||conjunction_introduction||),
        ConjunctionElimination(||conjunction_elimination||),
        UniversalIntroduction(||universal_introduction||),
        UniversalElimination(||universal_elimination||),
        CasesDistinction(||cases_distinction||),
    }
    
    impl PrettyPrintable {
        fn to_pp_element(&Self, detail: bool) -> PPElement;

        fn requires_parens(&Self, detail: bool) -> bool;

        fn open_paren(&Self) -> String;

        fn close_paren(&Self) -> String;
    }
    
    impl ProofTreeDisplayable {
        fn to_proof_tree_node(&Self) -> ProofTreeNode;
    }
}

impl MinlogProof {
    
}

#[derive(PartialEq, Eq, Clone, Hash)]
pub struct EmptyProofBody;

impl PrettyPrintable for EmptyProofBody {
    fn to_pp_element(&self, _detail: bool) -> PPElement {
        PPElement::text("<?>".to_string())
    }
}

impl ProofTreeDisplayable for EmptyProofBody {
    fn to_proof_tree_node(&self) -> ProofTreeNode {
        ProofTreeNode::new_leaf("<?>".to_string())
    }
}

impl ProofBody for EmptyProofBody {
    fn proved_formula(&self) -> Rc<MinlogFormula> {
        unimplemented!()
    }
    
    fn normalize(&self, _eta: bool, _pi: bool) -> Rc<MinlogProof> {
        unimplemented!()
    }
}