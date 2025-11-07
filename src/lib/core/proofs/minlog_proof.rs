
use std::{rc::Rc, hash::Hash, collections::HashSet};
use crate::utils::pretty_printer::{PrettyPrintable, PPElement};
use crate::utils::proof_tree_display::{ProofTreeDisplayable, ProofTreeNode};

use crate::core::substitution::{MatchContext, MatchOutput};

use crate::core::types::minlog_type::MinlogType;
use crate::core::terms::minlog_term::MinlogTerm;
use crate::core::predicates::minlog_predicate::MinlogPredicate;

use crate::core::proofs::proof_substitution::ProofSubstEntry;

use crate::core::proofs::goal::Goal;
use crate::core::proofs::assumption::Assumption;
use crate::core::proofs::axiom::Axiom;
use crate::core::proofs::theorem::Theorem;
use crate::core::proofs::implication_intro::ImplicationIntro;
use crate::core::proofs::implication_elim::ImplicationElim;
use crate::core::proofs::universal_intro::UniversalIntro;
use crate::core::proofs::universal_elim::UniversalElim;
use crate::core::proofs::bundled_proof::BundledProof;

crate::wrapper_enum! {
    
    pub trait ProofBody: PrettyPrintable, Clone, PartialEq, Eq, Hash {
        pub fn proved_formula(&Self) -> Rc<MinlogPredicate>
        
        pub fn length(&Self) -> usize {
            1
        }
        
        pub fn normalize(&Self, eta: bool, pi: bool) -> Rc<MinlogProof>
        
        pub fn unfold(&Self) -> Rc<MinlogProof>
        
        pub fn get_type_variables(&Self, _visited: &mut HashSet<MinlogProof>) -> HashSet<Rc<MinlogType>> {
            HashSet::new()
        }
        
        pub fn get_algebra_types(&Self, _visited: &mut HashSet<MinlogProof>) -> HashSet<Rc<MinlogType>> {
            HashSet::new()
        }

        pub fn get_free_variables(&Self, _visited: &mut HashSet<MinlogProof>) -> HashSet<Rc<MinlogTerm>> {
            HashSet::new()
        }
        
        pub fn get_bound_variables(&Self, _visited: &mut HashSet<MinlogProof>) -> HashSet<Rc<MinlogTerm>> {
            HashSet::new()
        }
        
        pub fn get_predicate_variables(&Self, _visited: &mut HashSet<MinlogProof>) -> HashSet<Rc<MinlogPredicate>> {
            HashSet::new()
        }
        
        pub fn get_comprehension_terms(&Self, _visited: &mut HashSet<MinlogProof>) -> HashSet<Rc<MinlogPredicate>> {
            HashSet::new()
        }
        
        pub fn get_inductive_predicates(&Self, _visited: &mut HashSet<MinlogProof>) -> HashSet<Rc<MinlogPredicate>> {
            HashSet::new()
        }
        
        pub fn get_prime_formulas(&Self, _visited: &mut HashSet<MinlogProof>) -> HashSet<Rc<MinlogPredicate>> {
            HashSet::new()
        }
        
        pub fn get_goals(&Self, _visited: &mut HashSet<MinlogProof>) -> HashSet<Rc<MinlogProof>> {
            HashSet::new()
        }
        
        pub fn get_assumptions(&Self, _visited: &mut HashSet<MinlogProof>) -> HashSet<Rc<MinlogProof>> {
            HashSet::new()
        }
        
        pub fn get_axioms(&Self, _visited: &mut HashSet<MinlogProof>) -> HashSet<Rc<MinlogProof>> {
            HashSet::new()
        }
        
        pub fn get_theorems(&Self, _visited: &mut HashSet<MinlogProof>) -> HashSet<Rc<MinlogProof>> {
            HashSet::new()
        }
        
        pub fn substitute(&Self, from: &ProofSubstEntry, to: &ProofSubstEntry) -> Rc<MinlogProof>
        
        pub fn first_conflict_with(&Self, other: &Rc<MinlogProof>) -> Option<(ProofSubstEntry, ProofSubstEntry)>
        
        pub fn match_with(&Self, ctx: &mut impl MatchContext<ProofSubstEntry>) -> MatchOutput<ProofSubstEntry>
    }
    
    #[derive(PartialEq, Eq, Hash)]
    pub enum MinlogProof {
        Goal(||goal|| Goal),
        Assumption(||assumption|| Assumption),
        Axiom(||axiom|| Axiom),
        Theorem(||theorem|| Theorem),
        ImplicationIntro(||implication_intro|| ImplicationIntro),
        ImplicationElim(||implication_elim|| ImplicationElim),
        UniversalIntro(||universal_intro|| UniversalIntro),
        UniversalElim(||universal_elim|| UniversalElim),
        BundledProof(||bundled_proof|| BundledProof),
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
    pub fn is_closed(&self) -> bool {
        self.get_assumptions(&mut HashSet::new()).is_empty()
    }

    pub fn contains_type_variable(&self, var: &Rc<MinlogType>) -> bool {
        var.is_variable() && self.get_type_variables(&mut HashSet::new()).contains(var)
    }
    
    pub fn contains_algebra_type(&self, alg: &Rc<MinlogType>) -> bool {
        alg.is_algebra() && self.get_algebra_types(&mut HashSet::new()).contains(alg)
    }
    
    pub fn contains_free_variable(&self, var: &Rc<MinlogTerm>) -> bool {
        var.is_variable() && self.get_free_variables(&mut HashSet::new()).contains(var)
    }
    
    pub fn contains_bound_variable(&self, var: &Rc<MinlogTerm>) -> bool {
        var.is_variable() && self.get_bound_variables(&mut HashSet::new()).contains(var)
    }
    
    pub fn contains_predicate_variable(&self, pvar: &Rc<MinlogPredicate>) -> bool {
        pvar.is_variable() && self.get_predicate_variables(&mut HashSet::new()).contains(pvar)
    }
    
    pub fn contains_comprehension_term(&self, cterm: &Rc<MinlogPredicate>) -> bool {
        cterm.is_comprehension_term() && self.get_comprehension_terms(&mut HashSet::new()).contains(cterm)
    }
    
    pub fn contains_inductive_predicate(&self, ipred: &Rc<MinlogPredicate>) -> bool {
        ipred.is_inductive_predicate() && self.get_inductive_predicates(&mut HashSet::new()).contains(ipred)
    }
    
    pub fn contains_prime_formula(&self, pform: &Rc<MinlogPredicate>) -> bool {
        pform.is_prime() && self.get_prime_formulas(&mut HashSet::new()).contains(pform)
    }
    
    pub fn contains_goal(&self, goal: &Rc<MinlogProof>) -> bool {
        goal.is_goal() && self.get_goals(&mut HashSet::new()).contains(goal)
    }
    
    pub fn contains_assumption(&self, asm: &Rc<MinlogProof>) -> bool {
        asm.is_assumption() && self.get_assumptions(&mut HashSet::new()).contains(asm)
    }
    
    pub fn contains_axiom(&self, axm: &Rc<MinlogProof>) -> bool {
        axm.is_axiom() && self.get_axioms(&mut HashSet::new()).contains(axm)
    }
    
    pub fn contains_theorem(&self, thm: &Rc<MinlogProof>) -> bool {
        thm.is_theorem() && self.get_theorems(&mut HashSet::new()).contains(thm)
    }
}