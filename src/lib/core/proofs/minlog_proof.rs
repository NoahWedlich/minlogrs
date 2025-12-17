
use crate::includes::{
    essential::*,
    utils::*,
    core::{
        types::*,
        terms::*,
        predicates::*,
        proofs::*,
    }
};

wrapper_enum::wrapper_enum! {
    
    pub fwd bnd trait ProofBody: PrettyPrintable + Clone + PartialEq + Eq + Hash {
        pub fwd fn proved_formula(&self) -> Rc<MinlogPredicate>
        
        pub fwd fn length(&self) -> usize {
            1
        }
        
        pub fwd fn normalize(&self, eta: bool, pi: bool) -> Rc<MinlogProof>
        
        pub fwd fn unfold(&self) -> Rc<MinlogProof>
        
        pub fwd fn extracted_term(&self) -> Option<MinlogTerm>
        
        pub fwd fn get_type_variables(&self) -> IndexSet<Rc<MinlogType>> {
            IndexSet::new()
        }
        
        pub fwd fn get_algebra_types(&self) -> IndexSet<Rc<MinlogType>> {
            IndexSet::new()
        }

        pub fwd fn get_free_variables(&self) -> IndexSet<MinlogTerm> {
            IndexSet::new()
        }
        
        pub fwd fn get_bound_variables(&self) -> IndexSet<MinlogTerm> {
            IndexSet::new()
        }
        
        pub fwd fn get_predicate_variables(&self) -> IndexSet<Rc<MinlogPredicate>> {
            IndexSet::new()
        }
        
        pub fwd fn get_comprehension_terms(&self) -> IndexSet<Rc<MinlogPredicate>> {
            IndexSet::new()
        }
        
        pub fwd fn get_inductive_predicates(&self) -> IndexSet<Rc<MinlogPredicate>> {
            IndexSet::new()
        }
        
        pub fwd fn get_prime_formulas(&self) -> IndexSet<Rc<MinlogPredicate>> {
            IndexSet::new()
        }
        
        pub fwd fn get_goals(&self) -> IndexSet<Rc<MinlogProof>> {
            IndexSet::new()
        }
        
        pub fwd fn get_assumptions(&self) -> IndexSet<Rc<MinlogProof>> {
            IndexSet::new()
        }
        
        pub fwd fn get_axioms(&self) -> IndexSet<Rc<MinlogProof>> {
            IndexSet::new()
        }
        
        pub fwd fn get_theorems(&self) -> IndexSet<Rc<MinlogProof>> {
            IndexSet::new()
        }
        
        pub fwd fn substitute(&self, from: &ProofSubstEntry, to: &ProofSubstEntry) -> Rc<MinlogProof>
        
        pub fwd fn first_conflict_with(&self, other: &Rc<MinlogProof>) -> Option<(ProofSubstEntry, ProofSubstEntry)>
        
        pub fwd fn match_with(&self, instance: &Rc<MinlogProof>) -> MatchOutput<ProofSubstEntry>
    }
    
    #[derive(PartialEq, Eq, Hash)]
    pub enum MinlogProof {
        Wildcard(wildcard: ProofWildcard),
        Goal(goal: Goal),
        Assumption(assumption: Assumption),
        Axiom(axiom: Axiom),
        Theorem(theorem: Theorem),
        ImplicationIntro(implication_intro: ImplicationIntro),
        ImplicationElim(implication_elim: ImplicationElim),
        UniversalIntro(universal_intro: UniversalIntro),
        UniversalElim(universal_elim: UniversalElim),
        BundledProof(bundled_proof: BundledProof),
    }
    
    ext bnd trait PrettyPrintable {
        fwd fn to_pp_element(&self, detail: bool) -> PPElement

        fwd fn requires_parens(&self, detail: bool) -> bool

        fwd fn open_paren(&self) -> String

        fwd fn close_paren(&self) -> String
    }
    
    ext bnd trait ProofTreeDisplayable {
        fwd fn to_proof_tree_node(&self) -> ProofTreeNode
    }
}

impl MinlogProof {
    pub fn is_closed(&self) -> bool {
        self.get_assumptions().is_empty()
    }

    pub fn contains_type_variable(&self, var: &Rc<MinlogType>) -> bool {
        var.is_variable() && self.get_type_variables().contains(var)
    }
    
    pub fn contains_algebra_type(&self, alg: &Rc<MinlogType>) -> bool {
        alg.is_algebra() && self.get_algebra_types().contains(alg)
    }
    
    pub fn contains_free_variable(&self, var: &MinlogTerm) -> bool {
        var.is_variable() && self.get_free_variables().contains(var)
    }
    
    pub fn contains_bound_variable(&self, var: &MinlogTerm) -> bool {
        var.is_variable() && self.get_bound_variables().contains(var)
    }
    
    pub fn contains_predicate_variable(&self, pvar: &Rc<MinlogPredicate>) -> bool {
        pvar.is_variable() && self.get_predicate_variables().contains(pvar)
    }
    
    pub fn contains_comprehension_term(&self, cterm: &Rc<MinlogPredicate>) -> bool {
        cterm.is_comprehension_term() && self.get_comprehension_terms().contains(cterm)
    }
    
    pub fn contains_inductive_predicate(&self, ipred: &Rc<MinlogPredicate>) -> bool {
        ipred.is_inductive_predicate() && self.get_inductive_predicates().contains(ipred)
    }
    
    pub fn contains_prime_formula(&self, pform: &Rc<MinlogPredicate>) -> bool {
        pform.is_prime() && self.get_prime_formulas().contains(pform)
    }
    
    pub fn contains_goal(&self, goal: &Rc<MinlogProof>) -> bool {
        goal.is_goal() && self.get_goals().contains(goal)
    }
    
    pub fn contains_assumption(&self, asm: &Rc<MinlogProof>) -> bool {
        asm.is_assumption() && self.get_assumptions().contains(asm)
    }
    
    pub fn contains_axiom(&self, axm: &Rc<MinlogProof>) -> bool {
        axm.is_axiom() && self.get_axioms().contains(axm)
    }
    
    pub fn contains_theorem(&self, thm: &Rc<MinlogProof>) -> bool {
        thm.is_theorem() && self.get_theorems().contains(thm)
    }
}