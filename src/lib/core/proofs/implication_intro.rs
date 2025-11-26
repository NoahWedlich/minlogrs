
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

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ImplicationIntro {
    assumption: Rc<MinlogProof>,
    conclusion: Rc<MinlogProof>,
    formula: Rc<MinlogPredicate>,
}

impl ImplicationIntro {
    pub fn create(proof: Rc<MinlogProof>, assumption: Rc<MinlogProof>) -> Rc<MinlogProof> {
        if !assumption.is_assumption() {
            panic!("ImplicationIntro::create called with a non-assumption proof as assumption");
        }
        
        let premise_formula = assumption.proved_formula();
        let conclusion_formula = proof.proved_formula();
        let implication_formula = Implication::create(vec![premise_formula], conclusion_formula);
        
        Rc::new(MinlogProof::ImplicationIntro(ImplicationIntro {
            assumption,
            conclusion: proof,
            formula: implication_formula,
        }))
    }

    pub fn assumption(&self) -> Rc<MinlogProof> {
        self.assumption.clone()
    }
    
    pub fn conclusion(&self) -> Rc<MinlogProof> {
        self.conclusion.clone()
    }
}

impl ProofBody for ImplicationIntro {
    fn proved_formula(&self) -> Rc<MinlogPredicate> {
        self.formula.clone()
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> Rc<MinlogProof> {
        Rc::new(MinlogProof::ImplicationIntro(ImplicationIntro {
            assumption: self.assumption.normalize(eta, pi),
            conclusion: self.conclusion.normalize(eta, pi),
            formula: self.formula.normalize(eta, pi),
        }))
    }
    
    fn unfold(&self) -> Rc<MinlogProof> {
        Rc::new(MinlogProof::ImplicationIntro(ImplicationIntro {
            assumption: self.assumption.unfold(),
            conclusion: self.conclusion.unfold(),
            formula: self.formula.clone(),
        }))
    }
    
    fn extracted_term(&self) -> Option<Rc<MinlogTerm>> {
        self.conclusion.extracted_term().map(|term| {
            if let Some(assump_term) = self.assumption.extracted_term() {
                Abstraction::create(
                    vec![assump_term],
                    term,
                )
            } else {
                term
            }
        })?.remove_nulls()
    }
    
    fn get_type_variables(&self) -> IndexSet<Rc<MinlogType>> {
        self.assumption.get_type_variables()
            .union(&self.conclusion.get_type_variables())
            .cloned().collect()
    }
    
    fn get_algebra_types(&self) -> IndexSet<Rc<MinlogType>> {
        self.assumption.get_algebra_types()
            .union(&self.conclusion.get_algebra_types())
            .cloned().collect()
    }
    
    fn get_free_variables(&self) -> IndexSet<Rc<MinlogTerm>> {
        self.assumption.get_free_variables()
            .union(&self.conclusion.get_free_variables())
            .cloned().collect()
    }
    
    fn get_bound_variables(&self) -> IndexSet<Rc<MinlogTerm>> {
        self.assumption.get_bound_variables()
            .union(&self.conclusion.get_bound_variables())
            .cloned().collect()
    }
    
    fn get_predicate_variables(&self) -> IndexSet<Rc<MinlogPredicate>> {
        self.assumption.get_predicate_variables()
            .union(&self.conclusion.get_predicate_variables())
            .cloned().collect()
    }
    
    fn get_comprehension_terms(&self) -> IndexSet<Rc<MinlogPredicate>> {
        self.assumption.get_comprehension_terms()
            .union(&self.conclusion.get_comprehension_terms())
            .cloned().collect()
    }
    
    fn get_inductive_predicates(&self) -> IndexSet<Rc<MinlogPredicate>> {
        self.assumption.get_inductive_predicates()
            .union(&self.conclusion.get_inductive_predicates())
            .cloned().collect()
    }
    
    fn get_prime_formulas(&self) -> IndexSet<Rc<MinlogPredicate>> {
        self.assumption.get_prime_formulas()
            .union(&self.conclusion.get_prime_formulas())
            .cloned().collect()
    }

    fn get_goals(&self) -> IndexSet<Rc<MinlogProof>> {
        self.conclusion.get_goals()
            .union(&self.assumption.get_goals())
            .cloned().collect()
    }

    fn get_assumptions(&self) -> IndexSet<Rc<MinlogProof>> {
        self.conclusion.get_assumptions()
            .difference(&IndexSet::from([self.assumption.clone()]))
            .cloned().collect()
    }
    
    fn get_axioms(&self) -> IndexSet<Rc<MinlogProof>> {
        self.assumption.get_axioms()
            .union(&self.conclusion.get_axioms())
            .cloned().collect()
    }
    
    fn get_theorems(&self) -> IndexSet<Rc<MinlogProof>> {
        self.assumption.get_theorems()
            .union(&self.conclusion.get_theorems())
            .cloned().collect()
    }
    
    fn substitute(&self, from: &ProofSubstEntry, to: &ProofSubstEntry) -> Rc<MinlogProof> {
        if let ProofSubstEntry::Proof(from_proof) = from && from_proof.is_implication_intro() && self == from_proof.to_implication_intro().unwrap() {
            to.to_proof().unwrap()
        } else {
            ImplicationIntro::create(
                self.conclusion.substitute(from, to),
                self.assumption.substitute(from, to),
            )
        }
    }
    
    fn first_conflict_with(&self, other: &Rc<MinlogProof>) -> Option<(ProofSubstEntry, ProofSubstEntry)> {
        if let MinlogProof::ImplicationIntro(other_intro) = other.as_ref() {
            if let Some(conflict) = self.assumption.first_conflict_with(&other_intro.assumption) {
                return Some(conflict);
            }
            
            if let Some(conflict) = self.conclusion.first_conflict_with(&other_intro.conclusion) {
                return Some(conflict);
            }
            
            None
        } else {
            Some((Rc::new(MinlogProof::ImplicationIntro(self.clone())).into(), other.clone().into()))
        }
    }
    
    fn match_with(&self, instance: &Rc<MinlogProof>) -> MatchOutput<ProofSubstEntry> {
        if !instance.is_implication_intro() {
            return MatchOutput::FailedMatch;
        }
        
        let intro_instance = instance.to_implication_intro().unwrap();
        
        let mut conditions = IndexMap::new();
        
        if self.assumption != intro_instance.assumption {
            conditions.insert(self.assumption.clone().into(), intro_instance.assumption.clone().into());
        }
        
        if self.conclusion != intro_instance.conclusion {
            conditions.insert(self.conclusion.clone().into(), intro_instance.conclusion.clone().into());
        }
        
        MatchOutput::Matched(conditions)
    }
}

impl PrettyPrintable for ImplicationIntro {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        PPElement::group(vec![
            PPElement::text("→⁺ (".to_string()),
            PPElement::break_elem(1, 4, false),
            PPElement::list(
                vec![
                    self.conclusion.to_pp_element(detail),
                    self.assumption.to_pp_element(detail),
                ],
                PPElement::break_elem(0, 0, false),
                PPElement::text(",".to_string()),
                PPElement::break_elem(1, 0, false),
                BreakType::Consistent
            ),
            PPElement::break_elem(1, 0, false),
            PPElement::text(")".to_string()),
        ], BreakType::Consistent, 0)
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}

impl ProofTreeDisplayable for ImplicationIntro {
    fn to_proof_tree_node(&self) -> ProofTreeNode {
        ProofTreeNode::new_node(
            vec![self.conclusion.to_proof_tree_node()],
            self.formula.display_string(),
            Some(format!("→⁺{}", self.assumption.to_assumption().unwrap().name()))
        )
    }
}