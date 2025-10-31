
use std::{rc::Rc, hash::{Hash, Hasher}, collections::HashSet};

use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};
use crate::utils::proof_tree_display::{ProofTreeDisplayable, ProofTreeNode};

use crate::core::substitution::{MatchContext, MatchOutput, SubstitutableWith};

use crate::core::types::minlog_type::MinlogType;
use crate::core::terms::minlog_term::MinlogTerm;
use crate::core::predicates::minlog_predicate::MinlogPredicate;

use crate::core::proofs::minlog_proof::{MinlogProof, ProofBody};

use crate::core::proofs::proof_substitution::ProofSubstEntry;

#[derive(Clone, PartialEq, Eq)]
pub struct Goal {
    name: String,
    formula: Rc<MinlogPredicate>,
    vars: HashSet<Rc<MinlogTerm>>,
    assumptions: HashSet<Rc<MinlogProof>>,
}

impl Goal {
    pub fn create(name: String, formula: Rc<MinlogPredicate>,
        vars: HashSet<Rc<MinlogTerm>>, assumptions: HashSet<Rc<MinlogProof>>
    ) -> Rc<MinlogProof> {

        if !formula.is_formula() {
            panic!("Can only create goals of nullary predicates")
        }

        for var in &vars {
            if !var.is_variable() {
                panic!("Goal::create called with a non-variable term in vars");
            }
        }
        
        for assump in &assumptions {
            if !assump.is_assumption() {
                panic!("Goal::create called with a non-assumption proof in assumptions");
            }
        }
        
        Rc::new(MinlogProof::Goal(Goal { name, formula, vars, assumptions }))
    }
    
    pub fn name(&self) -> &str {
        &self.name
    }
    
    pub fn vars(&self) -> &HashSet<Rc<MinlogTerm>> {
        &self.vars
    }
    
    pub fn assumptions(&self) -> &HashSet<Rc<MinlogProof>> {
        &self.assumptions
    }
}

impl ProofBody for Goal {
    fn proved_formula(&self) -> Rc<MinlogPredicate> {
        self.formula.clone()
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> Rc<MinlogProof> {
        Rc::new(MinlogProof::Goal(Goal {
            name: self.name.clone(),
            formula: self.formula.normalize(eta, pi),
            vars: self.vars.clone(),
            assumptions: self.assumptions.iter().map(|a| a.normalize(eta, pi)).collect(),
        }))
    }
    
    fn get_type_variables(&self) -> HashSet<Rc<MinlogType>> {
        self.formula.get_type_variables()
    }
    
    fn get_algebra_types(&self) -> HashSet<Rc<MinlogType>> {
        self.formula.get_algebra_types()
    }
    
    fn get_free_variables(&self) -> HashSet<Rc<MinlogTerm>> {
        self.formula.get_free_variables()
    }
    
    fn get_bound_variables(&self) -> HashSet<Rc<MinlogTerm>> {
        self.formula.get_bound_variables()
    }
    
    fn get_predicate_variables(&self) -> HashSet<Rc<MinlogPredicate>> {
        self.formula.get_predicate_variables()
    }
    
    fn get_comprehension_terms(&self) -> HashSet<Rc<MinlogPredicate>> {
        self.formula.get_comprehension_terms()
    }
    
    fn get_inductive_predicates(&self) -> HashSet<Rc<MinlogPredicate>> {
        self.formula.get_inductive_predicates()
    }
    
    fn get_prime_formulas(&self) -> HashSet<Rc<MinlogPredicate>> {
        self.formula.get_prime_formulas()
    }
    
    fn get_goals(&self) -> HashSet<Rc<MinlogProof>> {
        HashSet::from([Rc::new(MinlogProof::Goal(self.clone()))])
    }
    
    fn substitute(&self, from: &ProofSubstEntry, to: &ProofSubstEntry) -> Rc<MinlogProof> {
        if let ProofSubstEntry::Proof(from_proof) = from && from_proof.is_goal() && self == from_proof.to_goal().unwrap() {
            to.to_proof().unwrap()
        } else {
            Rc::new(MinlogProof::Goal(Goal {
                name: self.name.clone(),
                formula: self.formula.substitute_with(from, to),
                vars: self.vars.iter().map(|v| v.substitute_with(from, to)).collect(),
                assumptions: self.assumptions.iter()
                    .map(|a| a.substitute(from, to))
                    .collect(),
            }))
        }
    }
    
    fn first_conflict_with(&self, other: &Rc<MinlogProof>) -> Option<(ProofSubstEntry, ProofSubstEntry)> {
        if let Some(conflict) = self.formula.first_conflict_with(&other.proved_formula()) {
            return Some((conflict.0.into(), conflict.1.into()));
        }
        
        if other.is_goal() && self == other.to_goal().unwrap() {
            None
        } else {
            Some((Rc::new(MinlogProof::Goal(self.clone())).into(), other.clone().into()))
        }
    }
    
    fn match_with(&self, ctx: &mut impl MatchContext<ProofSubstEntry>) -> MatchOutput<ProofSubstEntry> {
        let pattern = ctx.next_pattern().unwrap();
        let instance = ctx.next_instance().unwrap();
        
        match (pattern, instance) {
            (ProofSubstEntry::Proof(p), ProofSubstEntry::Proof(i)) => {
                if !p.is_goal() {
                    return MatchOutput::FailedMatch;
                }
                
                if p.proved_formula() != i.proved_formula() {
                    ctx.extend(&p.proved_formula().into(), &i.proved_formula().into());
                }
                
                MatchOutput::Substitution(p.into(), i.into())
            },
            _ => MatchOutput::FailedMatch,
        }
    }
}

impl PrettyPrintable for Goal {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        PPElement::group(vec![
            PPElement::text("{".to_string()),
            PPElement::break_elem(1, 4, false),
            PPElement::group(vec![
                PPElement::text(self.name.clone()),
                PPElement::break_elem(0, 0, false),
                PPElement::text(":".to_string()),
                PPElement::break_elem(1, 4, false),
                self.formula.to_pp_element(detail),
            ], BreakType::Flexible, 0),
            PPElement::break_elem(1, 0, false),
            PPElement::text("}".to_string()),
        ], BreakType::Consistent, 0)
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}

impl ProofTreeDisplayable for Goal {
    fn to_proof_tree_node(&self) -> ProofTreeNode {
        ProofTreeNode::new_node(vec![
            ProofTreeNode::new_leaf(format!("{}\n⋮", self.name.clone()))
        ], self.formula.display_string(), Some("Goal".to_string()))
    }
}

impl Hash for Goal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.formula.hash(state);
    }
}