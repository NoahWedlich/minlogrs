
use indexmap::IndexSet;
use std::{rc::Rc, cell::RefCell, hash::{Hash, Hasher}};

use crate::utils::pretty_printer::{BreakType, PPElement, PrettyPrintable};
use crate::utils::proof_tree_display::{ProofTreeDisplayable, ProofTreeNode};

use crate::core::substitution::{MatchContext, MatchOutput, SubstitutableWith};

use crate::core::types::minlog_type::MinlogType;
use crate::core::terms::minlog_term::MinlogTerm;
use crate::core::predicates::minlog_predicate::MinlogPredicate;

use crate::core::proofs::minlog_proof::{MinlogProof, ProofBody};
use crate::core::proofs::proof_context::ProofContext;

use crate::core::proofs::proof_substitution::ProofSubstEntry;

#[derive(Clone, PartialEq, Eq)]
pub struct Goal {
    name: String,
    formula: Rc<MinlogPredicate>,
    context: RefCell<ProofContext>,
}

impl Goal {
    pub fn create(name: String, formula: Rc<MinlogPredicate>, context: ProofContext) -> Rc<MinlogProof> {
        if !formula.is_formula() {
            panic!("Can only create goals of nullary predicates")
        }
        
        for assumption in context.get_assumptions() {
            if assumption.proved_formula() == formula {
                return assumption.clone();
            }
        }
        
        Rc::new(MinlogProof::Goal(Goal { name, formula, context: RefCell::new(context) }))
    }
    
    pub fn name(&self) -> &str {
        &self.name
    }
    
    pub fn get_context(&self) -> std::cell::Ref<'_, ProofContext> {
        self.context.borrow()
    }
    
    pub fn get_context_mut(&self) -> std::cell::RefMut<'_, ProofContext> {
        self.context.borrow_mut()
    }
}

impl ProofBody for Goal {
    fn proved_formula(&self) -> Rc<MinlogPredicate> {
        self.formula.clone()
    }
    
    fn normalize(&self, eta: bool, pi: bool) -> Rc<MinlogProof> {
        let new_context = ProofContext {
            assumptions: self.context.borrow().assumptions.iter()
                .map(|a| a.normalize(eta, pi))
                .collect(),
            variables: self.context.borrow().variables.clone(),
        };
        
        let new_formula = self.formula.normalize(eta, pi);
        
        for assumption in &new_context.assumptions {
            if assumption.proved_formula() == new_formula {
                return assumption.clone();
            }
        }
        
        Rc::new(MinlogProof::Goal(Goal {
            name: self.name.clone(),
            formula: new_formula,
            context: RefCell::new(new_context),
        }))
    }
    
    fn unfold(&self) -> Rc<MinlogProof> {
        Rc::new(MinlogProof::Goal(self.clone()))
    }
    
    fn extracted_term(&self) -> Option<Rc<MinlogTerm>> {
        None
    }
    
    fn get_type_variables(&self, _visited: &mut IndexSet<MinlogProof>) -> IndexSet<Rc<MinlogType>> {
        self.formula.get_type_variables(&mut IndexSet::new())
    }
    
    fn get_algebra_types(&self, _visited: &mut IndexSet<MinlogProof>) -> IndexSet<Rc<MinlogType>> {
        self.formula.get_algebra_types(&mut IndexSet::new())
    }
    
    fn get_free_variables(&self, _visited: &mut IndexSet<MinlogProof>) -> IndexSet<Rc<MinlogTerm>> {
        self.formula.get_free_variables(&mut IndexSet::new())
            .union(&self.context.borrow().variables).cloned().collect()
    }
    
    fn get_bound_variables(&self, _visited: &mut IndexSet<MinlogProof>) -> IndexSet<Rc<MinlogTerm>> {
        self.formula.get_bound_variables(&mut IndexSet::new())
    }
    
    fn get_predicate_variables(&self, _visited: &mut IndexSet<MinlogProof>) -> IndexSet<Rc<MinlogPredicate>> {
        self.formula.get_predicate_variables(&mut IndexSet::new())
    }
    
    fn get_comprehension_terms(&self, _visited: &mut IndexSet<MinlogProof>) -> IndexSet<Rc<MinlogPredicate>> {
        self.formula.get_comprehension_terms(&mut IndexSet::new())
    }
    
    fn get_inductive_predicates(&self, _visited: &mut IndexSet<MinlogProof>) -> IndexSet<Rc<MinlogPredicate>> {
        self.formula.get_inductive_predicates(&mut IndexSet::new())
    }
    
    fn get_prime_formulas(&self, _visited: &mut IndexSet<MinlogProof>) -> IndexSet<Rc<MinlogPredicate>> {
        self.formula.get_prime_formulas(&mut IndexSet::new())
    }
    
    fn get_goals(&self, _visited: &mut IndexSet<MinlogProof>) -> IndexSet<Rc<MinlogProof>> {
        IndexSet::from([Rc::new(MinlogProof::Goal(self.clone()))])
    }
    
    fn get_assumptions(&self, _visited: &mut IndexSet<MinlogProof>) -> IndexSet<Rc<MinlogProof>> {
        self.context.borrow().assumptions.clone()
    }
    
    fn substitute(&self, from: &ProofSubstEntry, to: &ProofSubstEntry) -> Rc<MinlogProof> {
        if let ProofSubstEntry::Proof(from_proof) = from && from_proof.is_goal() && self == from_proof.to_goal().unwrap() {
            to.to_proof().unwrap()
        } else {
            let new_context = ProofContext {
                    assumptions: self.context.borrow().assumptions.iter()
                        .map(|a| a.substitute(from, to))
                        .collect(),
                    variables: self.context.borrow().variables.iter()
                        .map(|v| v.substitute_with(from, to))
                        .collect()
            };
            
            let new_formula = self.formula.substitute_with(from, to);
            
            for assumption in &new_context.assumptions {
                if assumption.proved_formula() == new_formula {
                    return assumption.clone();
                }
            }
            
            Rc::new(MinlogProof::Goal(Goal {
                name: self.name.clone(),
                formula: new_formula,
                context: RefCell::new(new_context),
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
                    ctx.extend(&p.clone().into(), &i.clone().into());
                    return MatchOutput::Matched;
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
            PPElement::text("} (".to_string()),
            PPElement::break_elem(1, 4, false),
            self.context.borrow().to_pp_element(detail),
            PPElement::break_elem(1, 0, false),
            PPElement::text(")".to_string()),
        ], BreakType::Consistent, 0)
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}

impl ProofTreeDisplayable for Goal {
    fn to_proof_tree_node(&self) -> ProofTreeNode {
        let v_ell_left_offset = self.name.chars().count() / 2;
        let v_ell_right_offset = self.name.chars().count() - v_ell_left_offset - 1;
        let v_ell = format!("{}⋮{}", " ".repeat(v_ell_left_offset), " ".repeat(v_ell_right_offset));
        
        ProofTreeNode::new_node(vec![
            ProofTreeNode::new_node(
                vec![ProofTreeNode::new_leaf(self.context.borrow().display_string())],
                format!("{}\n{}\n{}", v_ell.clone(), self.name, v_ell),
                None
            )
        ], self.formula.display_string(), None)
    }
}

impl Hash for Goal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.formula.hash(state);
    }
}