
use crate::includes::{
    essential::*,
    utils::*,
    core::{
        types::*,
        terms::*,
        predicates::*,
    }
};

#[derive(Clone, PartialEq, Eq)]
pub struct InductiveConstant {
    name: String,
    arity: Rc<MinlogType>,
    clauses: RefCell<Vec<(String, Rc<MinlogPredicate>)>>,
    computational_content: RefCell<Option<IDPComputationalContent>>,
}

impl InductiveConstant {
    pub fn create(name: String, arity: Rc<MinlogType>) -> Rc<InductiveConstant> {
        Rc::new(InductiveConstant {
            name,
            arity,
            clauses: RefCell::new(vec![]),
            computational_content: RefCell::new(None),
        })
    }
    
    pub fn name(&self) -> &String {
        &self.name
    }
    
    pub fn arity(&self) -> &Rc<MinlogType> {
        &self.arity
    }
    
    pub fn add_clause(&self, clause_name: String, clause_body: Rc<MinlogPredicate>) {
        if self.clauses.borrow().iter().any(|(n, _)| n == &clause_name) {
            panic!("Clause with name '{}' already exists in inductive constant '{}'", clause_name, self.name);
        }

        let ipreds = clause_body.get_polarized_inductive_preds(Polarity::StrictlyPositive, &mut IndexSet::new());
        if !ipreds.iter().any(|p| p.polarity.is_strictly_positive()
            && p.value.to_inductive_predicate().unwrap().definition().as_ref() == self) {
            panic!("Clause body does not contain strictly positive occurrence of inductive constant '{}'", self.name);
        }
        
        self.clauses.borrow_mut().push((clause_name, clause_body));
    }
    
    pub fn clauses(&self) -> Vec<(String, Rc<MinlogPredicate>)> {
        self.clauses.borrow().clone()
    }
    
    pub fn register_computational_content(&self, algebra_type: Rc<MinlogType>) {
        if !algebra_type.is_algebra() {
            panic!("register_computational_content called with a non-algebra type");
        }
        
        self.computational_content.borrow_mut().replace(IDPComputationalContent {
            algebra: algebra_type,
            clause_mapping: IndexMap::new(),
        });
    }
    
    pub fn make_computational(&self, existing: bool) {
        let algebra_type = if let Some(content) = self.computational_content.borrow().clone() {
            content.algebra.clone()
        } else {
            panic!("Inductive constant '{}' has no registered computational content", self.name);
        };
        
        let algebra = algebra_type.to_algebra().unwrap();
        
        *self.computational_content.borrow_mut() = Some(IDPComputationalContent { algebra: algebra_type.clone(), clause_mapping: IndexMap::new() });
        
        let mut clause_mapping = IndexMap::new();
        
        for (name, body) in self.clauses.borrow().iter() {
            let et_type = body.extracted_type_pattern();
            
            if existing {
                if let Some(existing) = algebra.constructor(name) {
                    if existing.minlog_type() != et_type {
                        panic!("Existing constructor '{}' has type '{}', but extracted type is '{}'",
                            name, existing.minlog_type().debug_string(), et_type.debug_string());
                    }
                    clause_mapping.insert(name.clone(), name.clone());
                } else if algebra.constructors().iter().filter(|c| c.minlog_type() == et_type).count() > 1 {
                    panic!("Multiple constructors with extracted type '{}' exist in the algebra, but none named '{}'",
                        et_type.debug_string(), name);
                } else if algebra.constructors().iter().all(|c| c.minlog_type() != et_type) {
                    panic!("No constructor with extracted type '{}' exists in the algebra for clause '{}'",
                        et_type.debug_string(), name);
                } else {
                    let constructor = algebra.constructors().iter()
                        .find(|c| c.minlog_type() == et_type).unwrap().clone();
                    clause_mapping.insert(name.clone(), constructor.to_constructor().unwrap().name().clone());
                }

            } else {
                let constr_name = format!("{}^et", name.clone());
                algebra.algebra().add_constructor(Constructor::create(constr_name.clone(), et_type));
                clause_mapping.insert(name.clone(), constr_name);
            }
        }
        
        if let Some(ref mut content) = *self.computational_content.borrow_mut() {
            content.clause_mapping = clause_mapping;
        }
    }
    
    pub fn is_computational(&self) -> bool {
        self.computational_content.borrow().is_some()
    }
    
    pub fn get_computational_content(&self) -> Option<IDPComputationalContent> {
        self.computational_content.borrow().clone()
    }
    
    pub fn get_type_variables(&self, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Rc<MinlogType>> {
        self.clauses.borrow().iter().flat_map(|(_, body)| body.get_type_variables(visited))
            .chain(self.arity.get_type_variables(&mut IndexSet::new())).collect()
    }
    
    pub fn get_free_variables(&self, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Rc<MinlogTerm>> {
        self.clauses.borrow().iter().flat_map(|(_, body)| body.get_free_variables(visited)).collect()
    }
    
    pub fn get_bound_variables(&self, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Rc<MinlogTerm>> {
        self.clauses.borrow().iter().flat_map(|(_, body)| body.get_bound_variables(visited)).collect()
    }
    
    pub fn get_polarized_pred_vars(&self, current: Polarity, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Polarized<Rc<MinlogPredicate>>> {
        self.clauses.borrow().iter().flat_map(|(_, body)| body.get_polarized_pred_vars(current, visited)).collect()
    }

    pub fn get_polarized_comp_terms(&self, current: Polarity, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Polarized<Rc<MinlogPredicate>>> {
        self.clauses.borrow().iter().flat_map(|(_, body)| body.get_polarized_comp_terms(current, visited)).collect()
    }

    pub fn get_polarized_inductive_preds(&self, current: Polarity, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Polarized<Rc<MinlogPredicate>>> {
        self.clauses.borrow().iter().flat_map(|(_, body)| body.get_polarized_inductive_preds(current, visited)).collect()
    }
    
    pub fn get_polarized_prime_formulas(&self, current: Polarity, visited: &mut IndexSet<MinlogPredicate>) -> IndexSet<Polarized<Rc<MinlogPredicate>>> {
        self.clauses.borrow().iter().flat_map(|(_, body)| body.get_polarized_prime_formulas(current, visited)).collect()
    }
}

impl PrettyPrintable for InductiveConstant {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        let tvars = self.get_type_variables(&mut IndexSet::new());
        let tmvars = self.get_free_variables(&mut IndexSet::new());
        let pvars = self.get_polarized_pred_vars(Polarity::Unknown, &mut IndexSet::new())
            .into_iter().map(|p| p.value).collect::<IndexSet<_>>();
        
        let has_tvars = !tvars.is_empty();
        let has_tmvars = !tmvars.is_empty();
        let has_pvars = !pvars.is_empty();
        
        let name = if !has_tvars && !has_tmvars && !has_pvars {
            PPElement::text(self.name.clone())
        } else {
            let tvars = PPElement::list(
                tvars.into_iter().map(|tv| tv.to_pp_element(false)).collect(),
                PPElement::break_elem(0, 0, false),
                PPElement::text(",".to_string()),
                PPElement::break_elem(1, 0, false),
                BreakType::Flexible,
            );
            
            let tmvars = PPElement::list(
                tmvars.into_iter().map(|tv| tv.to_pp_element(false)).collect(),
                PPElement::break_elem(0, 0, false),
                PPElement::text(",".to_string()),
                PPElement::break_elem(1, 0, false),
                BreakType::Flexible,
            );
            
            let pvars = PPElement::list(
                pvars.into_iter().map(|pv| pv.to_pp_element(false)).collect(),
                PPElement::break_elem(0, 0, false),
                PPElement::text(",".to_string()),
                PPElement::break_elem(1, 0, false),
                BreakType::Flexible,
            );
            
            let mut variable_list = vec![];
            if has_tvars { variable_list.push(tvars) };
            if has_tmvars { variable_list.push(tmvars) };
            if has_pvars { variable_list.push(pvars) };
            
            let variables = PPElement::list(
                variable_list,
                PPElement::break_elem(1, 0, false),
                PPElement::text("|".to_string()),
                PPElement::break_elem(1, 0, false),
                BreakType::Consistent
            );
            
            PPElement::group(vec![
                PPElement::text(self.name.clone()),
                PPElement::text("<".to_string()),
                PPElement::break_elem(1, 4, false),
                variables,
                PPElement::break_elem(1, 0, false),
                PPElement::text(">".to_string())
            ], BreakType::Consistent, 0)
        };
        
        if detail {
            let clauses = PPElement::list(
                self.clauses.borrow().iter().map(|(_, body)| body.to_pp_element(true)).collect(),
                PPElement::break_elem(0, 0, false),
                PPElement::text(";".to_string()),
                PPElement::break_elem(1, 0, true),
                BreakType::Flexible,
            );
            
            
            PPElement::group(vec![
                name,
                PPElement::text(" {".to_string()),
                PPElement::break_elem(1, 4, true),
                clauses,
                PPElement::break_elem(1, 0, true),
                PPElement::text("}".to_string())
            ], BreakType::Consistent, 0)
        } else {
            name
        }
    }
}

impl Hash for InductiveConstant {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.arity.hash(state);
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct IDPComputationalContent {
    pub algebra: Rc<MinlogType>,
    pub clause_mapping: IndexMap<String, String>,
}

impl PrettyPrintable for IDPComputationalContent {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        let header = PPElement::group(vec![
            PPElement::text("↘︎".to_string()),
            PPElement::break_elem(1, 4, false),
            self.algebra.to_pp_element(detail)
        ], BreakType::Consistent, 0);
        
        let mappings = PPElement::list(
            self.clause_mapping.iter().map(|(from, to)| {
                PPElement::group(vec![
                    PPElement::text(from.clone()),
                    PPElement::text(" -> ".to_string()),
                    PPElement::text(to.clone())
                ], BreakType::Consistent, 0)
            }).collect(),
            PPElement::break_elem(0, 0, false),
            PPElement::text(";".to_string()),
            PPElement::break_elem(1, 0, true),
            BreakType::Flexible,
        );
        
        PPElement::group(vec![
            header,
            PPElement::break_elem(1, 0, false),
            PPElement::text("{".to_string()),
            PPElement::break_elem(1, 4, true),
            mappings,
            PPElement::break_elem(1, 0, true),
            PPElement::text("}".to_string())
        ], BreakType::Consistent, 0)
    }
}