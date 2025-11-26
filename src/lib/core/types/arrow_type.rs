
use crate::includes::{
    essential::*,
    utils::*,
    core::{
        types::*,
    }
};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ArrowType {
    arguments: Vec<Rc<MinlogType>>,
    value: Rc<MinlogType>,
}

impl ArrowType {
    pub fn create(arguments: Vec<Rc<MinlogType>>, value: Rc<MinlogType>) -> Rc<MinlogType> {
        ArrowType::collapse(&Rc::new(MinlogType::Arrow(ArrowType { arguments, value })))
    }
    
    pub fn collapse(minlog_type: &Rc<MinlogType>) -> Rc<MinlogType> {
        if !minlog_type.is_arrow() || !minlog_type.to_arrow().unwrap().value().is_arrow() {
            minlog_type.clone()
        } else {
            let mut arrow = minlog_type.to_arrow().unwrap();
            let mut arguments = arrow.arguments().clone();
            
            while arrow.value().is_arrow() {
                let next_arrow = arrow.value().to_arrow().unwrap();
                arguments.extend(next_arrow.arguments().clone());
                arrow = next_arrow;
            }
            
            ArrowType::create(arguments, arrow.value().clone())
        }
    }
    
    pub fn arguments(&self) -> &Vec<Rc<MinlogType>> {
        &self.arguments
    }
    
    pub fn argument(&self, index: usize) -> Option<&Rc<MinlogType>> {
        self.arguments.get(index)
    }
    
    pub fn value(&self) -> &Rc<MinlogType> {
        &self.value
    }
}

impl TypeBody for ArrowType {
    fn is_object_type(&self) -> bool {
        self.arguments.iter().all(|arg| arg.is_object_type()) && self.value.is_object_type()
    }
    
    fn arity(&self) -> usize {
        self.arguments.len() + self.value.arity()
    }
    
    fn level(&self) -> usize {
        max(self.arguments.iter().map(|arg| arg.level()).max().unwrap_or(0), self.value.level())
    }
    
    fn get_polarized_tvars(&self, current: Polarity, visited: &mut IndexSet<MinlogType>) -> IndexSet<Polarized<Rc<MinlogType>>> {
        let mut result = self.arguments.iter()
            .flat_map(|arg| arg.get_polarized_tvars(current.invert(), visited))
            .collect::<IndexSet<_>>();
        
        result.extend(self.value.get_polarized_tvars(current, visited));
        
        result
    }

    fn get_polarized_algebras(&self, current: Polarity, visited: &mut IndexSet<MinlogType>) -> IndexSet<Polarized<Rc<MinlogType>>> {
        let mut result = self.arguments.iter()
            .flat_map(|arg| arg.get_polarized_algebras(current.invert(), visited))
            .collect::<IndexSet<_>>();

        result.extend(self.value.get_polarized_algebras(current, visited));

        result
    }
    
    fn remove_nulls(&self) -> Option<Rc<MinlogType>> {
        let mut new_arguments = vec![];
        
        for arg in &self.arguments {
            if let Some(new_arg) = arg.remove_nulls() {
                new_arguments.push(new_arg);
            }
        }
        
        if let Some(new_value) = self.value.remove_nulls() {
            if new_arguments.is_empty() {
                Some(new_value)
            } else {
                Some(ArrowType::create(new_arguments, new_value))
            }
        } else {
            None
        }
    }

    fn substitute(&self, from: &Rc<MinlogType>, to: &Rc<MinlogType>) -> Rc<MinlogType> {
        if from.is_arrow() && self == from.to_arrow().unwrap() {
            to.clone()
        } else {
            let new_arguments = self.arguments.iter()
                .map(|arg| arg.substitute(from, to))
                .collect();
            let new_value = self.value.substitute(from, to);
            
            ArrowType::create(new_arguments, new_value)
        }
    }
    
    fn first_conflict_with(&self, other: &Rc<MinlogType>) -> Option<(Rc<MinlogType>, Rc<MinlogType>)> {
        if !other.is_arrow() {
            return Some((Rc::new(MinlogType::Arrow(self.clone())), other.clone()));
        }
        
        let other_arrow = other.to_arrow().unwrap();
        
        if self.arguments.len() != other_arrow.arguments.len() {
            return Some((Rc::new(MinlogType::Arrow(self.clone())), other.clone()));
        }
        
        for (arg1, arg2) in self.arguments.iter().zip(other_arrow.arguments.iter()) {
            if let Some(conflict) = arg1.first_conflict_with(arg2) {
                return Some(conflict);
            }
        }
        
        self.value.first_conflict_with(&other_arrow.value)
    }
    
    fn match_with(&self, instance: &Rc<MinlogType>) -> MatchOutput<Rc<MinlogType>> {
        if !instance.is_arrow() {
            return MatchOutput::FailedMatch;
        }

        let instance_arrow = instance.to_arrow().unwrap();

        if self.arguments.len() != instance_arrow.arguments.len() {
            return MatchOutput::FailedMatch;
        }
        
        let mut conditions = self.arguments.iter().cloned()
            .zip(instance_arrow.arguments.iter().cloned())
            .filter(|(t1, t2)| t1 != t2)
            .collect::<IndexMap<_, _>>();
        conditions.insert(self.value.clone(), instance_arrow.value.clone());
        
        MatchOutput::Matched(conditions)
    }
}

impl PrettyPrintable for ArrowType {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        if self.arguments.is_empty() {
            return self.value.to_pp_element(detail);
        }
        
        PPElement::list(
            self.arguments.iter().map(|arg| arg.to_enclosed_pp_element(detail))
                .chain(std::iter::once(self.value.to_enclosed_pp_element(detail))).collect(),
            PPElement::break_elem(1, 4, false),
            PPElement::text("->".to_string()),
            PPElement::break_elem(1, 4, false),
            BreakType::Flexible
        )
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        true
    }
    
    fn open_paren(&self) -> String {
        "(".to_string()
    }
    
    fn close_paren(&self) -> String {
        ")".to_string()
    }
}