
use crate::includes::{
    essential::*,
    utils::*,
    core::{
        types::*,
    }
};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ArrowType {
    argument: Rc<MinlogType>,
    value: Rc<MinlogType>,
}

impl ArrowType {
    pub fn create(argument: Rc<MinlogType>, value: Rc<MinlogType>) -> Rc<MinlogType> {
        Rc::new(MinlogType::Arrow(ArrowType { argument, value }))
    }
    
    pub fn create_nested(arguments: Vec<Rc<MinlogType>>, value: Rc<MinlogType>) -> Rc<MinlogType> {
        let mut current_value = value;
        
        for arg in arguments.into_iter().rev() {
            current_value = ArrowType::create(arg, current_value);
        }
        
        current_value
    }
    
    pub fn argument(&self) -> &Rc<MinlogType> {
        &self.argument
    }
    
    pub fn all_arguments(&self) -> Vec<Rc<MinlogType>> {
        let mut current = self;
        let mut args = vec![current.argument.clone()];
        
        while let Some(next_arrow) = current.value.to_arrow() {
            args.push(next_arrow.argument.clone());
            current = next_arrow;
        }
        
        args
    }
    
    pub fn argument_at(&self, index: usize) -> Option<&Rc<MinlogType>> {
        if index == 0 {
            Some(&self.argument)
        } else if self.value.is_arrow() {
            self.value.to_arrow().unwrap().argument_at(index - 1)
        } else {
            None
        }
    }
    
    pub fn value(&self) -> &Rc<MinlogType> {
        &self.value
    }
    
    pub fn final_value(&self) -> &Rc<MinlogType> {
        if let Some(next_arrow) = self.value.to_arrow() {
            next_arrow.final_value()
        } else {
            &self.value
        }
    }
}

impl TypeBody for ArrowType {
    fn is_object_type(&self) -> bool {
        self.argument.is_object_type() && self.value.is_object_type()
    }
    
    fn arity(&self) -> usize {
        1 + self.value.arity()
    }
    
    fn level(&self) -> usize {
        max(self.argument.level(), self.value.level())
    }
    
    fn get_polarized_tvars(&self, current: Polarity, visited: &mut IndexSet<MinlogType>) -> IndexSet<Polarized<Rc<MinlogType>>> {
        let mut result = self.argument.get_polarized_tvars(current.invert(), visited);
        
        result.extend(self.value.get_polarized_tvars(current, visited));
        
        result
    }

    fn get_polarized_algebras(&self, current: Polarity, visited: &mut IndexSet<MinlogType>) -> IndexSet<Polarized<Rc<MinlogType>>> {
        let mut result = self.argument.get_polarized_algebras(current.invert(), visited);
        
        result.extend(self.value.get_polarized_algebras(current, visited));

        result
    }
    
    fn remove_nulls(&self) -> Option<Rc<MinlogType>> {
        if let Some(new_value) = self.value.remove_nulls() {
            if let Some(new_argument) = self.argument.remove_nulls() {
                Some(ArrowType::create(new_argument, new_value))
            } else {
                Some(new_value)
            }
        } else {
            None
        }
    }

    fn substitute(&self, from: &Rc<MinlogType>, to: &Rc<MinlogType>) -> Rc<MinlogType> {
        if from.is_arrow() && self == from.to_arrow().unwrap() {
            to.clone()
        } else {
            let new_argument = self.argument.substitute(from, to);
            let new_value = self.value.substitute(from, to);
            
            ArrowType::create(new_argument, new_value)
        }
    }
    
    fn first_conflict_with(&self, other: &Rc<MinlogType>) -> Option<(Rc<MinlogType>, Rc<MinlogType>)> {
        if !other.is_arrow() {
            return Some((Rc::new(MinlogType::Arrow(self.clone())), other.clone()));
        }
        
        let other_arrow = other.to_arrow().unwrap();
        
        if let Some(conflict) = self.argument.first_conflict_with(&other_arrow.argument) {
            return Some(conflict);
        }
        
        self.value.first_conflict_with(&other_arrow.value)
    }
    
    fn match_with(&self, instance: &Rc<MinlogType>) -> MatchOutput<Rc<MinlogType>> {
        if !instance.is_arrow() {
            return MatchOutput::FailedMatch;
        }

        let instance_arrow = instance.to_arrow().unwrap();
        
        let conditions = IndexMap::from([
            (self.argument.clone(), instance_arrow.argument.clone()),
            (self.value.clone(), instance_arrow.value.clone()),
        ]);
        
        MatchOutput::Matched(conditions)
    }
}

impl PrettyPrintable for ArrowType {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        PPElement::list(
            vec![
                self.argument.to_enclosed_pp_element(detail),
                self.value.to_enclosed_pp_element(detail)
            ],
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