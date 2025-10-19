
use std::{cmp::max, rc::Rc};
use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};

use crate::core::substitution::{MatchContext, MatchOutput};

use crate::core::types::minlog_type::{TypeBody, MinlogType};

#[derive(Clone)]
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
    
    fn get_type_variables(&self) -> Vec<Rc<MinlogType>> {
        let mut inner = vec![];
        
        for arg in &self.arguments {
            inner.extend(arg.get_type_variables());
        }

        inner.extend(self.value.get_type_variables());
        inner
    }
    
    fn get_algebra_types(&self) -> Vec<Rc<MinlogType>> {
        let mut inner = vec![];
        
        for arg in &self.arguments {
            inner.extend(arg.get_algebra_types());
        }

        inner.extend(self.value.get_algebra_types());
        inner
    }
    
    fn remove_nulls(&self) -> Option<Rc<MinlogType>> {
        let mut new_arguments = vec![];
        
        for arg in &self.arguments {
            if let Some(new_arg) = MinlogType::remove_nulls(arg) {
                new_arguments.push(new_arg);
            } else {
                return None;
            }
        }
        
        MinlogType::remove_nulls(&self.value).map(|new_value| ArrowType::create(new_arguments, new_value))
    }

    fn substitute(&self, from: &Rc<MinlogType>, to: &Rc<MinlogType>) -> Rc<MinlogType> {
        let new_arguments = self.arguments.iter()
            .map(|arg| arg.substitute(from, to))
            .collect();
        let new_value = self.value.substitute(from, to);
        
        ArrowType::create(new_arguments, new_value)
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
    
    fn match_with(&self, ctx: &mut impl MatchContext<Rc<MinlogType>>) -> MatchOutput<Rc<MinlogType>> {
        let pattern = ctx.next_pattern().unwrap();
        let instance = ctx.next_instance().unwrap();
        
        if !pattern.is_arrow() || !instance.is_arrow() {
            return MatchOutput::FailedMatch;
        }

        let pattern_arrow = pattern.to_arrow().unwrap();
        let instance_arrow = instance.to_arrow().unwrap();

        if pattern_arrow.arguments.len() != instance_arrow.arguments.len() {
            return MatchOutput::FailedMatch;
        }

        for (arg1, arg2) in pattern_arrow.arguments.iter().zip(instance_arrow.arguments.iter()) {
            ctx.extend(arg1, arg2);
        }
        
        ctx.extend(&self.value, &instance_arrow.value);
        MatchOutput::Matched
    }
}

impl PrettyPrintable for ArrowType {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        if self.arguments.is_empty() {
            return self.value.to_pp_element(detail);
        }
        
        let mut elements = vec![];
        
        for (i, arg) in self.arguments.iter().enumerate() {
            elements.push(
                if i > 0 {
                    PPElement::group(vec![
                        PPElement::text("->".to_string()),
                        PPElement::break_elem(1, 4, false),
                        arg.to_enclosed_pp_element(detail)
                    ], BreakType::Flexible, 0)
                } else {
                    arg.to_enclosed_pp_element(detail)
                }
            );
            
            elements.push(PPElement::break_elem(1, 4, false));
        }
        
        elements.push(
            PPElement::group(
                vec![
                    PPElement::text("->".to_string()),
                    PPElement::break_elem(1, 4, false),
                    self.value.to_enclosed_pp_element(detail)
                ], BreakType::Flexible, 0
            )
        );
        
        PPElement::group(elements, BreakType::Flexible, 0)
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

impl PartialEq for ArrowType {
    fn eq(&self, other: &Self) -> bool {
        self.arguments == other.arguments && self.value == other.value
    }
}

impl Eq for ArrowType {}