
use indexmap::IndexMap;
use std::{hash::{Hash, Hasher}};
use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};

pub trait Substitutable: Hash + Eq + Clone + PrettyPrintable {
    fn substitute(&self, from: &Self, to: &Self) -> Self;
    
    fn first_conflict_with(&self, other: &Self) -> Option<(Self, Self)>;
    
    fn valid_substitution(&self, to: &Self) -> bool;

    fn match_with(&self, ctx: &mut impl MatchContext<Self>) -> MatchOutput<Self>;
}

impl<T: Substitutable> SubstitutableWith<T> for T {
    fn substitute_with(&self, from: &T, to: &T) -> T {
        self.substitute(from, to)
    }
}

pub trait SubstitutableWith<T>: Eq + Clone + PrettyPrintable {
    fn substitute_with(&self, from: &T, to: &T) -> Self;
}

#[derive(Clone)]
pub struct Substitution<T: Substitutable> {
    map: IndexMap<T, T>,
}

impl<T: Substitutable> Substitution<T> {
    pub fn make_empty() -> Self {
        Self {
            map: IndexMap::new(),
        }
    }

    pub fn from_pairs(pairs: Vec<(T, T)>) -> Self {
        let mut subst = Self::make_empty();
        
        for (from, to) in pairs {
            subst.extend((from, to));
        }
        
        subst
    }
    
    pub fn empty(&self) -> bool {
        self.map.is_empty()
    }
    
    pub fn size(&self) -> usize {
        self.map.len()
    }
    
    pub fn pairs(&self) -> Vec<(T, T)> {
        self.map.iter().map(|(k, v)| (k.clone(), v.clone())).collect()
    }
    
    pub fn restrict<F>(&mut self, filter: F)
    where F: Fn(&T) -> bool {
        self.map.retain(|k, _| filter(k));
    }
    
    pub fn contains(&self, key: &T) -> bool {
        self.map.contains_key(key)
    }
    
    pub fn apply(&self, value: &T) -> T {
        self.map.get(value).cloned().unwrap_or_else(|| value.clone())
    }
    
    pub fn substitute<U: SubstitutableWith<T>>(&self, element: &U) -> U {
        let mut result = element.clone();
        
        for (k, v) in self.map.iter() {
            result = result.substitute_with(k, v);
        }

        result
    }

    pub fn substitute_all<U: SubstitutableWith<T>>(&self, elements: &[U]) -> Vec<U> {
        elements.iter().map(|e| self.substitute::<U>(e)).collect()
    }
    
    pub fn collapse(&mut self) {
        self.map.retain(|k, v| k != v);
    }

    pub fn extend(&mut self, pair: (T, T)) {
        if pair.0 != pair.1 {
            if !pair.0.valid_substitution(&pair.1) {
                panic!("Invalid substitution: {} -> {}", pair.0.debug_string(), pair.1.debug_string());
            }
            
            for (_, v) in self.map.iter_mut() {
                *v = v.substitute(&pair.0, &pair.1);
            }
            
            if !self.map.iter().any(|(k, _)| k == &pair.0) {
                self.map.insert(pair.0, pair.1);
            }
            
            self.collapse();
        }
    }

    pub fn agrees(&self, _other: &Self) -> bool {
        for (k, v) in self.map.iter() {
            match _other.map.get(k) {
                Some(v2) if v != v2 => return false,
                _ => {}
            }
        }
        
        true
    }

    pub fn compose(&mut self, _other: &Self) {
        for (_, v) in self.map.iter_mut() {
            if let Some(v2) = _other.map.get(v) {
                *v = v2.clone();
            }
        }
        
        for (k, v) in _other.map.iter() {
            if !self.contains(k) {
                self.map.insert(k.clone(), v.clone());
            }
        }
        
        self.collapse();
    }
    
    pub fn unify(first: &T, second: &T) -> Option<Self> {
        let mut result = Self::make_empty();
        
        let mut first = first.clone();
        let mut second = second.clone();
        
        let mut conflict = first.first_conflict_with(&second);
        while let Some((left, right)) = conflict {
            if left.valid_substitution(&right) {
                result.extend((left.clone(), right.clone()));
                
                first = first.substitute(&left, &right);
                second = second.substitute(&left, &right);
            } else if right.valid_substitution(&left) {
                result.extend((right.clone(), left.clone()));

                first = first.substitute(&right, &left);
                second = second.substitute(&right, &left);
            } else {
                return None;
            }
            
            conflict = first.first_conflict_with(&second);
        }
        
        result.collapse();
        Some(result)
    }
    
    pub fn unify_all(elements: &mut [T]) -> Option<Self> {
        let mut result = Self::make_empty();
        
        for i in 0..elements.len() {
            for j in (i + 1)..elements.len() {
                if let Some(partial) = Self::unify(&elements[i], &elements[j]) {
                    result.compose(&partial);
                    
                    result.substitute_all(elements);
                } else {
                    return None;
                }
            }
        }
        
        result.collapse();
        Some(result)
    }
    
    pub fn match_with(pattern: &T, instance: &T) -> Option<Self> {
        Self::match_all(&mut [pattern.clone()], &mut [instance.clone()])
    }
    
    pub fn match_all(patterns: &mut [T], instances: &mut [T]) -> Option<Self> {
        if patterns.len() != instances.len() {
            return None;
        }
        
        let mut ctx = MatchContextImpl::new(&mut patterns.to_vec(), &mut instances.to_vec());
        Self::match_on(&mut ctx)
    }

    pub fn match_on(ctx: &mut impl MatchContext<T>) -> Option<Self> {

        let mut substitution = Self::make_empty();
        
        while let Some(pattern) = ctx.next_pattern() {
            if let Some(instance) = ctx.next_instance() {

                if ctx.is_restricted(&pattern) {
                    if pattern == instance {
                        ctx.skip_current();
                        continue;
                    } else {
                        return None;
                    }
                }
                
                if pattern == instance {
                    if pattern.valid_substitution(&instance) {
                        ctx.substitute(&pattern, &instance);
                        substitution.extend((pattern.clone(), instance.clone()));
                        
                        ctx.restrict(&pattern);
                    }
                    
                    ctx.skip_current();
                    continue;
                }
                
                match pattern.match_with(ctx) {
                    MatchOutput::Matched => {}
                    MatchOutput::FailedMatch => {
                        return None;
                    },
                    MatchOutput::Substitution(from, to) => {
                        if from.valid_substitution(&to) {
                            ctx.skip_current();
                            
                            ctx.substitute(&from, &to);
                            
                            substitution.extend((from.clone(), to.clone()));
                            ctx.restrict(&from);
                        }
                    }
                }
                
            } else {
                return None;
            }
        }
        
        substitution.collapse();
        Some(substitution)
    }
    
    pub fn match_subst(pattern_subst: &Self, instance_subst: &Self) -> Option<Self> {
        let mut arguments = pattern_subst.map.keys().cloned().collect::<Vec<T>>();
        arguments.extend(instance_subst.map.keys().cloned());
        
        let patterns = arguments.iter().map(|a| pattern_subst.apply(a)).collect::<Vec<T>>();
        let instances = arguments.iter().map(|a| instance_subst.apply(a)).collect::<Vec<T>>();
        
        Self::match_all(&mut patterns.clone(), &mut instances.clone())
    }
    
    pub fn add_subst_match(&self, other: &Self, ctx: &mut impl MatchContext<T>) {
        let mut arguments = self.map.keys().cloned().collect::<Vec<T>>();
        arguments.extend(other.map.keys().cloned());
        
        let patterns = arguments.iter().map(|a| self.apply(a)).collect::<Vec<T>>();
        let instances = arguments.iter().map(|a| other.apply(a)).collect::<Vec<T>>();
        
        for (p, i) in patterns.iter().zip(instances.iter()) {
            ctx.extend(p, i);
        }
    }
}

impl<T: Substitutable> PrettyPrintable for Substitution<T> {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        
        let mut elements = vec![];
        elements.push(PPElement::text("{".to_string()));
        elements.push(PPElement::break_elem(1, 4, false));
        
        elements.push(PPElement::list(
            self.map.iter().map(|(k, v)| {
                PPElement::group(vec![
                    k.to_enclosed_pp_element(detail),
                    PPElement::break_elem(1, 0, false),
                    PPElement::text("->".to_string()),
                    PPElement::break_elem(1, 0, false),
                    v.to_enclosed_pp_element(detail),
                ], BreakType::Flexible, 0)
            }).collect(),
            PPElement::break_elem(0, 0, false),
            PPElement::text(",".to_string()),
            PPElement::break_elem(1, 0, false),
            BreakType::Flexible,
        ));
        
        elements.push(PPElement::break_elem(1, 0, false));
        elements.push(PPElement::text("}".to_string()));
        
        PPElement::group(vec![
            PPElement::text("Substitution".to_string()),
            PPElement::break_elem(1, 0, false),
            PPElement::group(elements, BreakType::Consistent, 0)
        ], BreakType::Flexible, 0)
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        true
    }
    
    fn open_paren(&self) -> String {
        "{".to_string()
    }
    
    fn close_paren(&self) -> String {
        "}".to_string()
    }
}

impl<T: Substitutable> Hash for Substitution<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for (k, v) in self.pairs().iter() {
            k.hash(state);
            v.hash(state);
        }
    }
}

impl<T: Substitutable> PartialEq for Substitution<T> {
    fn eq(&self, other: &Self) -> bool {
        if self.size() != other.size() {
            return false;
        }
        
        for (k, v) in self.map.iter() {
            match other.map.get(k) {
                Some(v2) if v == v2 => {},
                _ => return false,
            }
        }
        
        true
    }
}

impl<T: Substitutable> Eq for Substitution<T> {}

pub trait MatchContext<T: Substitutable> {
    fn new(patterns: &mut Vec<T>, instances: &mut Vec<T>) -> Self;
    
    fn extend(&mut self, pattern: &T, instance: &T);
    
    fn next_pattern(&mut self) -> Option<T>;
    fn next_instance(&mut self) -> Option<T>;
    fn skip_current(&mut self);
    
    fn restrict(&mut self, element: &T);
    fn is_restricted(&self, element: &T) -> bool;
    
    fn substitute(&mut self, from: &T, to: &T);
}

pub struct MatchContextImpl<T> {
    patterns: Vec<T>,
    instances: Vec<T>,
    restricted: Vec<T>,
}

impl<T: Substitutable> MatchContext<T> for MatchContextImpl<T> {
    fn new(patterns: &mut Vec<T>, instances: &mut Vec<T>) -> Self {
        Self {
            patterns: std::mem::take(patterns),
            instances: std::mem::take(instances),
            restricted: vec![],
        }
    }

    fn extend(&mut self, pattern: &T, instance: &T) {
        if pattern != instance && (!self.patterns.contains(pattern) || !self.instances.contains(instance)) {
            self.patterns.push(pattern.clone());
            self.instances.push(instance.clone());
        }
    }
    
    fn next_pattern(&mut self) -> Option<T> {
        self.patterns.last().cloned()
    }

    fn next_instance(&mut self) -> Option<T> {
        self.instances.last().cloned()
    }
    
    fn skip_current(&mut self) {
        self.patterns.pop();
        self.instances.pop();
    }
    
    fn restrict(&mut self, element: &T) {
        self.restricted.push(element.clone());
    }
    
    fn is_restricted(&self, element: &T) -> bool {
        self.restricted.iter().any(|e| e == element)
    }
    
    fn substitute(&mut self, from: &T, to: &T) {
        for pattern in self.patterns.iter_mut() {
            *pattern = pattern.substitute(from, to);
        }
        
        for instance in self.instances.iter_mut() {
            *instance = instance.substitute(from, to);
        }
    }
}

pub enum MatchOutput<T> {
    Substitution(T, T),
    Matched,
    FailedMatch,
}