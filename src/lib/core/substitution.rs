
use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};

pub trait Substitutable: Eq + Clone + PrettyPrintable {
    fn substitute(&self, from: &Self, to: &Self) -> Self;
    
    fn first_conflict_with(&self, other: &Self) -> Option<(Self, Self)>;
    
    fn valid_substitution(&self, to: &Self) -> bool;

    fn match_with(&self, ctx: &mut impl MatchContext<Self>) -> Result<Option<(Self, Self)>, ()>;
}

pub trait Substitution: Sized {
    
    type ElementType: Substitutable;
    
    fn make_empty() -> Self;
    
    fn from_pairs(pairs: Vec<(Self::ElementType, Self::ElementType)>) -> Self {
        let mut subst = Self::make_empty();
        
        for (from, to) in pairs {
            subst.extend((from, to));
        }
        
        subst
    }
    
    fn pairs(&self) -> &Vec<(Self::ElementType, Self::ElementType)>;
    fn pairs_mut(&mut self) -> &mut Vec<(Self::ElementType, Self::ElementType)>;
    
    fn empty(&self) -> bool {
        self.pairs().is_empty()
    }
    
    fn size(&self) -> usize {
        self.pairs().len()
    }
    
    fn restrict<F>(&mut self, filter: F)
    where F: Fn(&Self::ElementType) -> bool {
        self.pairs_mut().retain(|(k, _)| filter(k));
    }
    
    fn contains(&self, key: &Self::ElementType) -> bool {
        self.pairs().iter().any(|(k, _)| k == key)
    }
    
    fn apply(&self, value: &Self::ElementType) -> Self::ElementType {
        self.pairs().iter()
            .find(|(k, _)| k == value)
            .map(|(_, v)| v.clone())
            .unwrap_or(value.clone())
    }
    
    fn substitute(&self, element: &Self::ElementType) -> Self::ElementType {
        let mut result = element.clone();
        
        for (k, v) in self.pairs().iter() {
            result = result.substitute(k, v);
        }

        result
    }
    
    fn substitute_all(&self, elements: &[Self::ElementType]) -> Vec<Self::ElementType> {
        elements.iter().map(|e| self.substitute(e)).collect()
    }
    
    fn collapse(&mut self) {
        self.pairs_mut().retain(|(k, v)| k != v);
    }

    fn extend(&mut self, pair: (Self::ElementType, Self::ElementType)) {
        if pair.0 != pair.1 {
            if !pair.0.valid_substitution(&pair.1) {
                panic!("Invalid substitution: {} -> {}", pair.0.debug_string(), pair.1.debug_string());
            }
            
            let mut contains = false;
            
            for (k, v) in self.pairs_mut().iter_mut() {
                if v.clone().eq(&pair.1) {
                    *v = pair.1.clone();
                }
                
                if k.clone().eq(&pair.1) {
                    if !v.clone().eq(&pair.1) {
                        *v = pair.1.clone();
                    }
                    
                    contains = true;
                }
            }
            
            if !contains {
                self.pairs_mut().push(pair);
            }
        }
    }

    fn equals(&self, _other: &Self) -> bool {
        if self.size() != _other.size() {
            return false;
        }
        
        for (k, v) in self.pairs().iter() {
            if !_other.pairs().iter().any(|(k2, v2)| k == k2 && v == v2) {
                return false;
            }
        }
        
        true
    }

    fn agrees(&self, _other: &Self) -> bool {
        for (k, v) in self.pairs().iter() {
            if let Some((_, v2)) = _other.pairs().iter().find(|(k2, _)| k == k2) {
                if v != v2 {
                    return false;
                }
            }
        }
        
        true
    }

    fn compose(&mut self, _other: &Self) {
        for (k, v) in self.pairs_mut().iter_mut() {
            if let Some((_, v2)) = _other.pairs().iter().find(|(k2, _)| k == k2) {
                *v = v2.clone();
            }
        }
        
        for (k, v) in _other.pairs().iter() {
            if !self.contains(k) {
                self.pairs_mut().push((k.clone(), v.clone()));
            }
        }
        
        self.collapse();
    }
    
    fn unify(first: &Self::ElementType, second: &Self::ElementType) -> Option<Self> {
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
    
    fn unify_all(elements: &mut [Self::ElementType]) -> Option<Self> {
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
    
    fn match_with(first: &Self::ElementType, second: &Self::ElementType) -> Option<Self> {
        Self::match_all(&mut [first.clone()], &mut [second.clone()])
    }
    
    fn match_all(patterns: &mut [Self::ElementType], instances: &mut [Self::ElementType]) -> Option<Self> {
        if patterns.len() != instances.len() {
            return None;
        }
        
        let mut ctx = SimpleMatchContext::new(&mut patterns.to_vec(), &mut instances.to_vec());
        Self::match_on(&mut ctx)
    }

    fn match_on(ctx: &mut impl MatchContext<Self::ElementType>) -> Option<Self> {

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
                        
                        ctx.skip_current();
                        
                        continue;
                    } else {
                        return None;
                    }
                }
                
                let match_result = pattern.match_with(ctx);
                if match_result.is_err() {
                    return None;
                } else if let Ok(Some((from, to))) = match_result {
                    if from.valid_substitution(&to) {
                        ctx.substitute(&from, &to);
                        
                        substitution.extend((from.clone(), to.clone()));
                        ctx.restrict(&from);
                        
                        ctx.skip_current();
                        
                        continue;
                    }
                }
                
                return None;
                
            } else {
                return None;
            }
        }
        
        substitution.collapse();
        Some(substitution)
    }
    
    fn match_subst(pattern_subst: &Self, instance_subst: &Self) -> Option<Self> {
        let mut arguments = pattern_subst.pairs().iter().map(|(k, _)| k.clone()).collect::<Vec<Self::ElementType>>();
        arguments.extend(instance_subst.pairs().iter().map(|(k, _)| k.clone()));
        
        let patterns = arguments.iter().map(|a| pattern_subst.apply(a)).collect::<Vec<Self::ElementType>>();
        let instances = arguments.iter().map(|a| instance_subst.apply(a)).collect::<Vec<Self::ElementType>>();
        
        Self::match_all(&mut patterns.clone(), &mut instances.clone())
    }
}

impl<Sub: Substitution> PrettyPrintable for Sub {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        
        let mut elements = vec![];
        elements.push(PPElement::text("{".to_string()));
        elements.push(PPElement::break_elem(1, 4, false));
        
        for (i, (k, v)) in self.pairs().iter().enumerate() {
            elements.push(
                if i < self.pairs().len() - 1 {
                    PPElement::group(vec![
                        PPElement::group(vec![
                            k.to_enclosed_pp_element(detail),
                            PPElement::break_elem(1, 4, false),
                            PPElement::text("->".to_string()),
                            PPElement::break_elem(1, 4, false),
                            v.to_enclosed_pp_element(detail),
                        ], BreakType::Flexible, 0),
                        PPElement::break_elem(0, 4, false),
                        PPElement::text(",".to_string()),
                    ], BreakType::Flexible, 0)
                } else {
                    PPElement::group(vec![
                        k.to_enclosed_pp_element(detail),
                        PPElement::break_elem(1, 4, false),
                        PPElement::text("->".to_string()),
                        PPElement::break_elem(1, 4, false),
                        v.to_enclosed_pp_element(detail),
                    ], BreakType::Flexible, 0)
                }
            );
            
            if i + 1 < self.pairs().len() {
                elements.push(PPElement::break_elem(1, 4, false));
            }
        }
        
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

pub struct SimpleMatchContext<T> {
    patterns: Vec<T>,
    instances: Vec<T>,
    restricted: Vec<T>,
}

impl<T: Substitutable> MatchContext<T> for SimpleMatchContext<T> {
    fn new(patterns: &mut Vec<T>, instances: &mut Vec<T>) -> Self {
        Self {
            patterns: patterns.drain(..).collect(),
            instances: instances.drain(..).collect(),
            restricted: vec![],
        }
    }

    fn extend(&mut self, pattern: &T, instance: &T) {
        self.patterns.push(pattern.clone());
        self.instances.push(instance.clone());
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