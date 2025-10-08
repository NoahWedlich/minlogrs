
use std::fmt::Debug;
pub trait MatchContext<T>: Eq + Clone {
    fn new(patterns: &mut Vec<T>, instances: &mut Vec<T>) -> Self;
    
    fn next_pattern(&mut self) -> Option<T>;
    fn next_instance(&mut self) -> Option<T>;
    fn skip_current(&mut self);
    
    fn restrict(&mut self, element: &T);
    fn is_restricted(&self, element: &T) -> bool;
    
    fn substitute(&mut self, from: &T, to: &T);
}

pub trait Substitutable<Ctx: MatchContext<Self>>: Eq + Clone + Debug {
    fn substitute(&self, from: &Self, to: &Self) -> Self;
    
    fn first_conflict_with(&self, other: &Self) -> Option<(Self, Self)>;
    
    fn valid_substitution(&self, to: &Self) -> bool;

    fn match_with(&self, other: &Self, ctx: &mut Ctx) -> Option<(&Self, &Self)>;
}

pub trait Substitution<T: Substitutable<Ctx>, Ctx: MatchContext<T>>: Sized {
    fn make_empty() -> Self;
    
    fn pairs(&self) -> &Vec<(T, T)>;
    fn pairs_mut(&mut self) -> &mut Vec<(T, T)>;
    
    fn empty(&self) -> bool {
        self.pairs().is_empty()
    }
    
    fn size(&self) -> usize {
        self.pairs().len()
    }
    
    fn restrict<F>(&mut self, filter: F)
    where F: Fn(&T) -> bool {
        self.pairs_mut().retain(|(k, _)| filter(k));
    }
    
    fn contains(&self, key: &T) -> bool {
        self.pairs().iter().any(|(k, _)| k == key)
    }
    
    fn apply(&self, value: &T) -> T {
        self.pairs().iter()
            .find(|(k, _)| k == value)
            .map(|(_, v)| v.clone())
            .unwrap_or(value.clone())
    }
    
    fn substitute(&self, element: &T) -> T {
        let mut result = element.clone();
        
        for (k, v) in self.pairs().iter() {
            result = result.substitute(k, v);
        }

        result
    }
    
    fn substitute_all(&self, elements: &[T]) -> Vec<T> {
        elements.iter().map(|e| self.substitute(e)).collect()
    }
    
    fn collapse(&mut self) {
        self.pairs_mut().retain(|(k, v)| k != v);
    }

    fn extend(&mut self, pair: (T, T)) {
        if pair.0 != pair.1 {
            if !pair.0.valid_substitution(&pair.1) {
                panic!("Invalid substitution: {:?} -> {:?}", pair.0, pair.1);
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
    
    fn unify(first: &T, second: &T) -> Option<Self> {
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
    
    fn unify_all(elements: &mut [T]) -> Option<Self> {
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
    
    fn match_with(first: &T, second: &T) -> Option<Self> {
        Self::match_all(&mut [first.clone()], &mut [second.clone()])
    }
    
    fn match_all(patterns: &mut [T], instances: &mut [T]) -> Option<Self> {
        if patterns.len() != instances.len() {
            return None;
        }
        
        let mut ctx = Ctx::new(&mut patterns.to_vec(), &mut instances.to_vec());
        Self::match_on(&mut ctx)
    }
    
    fn match_on(ctx: &mut Ctx) -> Option<Self> {
        
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
                
                if let Some((from, to)) = pattern.match_with(&instance, ctx) {
                    if from.valid_substitution(&to) {
                        ctx.substitute(&from, &to);
                        substitution.extend((from.clone(), to.clone()));
                        
                        ctx.restrict(&from);
                        
                        ctx.skip_current();
                        
                        continue;
                    } else {
                        return None;
                    }
                }
                
            } else {
                return None;
            }
        }
        
        substitution.collapse();
        Some(substitution)
    }
    
    fn match_subst(pattern_subst: &Self, instance_subst: &Self) -> Option<Self> {
        let mut arguments = pattern_subst.pairs().iter().map(|(k, _)| k.clone()).collect::<Vec<T>>();
        arguments.extend(instance_subst.pairs().iter().map(|(k, _)| k.clone()));
        
        let patterns = arguments.iter().map(|a| pattern_subst.apply(a)).collect::<Vec<T>>();
        let instances = arguments.iter().map(|a| instance_subst.apply(a)).collect::<Vec<T>>();
        
        Self::match_all(&mut patterns.clone(), &mut instances.clone())
    }
}