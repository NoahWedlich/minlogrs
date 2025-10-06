use std::sync::atomic::AtomicUsize;
use std::cell::RefCell;
use std::rc::Rc;

static GROUP_INDEX_COUNTER: AtomicUsize = AtomicUsize::new(0);

pub struct GroupIndex {
    index: usize
}

impl GroupIndex {
    pub fn new() -> Self {
        Self {
            index: GROUP_INDEX_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
        }
    }
}
    
pub trait RefGroupable: Sized + Eq {
    fn add_group(&self, group: &RefGroup<Self>);
    fn del_group(&self, group: &RefGroup<Self>);

    fn copy_if_needed(&self, group: &Rc<RefCell<RefGroup<Self>>>, children: &mut Vec<IRef<Self>>) -> Option<Rc<Self>>;
}

type CellRc<T> = RefCell<Rc<T>>;

pub struct RefGroup<T: RefGroupable> {
    members: Vec<CellRc<T>>,
    index: GroupIndex
}

impl<T: RefGroupable> RefGroup<T> {
    pub fn new() -> Self {
        Self {
            members: Vec::new(),
            index: GroupIndex::new()
        }
    }
    
    pub fn index(&self) -> usize {
        self.index.index
    }
    
    pub fn contains(&self, element: &Rc<T>) -> bool {
        self.members.iter().any(|e| e.borrow().as_ref().eq(element.as_ref()))
    }
    
    pub fn get(&self, element: &Rc<T>) -> Option<usize> {
        self.members.iter().position(|e| e.borrow().as_ref().eq(element.as_ref()))
    }
    
    pub fn add(&mut self, element: &Rc<T>) -> usize {
        if !self.contains(element) {
            element.add_group(self);
            self.members.push(RefCell::new(element.clone()));
        }
        
        self.get(element).unwrap()
    }

    pub fn redirect(&self, index: usize, new_element: &Rc<T>) {
        if let Some(pos) = self.members.get(index) {
            pos.borrow().as_ref().del_group(self);
            new_element.add_group(self);
            self.members[index].replace(new_element.clone());
        }
    }
}

impl<T: RefGroupable> Drop for RefGroup<T> {
    fn drop(&mut self) {
        for member in &self.members {
            let element = member.borrow();
            element.del_group(self);
        }
    }
}

impl<T: RefGroupable> PartialEq for RefGroup<T> {
    fn eq(&self, other: &Self) -> bool {
        self.index() == other.index()
    }
}

impl<T: RefGroupable> Eq for RefGroup<T> {}

impl<T: RefGroupable> std::fmt::Debug for RefGroup<T> where T: std::fmt::Debug {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "RefGroup<{}>[{}] {{ ", std::any::type_name::<T>(), self.index())?;
        for (i, member) in self.members.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{:?}", member.borrow().as_ref())?;
        }
        write!(f, " }}")
    }
}

impl<T: RefGroupable> std::fmt::Display for RefGroup<T> where T: std::fmt::Display {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "RefGroup[{}] {{ ", self.index())?;
        for (i, member) in self.members.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", member.borrow().as_ref())?;
        }
        write!(f, " }}")
    }
}

pub type IRefGroup<T> = Rc<RefCell<RefGroup<T>>>;

pub struct IRef<T: RefGroupable> {
    group: IRefGroup<T>,
    index: usize
}

impl<T: RefGroupable> IRef<T> {
    pub fn new(element: Rc<T>, group: IRefGroup<T>) -> Self {
        let index = group.borrow_mut().add(&element);
        Self {
            group,
            index
        }
    }
    
    pub fn get(&self) -> &T {
        unsafe { (*self.group.borrow().members[self.index].as_ptr()).as_ref() }
    }
    
    pub fn get_shared(&self) -> &Rc<T> {
        unsafe { &*self.group.borrow().members[self.index].as_ptr() }
    }
    
    pub fn get_index(&self) -> usize {
        self.index
    }
    
    pub fn redirect(&self, new_element: Rc<T>) {
        self.group.borrow().redirect(self.index, &new_element);
    }
    
    pub fn shallow_copy_to(&self, other: &IRefGroup<T>) -> Option<IRef<T>> {
        if self.group.borrow().index() == other.borrow().index() {
            return None;
        }
        
        let copied = &IRef::new(self.get_shared().clone(), other.clone());
        Some(copied.clone())
    }
    
    pub fn copy_to(&self, other: &IRefGroup<T>) -> IRef<T> {
        let copied = self.shallow_copy_to(other);
        
        if copied.is_none() {
            return self.clone();
        }
        
        let mut to_copy = vec![copied.clone().unwrap()];
        
        while let Some(item) = to_copy.pop() {
            if item.group.borrow().index() != other.borrow().index() {
                panic!(
                    "copy_if_needed violated contract: returned item in group {} but expected group {}",
                    item.group.borrow().index(), other.borrow().index()
                );
            }
            
            let copied = item.get_shared()
                .copy_if_needed(other, &mut to_copy)
                .unwrap_or(item.get_shared().clone());
            
            item.redirect(copied.clone());
        }
        
        copied.unwrap()
    }
}

impl<T: RefGroupable> AsRef<T> for IRef<T> {
    fn as_ref(&self) -> &T {
        self.get()
    }
}

impl<T: RefGroupable> Clone for IRef<T> {
    fn clone(&self) -> Self {
        Self {
            group: self.group.clone(),
            index: self.index
        }
    }
}

impl<T: RefGroupable> PartialEq for IRef<T> {
    fn eq(&self, other: &Self) -> bool {
        self.group.eq(&other.group) && self.index == other.index
    }
}

impl<T: RefGroupable> Eq for IRef<T> {}

impl<T: RefGroupable + std::hash::Hash> std::hash::Hash for IRef<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.get_shared().hash(state);
    }
}

impl<T: RefGroupable + std::fmt::Debug> std::fmt::Debug for IRef<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let group = self.group.borrow();
        write!(f, "IRef<{}[{}]>({:?})", group.index(), self.index, self.get())
    }
}

impl<T: RefGroupable + std::fmt::Display> std::fmt::Display for IRef<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.get())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct MakeGroupable<T: Sized + Eq>(pub T);

impl<T: Sized + Eq> RefGroupable for MakeGroupable<T> {
    fn add_group(&self, _group: &RefGroup<Self>) {}
    fn del_group(&self, _group: &RefGroup<Self>) {}

    fn copy_if_needed(&self, _group: &Rc<RefCell<RefGroup<Self>>>, _elements: &mut Vec<IRef<Self>>) -> Option<Rc<Self>> {
        None
    }
}

impl<T: Sized + Eq> From<T> for MakeGroupable<T> {
    fn from(value: T) -> Self {
        Self(value)
    }
}

impl<T: Sized + Eq> std::fmt::Display for MakeGroupable<T> where T: std::fmt::Display {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0, f)
    }
}