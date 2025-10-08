use core::panic;
use std::sync::atomic::AtomicUsize;
use std::rc::Rc;
use std::cell::{RefCell, UnsafeCell};
use std::hash::{Hash, Hasher};
use std::fmt::{Debug, Display, Formatter, Result};
use std::ops::Deref;

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

    fn copy_if_needed(&self, _group: &Rc<RefCell<RefGroup<Self>>>, _children: &mut Vec<IRef<Self>>) -> Option<IRef<Self>> {
        None
    }
}

type CellRc<T> = RefCell<Rc<T>>;

pub struct RefGroup<T: RefGroupable> {
    members: Vec<CellRc<T>>,
    index: GroupIndex
}

impl<T: RefGroupable> RefGroup<T> {
    pub fn new() -> IRefGroup<T> {
        Rc::new(RefCell::new(Self {
            members: vec![],
            index: GroupIndex::new(),
        }))
    }
    
    pub fn index(&self) -> usize {
        self.index.index
    }
    
    pub fn get_from_index(&self, index: usize) -> Option<Rc<T>> {
        if index >= self.members.len() {
            return None;
        }
        
        Some(self.members[index].borrow().clone())
    }
    
    pub fn get(&self, element: &T) -> Option<usize> {
        self.members.iter().position(|e| e.borrow().as_ref().eq(element))
    }
    
    pub fn get(&self, element: &Rc<T>) -> Option<usize> {
        self.members.iter().position(|e| e.borrow().as_ref().eq(element.as_ref()))
    }
    
    pub fn add(&mut self, element: &Rc<T>) -> usize {
        if !self.contains(element) {
            self.members.push(RefCell::new(element.clone()));
            element.add_group(self);
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

pub enum IRef<T: RefGroupable> {
    Indirect(IRefGroup<T>, usize),
    Direct(UnsafeCell<Rc<T>>),
}

impl<T: RefGroupable> IRef<T> {
    pub fn new(element: Rc<T>, group: IRefGroup<T>) -> Self {
        let index = unsafe { group.as_ptr().as_mut().unwrap().add(&element) };
        Self::Indirect(group, index)
    }
    
    pub fn from_group(group: IRefGroup<T>, index: usize) -> Self {
        if index >= group.borrow().members.len() {
            panic!("Index {} out of bounds for group of size {}", index, group.borrow().members.len());
        }
        Self::Indirect(group, index)
    }
    
    pub fn new_decoupled(element: Rc<T>) -> Self {
        Self::Direct(UnsafeCell::new(element))
    }
    
    pub fn is_decoupled(&self) -> bool {
        matches!(self, IRef::Direct(_))
    }
    
    pub fn decouple(&mut self) {
        if let IRef::Indirect(group, index) = self {
            let element = group.borrow().members[*index].borrow().clone();
            *self = IRef::Direct(UnsafeCell::new(element));
        }
    }
    
    pub fn group(&self) -> Option<IRefGroup<T>> {
        match self {
            IRef::Indirect(group, _) => Some(group.clone()),
            IRef::Direct(_) => None,
        }
    }
    
    pub fn same_group(&self, other: &Self) -> bool {
        match (self, other) {
            (IRef::Indirect(g1, _), IRef::Indirect(g2, _)) => g1.borrow().index() == g2.borrow().index(),
            (IRef::Direct(_), IRef::Direct(_)) => true,
            _ => false,
        }
    }
    
    pub fn get(&self) -> &T {
        match self {
            IRef::Indirect(group, index) => unsafe {
                (*group.borrow().members[*index].as_ptr()).as_ref()
            },
            IRef::Direct(cell) => unsafe {
                cell.get().as_ref().unwrap()
            }
        }
    }
    
    pub fn get_shared(&self) -> &Rc<T> {
        match self {
            IRef::Indirect(group, index) => unsafe {
                &*group.borrow().members[*index].as_ptr()
            },
            IRef::Direct(cell) => unsafe {
                &*cell.get()
            },
        }
    }
    
    pub fn redirect(&self, new_element: IRef<T>) {
        match self {
            IRef::Indirect(group, index) => {
                match new_element {
                    IRef::Indirect(new_group, new_index) => {
                        if group.borrow().index() != new_group.borrow().index() {
                            panic!(
                                "Cannot redirect IRef in group {} to IRef in different group {}",
                                group.borrow().index(), new_group.borrow().index()
                            );
                        }
                        
                        group.borrow().redirect(*index, unsafe {
                            &*new_group.borrow().members[new_index].as_ptr()
                        });
                    }
                    IRef::Direct(_) => {
                        panic!(
                            "Cannot redirect IRef in group {} to decoupled IRef",
                            group.borrow().index()
                        );
                    }
                }
            }
            IRef::Direct(element) => {
                let _ = std::mem::replace(unsafe { &mut *element.get() }, new_element.get_shared().clone());
            }
        }
    }
    
    pub fn shallow_copy_to(&self, other: &IRefGroup<T>) -> Option<IRef<T>> {
        match self {
            IRef::Indirect(group, _) => {
                if group.borrow().index() == other.borrow().index() {
                    return None;
                }
            }
            IRef::Direct(_) => {}
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
            if !self.same_group(&item) {
                panic!(
                    "copy_if_needed violated contract: returned item in group {} but expected group {}",
                    item.group().unwrap().borrow().index(), other.borrow().index()
                );
            }
            
            let copied = item.get_shared()
                .copy_if_needed(other, &mut to_copy)
                .unwrap_or(item.clone());

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
        match self {
            IRef::Indirect(group, index) => IRef::Indirect(group.clone(), *index),
            IRef::Direct(cell) => IRef::Direct(UnsafeCell::new(unsafe { (*cell.get()).clone() })),
        }
    }
}

impl<T: RefGroupable> PartialEq for IRef<T> {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref().eq(other.as_ref())
    }
}

impl<T: RefGroupable> Eq for IRef<T> {}

impl<T: RefGroupable + std::hash::Hash> std::hash::Hash for IRef<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.get_shared().hash(state);
    }
}

impl<T: RefGroupable + Debug> Debug for IRef<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            IRef::Indirect(group, index) => write!(f, "IRef<{}[{}]>({:?})", group.borrow().index(), index, self.get()),
            IRef::Direct(cell) => write!(f, "IRef<Decoupled>({:?})", unsafe { &*cell.get() }),
        }
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