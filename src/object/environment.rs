use crate::object::object::Object;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub struct Environment {
    store: HashMap<Rc<String>, Rc<Object>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::default(),
        }
    }

    pub fn get(&self, name: Rc<String>) -> Option<&Rc<Object>> {
        self.store.get(name.as_ref())
    }

    pub fn set(&mut self, name: &Rc<String>, val: Rc<Object>) {
        self.store.insert(name.clone(), val);
    }
}
