use std::{cell::RefCell, collections::BTreeMap, fmt::Display, rc::Rc};

use crate::{parse::expr::Expr, spans::span::Spanned};

use super::value::Value;

#[derive(Clone)]
pub struct Env {
    pub values: BTreeMap<String, Rc<RefCell<Value>>>,
    pub last_use: BTreeMap<String, Spanned<Expr>>,
    pub parent: Option<Rc<RefCell<Self>>>,
}

impl Env {
    #[must_use]
    pub const fn new() -> Self {
        Self::with_parent(None)
    }

    #[must_use]
    pub const fn with_parent(parent: Option<Rc<RefCell<Self>>>) -> Self {
        Self {
            values: BTreeMap::new(),
            last_use: BTreeMap::new(),
            parent,
        }
    }

    #[must_use]
    pub fn get(&self, label: impl Display) -> Option<Rc<RefCell<Value>>> {
        self.values.get(&label.to_string()).map_or_else(
            || self.parent.as_ref().and_then(|p| p.borrow().get(label)),
            |v| Some(v.clone()),
        )
    }

    #[must_use]
    pub fn remove(&mut self, label: String) -> Option<Rc<RefCell<Value>>> {
        self.values.remove(&label).map_or_else(
            || {
                self.parent
                    .as_ref()
                    .and_then(|p| p.borrow_mut().remove(label))
            },
            Some,
        )
    }

    #[must_use]
    pub fn define(
        &mut self,
        label: impl Display,
        value: Rc<RefCell<Value>>,
    ) -> Option<Rc<RefCell<Value>>> {
        self.values.insert(label.to_string(), value)
    }
}
