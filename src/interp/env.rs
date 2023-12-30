use std::{collections::BTreeMap, fmt::Display};

use crate::{parse::expr::Expr, rc, spans::span::Spanned};

use super::value::Value;

#[derive(Clone, Debug)]
pub struct Env {
    pub values: BTreeMap<String, rc!(ty Value)>,
    pub last_use: BTreeMap<String, Spanned<Expr>>,
    pub local_self: Option<rc!(ty Value)>,

    pub parent: Option<rc!(ty Self)>,
}

impl Env {
    #[must_use]
    pub const fn new() -> Self {
        Self::with_parent(None)
    }

    #[must_use]
    pub const fn with_parent(parent: Option<rc!(ty Self)>) -> Self {
        Self {
            values: BTreeMap::new(),
            last_use: BTreeMap::new(),
            local_self: None,

            parent,
        }
    }

    #[must_use]
    pub fn get(&self, label: impl Display) -> Option<rc!(ty Value)> {
        self.values.get(&label.to_string()).map_or_else(
            || self.parent.as_ref().and_then(|p| p.borrow().get(label)),
            |v| Some(v.clone()),
        )
    }

    #[must_use]
    pub fn remove(&mut self, label: String) -> Option<rc!(ty Value)> {
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
    pub fn define(&mut self, label: impl Display, value: rc!(ty Value)) -> Option<rc!(ty Value)> {
        self.values.insert(label.to_string(), value)
    }

    #[must_use]
    pub fn reassign(&mut self, label: impl Display, value: rc!(ty Value)) -> Option<()> {
        if self.values.get(&label.to_string()).is_some() {
            self.values.insert(label.to_string(), value);
            Some(())
        } else if let Some(p) = &self.parent {
            let _ = p.borrow_mut().reassign(label, value);
            Some(())
        } else {
            None
        }
    }

    #[must_use]
    pub fn put_self(&mut self, value: rc!(ty Value)) -> Option<rc!(ty Value)> {
        self.local_self.replace(value)
    }

    #[must_use]
    pub const fn get_self(&self) -> Option<&rc!(ty Value)> {
        self.local_self.as_ref()
    }
}
