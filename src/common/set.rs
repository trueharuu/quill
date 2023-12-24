#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Set<T>(pub(crate) Vec<T>);

impl<T> Set<T> {
    #[must_use]
    pub const fn empty() -> Self {
        Self(Vec::new())
    }

    pub fn insert(&mut self, value: T) -> &mut Self
    where
        T: PartialEq,
    {
        if !self.0.contains(&value) {
            self.0.push(value);
        }

        self
    }
}
