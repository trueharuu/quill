use yansi::Paint;

pub trait Painted {
    fn painted(&self) -> Paint<&Self> { Paint::new(self) }
}

impl<T> Painted for T {}