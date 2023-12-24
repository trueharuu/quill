pub trait Vox
where
    Self: Sized,
{
    fn vox(self) -> Box<Self> {
        Box::new(self)
    }
}

impl<T> Vox for T {}
