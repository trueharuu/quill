pub trait Convert {
    fn to<T>(self) -> T
    where
        Self: Into<T>,
    {
        Into::into(self)
    }

    fn of<T>(value: T) -> Self
    where
        Self: From<T>,
    {
        From::from(value)
    }

    fn try_to<T>(self) -> Result<T, Self::Error>
    where
        Self: TryInto<T>,
    {
        TryInto::try_into(self)
    }

    fn try_of<T>(value: T) -> Result<Self, Self::Error>
    where
        Self: TryFrom<T>,
    {
        TryFrom::try_from(value)
    }
}

impl<T> Convert for T {}
