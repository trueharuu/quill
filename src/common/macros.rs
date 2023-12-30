#[macro_export]
macro_rules! assume {
    ($pat:pat = $i:expr => { $($t:tt)*} ) => {
        if let $pat = $i { $($t)* } else { unreachable!() }
    };

    (Option $pat:pat = $i:expr => { $($t:tt)*} ) => {
        if let $pat = $i { Some($($t)*) } else { None }
    };
}

#[macro_export]
macro_rules! rc {
    ($t:expr) => {
        ::std::rc::Rc::new(::std::cell::RefCell::new($t))
    };
    (ty $t:ty) => { ::std::rc::Rc<::std::cell::RefCell<$t>> };
    () => {
        ::std::rc::Rc::new(::std::cell::RefCell::new(Value::None))
    };
}

#[macro_export]
macro_rules! list {
    ($t:ty) => {
        ::std::boxed::Box<[$t]>
    };
}