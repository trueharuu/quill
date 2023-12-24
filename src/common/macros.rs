#[macro_export]
macro_rules! assume {
    ($pat:pat = $i:expr => { $($t:tt)*} ) => {
        if let $pat = $i { $($t)* } else { unreachable!() }
    };

    (Option $pat:pat = $i:expr => { $($t:tt)*} ) => {
        if let $pat = $i { Some($($t)*) } else { None }
    };


}
