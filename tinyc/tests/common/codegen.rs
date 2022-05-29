#[macro_export]
macro_rules! define_e2e_test {
    ($name:ident) => {
        define_e2e_test!($name,);
    };

    ($name:ident, $($flag:expr),*) => {
        #[test]
        fn $name() {
            $crate::common::run_e2e_test(
                uuid::Uuid::new_v4(),
                concat!("tests/rsrc/", stringify!($name)),
                make_flags!($($flag),*),
            );
        }
    };
}

#[macro_export]
macro_rules! make_flags {
    () => (None);
    ($($flag:expr),+) => (Some(vec![$($flag),+]));
}
