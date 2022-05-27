#[macro_export]
macro_rules! define_e2e_test {
    ($name:ident, $($flag:expr),*) => {
        #[test]
        fn $name() {
            $crate::common::TestRun::start(
                make_test_name!($name),
                uuid::Uuid::new_v4(), 
                make_flags!($($flag),*),
            ).run();
        }
    }
}

#[macro_export]
macro_rules! make_flags {
    () => (None);
    ($($flag:expr),+) => (Some(vec![$($flag),+]));
}

#[macro_export]
macro_rules! make_test_name {
    ($name:ident) => (concat!("tests/rsrc/", stringify!($name), ".tiny"));
}
