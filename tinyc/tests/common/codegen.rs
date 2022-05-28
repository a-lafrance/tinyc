#[macro_export]
macro_rules! define_e2e_test {
    ($name:ident) => {
        define_e2e_test!($name,);
    };

    ($name:ident, $($flag:expr),*) => {
        #[test]
        fn $name() {
            let (src_file, input_file, output_file) = make_test_file_names!($name);

            $crate::common::TestRun::start(
                src_file,
                uuid::Uuid::new_v4(),
                make_flags!($($flag),*),
            ).run(input_file, output_file);
        }
    };
}

#[macro_export]
macro_rules! make_flags {
    () => (None);
    ($($flag:expr),+) => (Some(vec![$($flag),+]));
}

#[macro_export]
macro_rules! make_test_file_names {
    ($name:ident) => ((
        concat!("tests/rsrc/", stringify!($name), ".tiny"),
        concat!("tests/rsrc/", stringify!($name), ".in"),
        concat!("tests/rsrc/", stringify!($name), ".out"),
    ));
}
