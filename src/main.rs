use std::env;
use tinyc::driver;

fn main() {
    driver::start(env::args());
}
