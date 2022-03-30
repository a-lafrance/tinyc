mod driver;

use std::env;

fn main() {
    driver::start(env::args());
}
