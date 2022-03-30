pub mod tok;

use std::{
    io::BufRead,
    iter,
};
use self::tok::Token;

pub struct Cursor<'a>(pub &'a str);

impl<'a> Cursor<'a> {
    pub fn advance(&mut self) -> Option<Token> {
        None
    }
}

pub fn tokenize(input: &str) -> impl Iterator<Item = Token> + '_ {
    // define a cursor type that tracks position in the input
    let mut cursor = Cursor(input);

    iter::from_fn(move || {
        cursor.advance()
    })
}
