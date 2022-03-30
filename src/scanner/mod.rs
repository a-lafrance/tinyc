pub mod tok;

use self::tok::{RelOp, Token};
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
    iter,
    str::Chars,
};

pub struct Cursor<'a> {
    input: Chars<'a>,
}

impl<'a> Cursor<'a> {
    pub fn new(input: &'a str) -> Cursor<'a> {
        Cursor {
            input: input.chars(),
        }
    }

    fn peek(&self) -> Option<char> {
        self.input.clone().next()
    }

    fn relop_if_eq_sign(&mut self, base: char, op: RelOp) -> Result<Token, InvalidCharError> {
        match self.consume_next_if(|c| c == '=') {
            Some(_) => Ok(Token::RelOp(op)),
            None => Err(InvalidCharError(base)),
        }
    }

    fn consume_next_if(&mut self, predicate: impl FnOnce(char) -> bool) -> Option<char> {
        if predicate(self.peek()?) {
            Some(self.input.next().unwrap()) // unwrap because the peek check implies that it exists
        } else {
            None
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(_) = self.consume_next_if(|c| c.is_ascii_whitespace()) { }
    }

    pub fn advance(&mut self) -> Option<Result<Token, InvalidCharError>> {
        self.skip_whitespace();
        let first = self.peek()?;

        let tok = self.read_ident().map(|tok| Ok(tok))
            .or(self.read_number())
            .unwrap_or_else(move || {
                if first == '>' {
                    let tok = if self.consume_next_if(|c| c == '=').is_some() {
                        Token::RelOp(RelOp::Ge)
                    } else {
                        Token::RelOp(RelOp::Gt)
                    };

                    Ok(tok)
                } else if first == '<' {
                    let tok = if self.consume_next_if(|c| c == '=').is_some() {
                        Token::RelOp(RelOp::Le)
                    } else if self.consume_next_if(|c| c == '-').is_some() {
                        Token::AssignOp
                    } else {
                        Token::RelOp(RelOp::Lt)
                    };

                    Ok(tok)
                } else if first == '=' {
                    self.relop_if_eq_sign(first, RelOp::Eq)
                } else if first == '!' {
                    self.relop_if_eq_sign(first, RelOp::Ne)
                } else if first == '+'
                    || first == '-'
                    || first == '*'
                    || first == '/'
                    || first == '('
                    || first == ')'
                    || first == ','
                    || first == '{'
                    || first == '}'
                    || first == ';'
                    || first == '.'
                {
                    // punctuation
                    Ok(Token::Punctuation(first))
                } else {
                    // invalid
                    Err(InvalidCharError(first))
                }
            });

        Some(tok)
    }

    fn read_ident(&mut self) -> Option<Token> {
        // read letters and digits
        // if you hit anything else, end the token and return
        let mut ident = String::new();

        while let Some(c) = self.consume_next_if(|c| c.is_ascii_lowercase() || c.is_ascii_digit()) {
            ident.push(c);
        }

        if ident.is_empty() {
            None
        } else {
            Some(Token::Ident(ident))
        }
    }

    fn read_number(&mut self) -> Option<Result<Token, InvalidCharError>> {
        // read digits
        // if you hit a letter, error
        // if you hit anything else, end the token and return
        let mut n = 0;
        let mut found_digit = false;

        while let Some(d) = self.consume_next_if(|c| c.is_ascii_digit()).map(|c| c.to_digit(10)).flatten() {
            n = 10 * n + d;
            found_digit = true;
        }

        if found_digit {
            // check if you hit a letter
            let tok = Ok(Token::Number(n));
            let res = if let Some(end) = self.peek() {
                if end.is_ascii_lowercase() {
                    Err(InvalidCharError(end))
                } else {
                    tok
                }
            } else {
                tok
            };

            Some(res)
        } else {
            None
        }
    }
}


#[derive(Debug)]
pub struct InvalidCharError(char);

impl Display for InvalidCharError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "error: invalid character '{}'", self.0)
    }
}

impl Error for InvalidCharError { }


pub fn tokenize(input: &str) -> Result<Vec<Token>, InvalidCharError> {
    let mut cursor = Cursor::new(input);
    iter::from_fn(move || cursor.advance()).try_collect()
}
