use crate::tok::{RelOp, Token};
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
    iter,
    str::Chars,
};

pub type TokenResult = Result<Token, InvalidCharError>;

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
        while self.consume_next_if(|c| c.is_ascii_whitespace()).is_some() {}
    }

    pub fn advance(&mut self) -> Option<TokenResult> {
        self.skip_whitespace();
        let first = self.peek()?;

        let tok = self
            .read_number()
            .or_else(|| self.read_ident().map(Ok))
            .unwrap_or_else(move || {
                if self.consume_next_if(|c| c == '>').is_some() {
                    let tok = if self.consume_next_if(|c| c == '=').is_some() {
                        Token::RelOp(RelOp::Ge)
                    } else {
                        Token::RelOp(RelOp::Gt)
                    };

                    Ok(tok)
                } else if self.consume_next_if(|c| c == '<').is_some() {
                    let tok = if self.consume_next_if(|c| c == '=').is_some() {
                        Token::RelOp(RelOp::Le)
                    } else if self.consume_next_if(|c| c == '-').is_some() {
                        Token::AssignOp
                    } else {
                        Token::RelOp(RelOp::Lt)
                    };

                    Ok(tok)
                } else if self.consume_next_if(|c| c == '=').is_some() {
                    self.relop_if_eq_sign(first, RelOp::Eq)
                } else if self.consume_next_if(|c| c == '!').is_some() {
                    self.relop_if_eq_sign(first, RelOp::Ne)
                } else if let Some(c) = self.consume_next_if(|c| {
                    c == '+'
                        || c == '-'
                        || c == '*'
                        || c == '/'
                        || c == '('
                        || c == ')'
                        || c == ','
                        || c == '{'
                        || c == '}'
                        || c == ';'
                        || c == '.'
                }) {
                    // punctuation
                    Ok(Token::Punctuation(c))
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

    fn read_number(&mut self) -> Option<TokenResult> {
        // read digits
        // if you hit a letter, error
        // if you hit anything else, end the token and return
        let mut n = 0;
        let mut found_digit = false;

        while let Some(d) = self
            .consume_next_if(|c| c.is_ascii_digit())
            .and_then(|c| c.to_digit(10))
        {
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

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct InvalidCharError(char);

impl Display for InvalidCharError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "error: invalid character '{}'", self.0)
    }
}

impl Error for InvalidCharError {}

pub fn tokenize(input: &str) -> impl Iterator<Item = TokenResult> + '_ {
    let mut cursor = Cursor::new(input);

    iter::from_fn(move || cursor.advance())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scanner_sanity_check() {
        let input = "
            main
            var a, b; {
                let a <- 1 * 2 + 3;

                if a > 0
                then
                    let b <- a * 2;
                else
                    let b <- a / 2 - 1;
                fi;
            }.
        ";

        assert_eq!(
            tokenize(input).collect::<Result<Vec<_>, _>>(),
            Ok(vec![
                Token::Ident("main".to_string()),
                Token::Ident("var".to_string()),
                Token::Ident("a".to_string()),
                Token::Punctuation(','),
                Token::Ident("b".to_string()),
                Token::Punctuation(';'),
                Token::Punctuation('{'),
                Token::Ident("let".to_string()),
                Token::Ident("a".to_string()),
                Token::AssignOp,
                Token::Number(1),
                Token::Punctuation('*'),
                Token::Number(2),
                Token::Punctuation('+'),
                Token::Number(3),
                Token::Punctuation(';'),
                Token::Ident("if".to_string()),
                Token::Ident("a".to_string()),
                Token::RelOp(RelOp::Gt),
                Token::Number(0),
                Token::Ident("then".to_string()),
                Token::Ident("let".to_string()),
                Token::Ident("b".to_string()),
                Token::AssignOp,
                Token::Ident("a".to_string()),
                Token::Punctuation('*'),
                Token::Number(2),
                Token::Punctuation(';'),
                Token::Ident("else".to_string()),
                Token::Ident("let".to_string()),
                Token::Ident("b".to_string()),
                Token::AssignOp,
                Token::Ident("a".to_string()),
                Token::Punctuation('/'),
                Token::Number(2),
                Token::Punctuation('-'),
                Token::Number(1),
                Token::Punctuation(';'),
                Token::Ident("fi".to_string()),
                Token::Punctuation(';'),
                Token::Punctuation('}'),
                Token::Punctuation('.'),
            ])
        );
    }
}
