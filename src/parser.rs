use crate::{
    ast::{Computation, Expr, Factor, FactorOp, FuncCall, Relation, Term, TermOp},
    scanner::TokenResult,
    tok::{RelOp, Token},
};

pub type ParseResult<T> = Result<T, ()>;

pub struct Parser<T: Iterator<Item = TokenResult>> {
    current: Option<TokenResult>,
    stream: T,
}

impl<T: Iterator<Item = TokenResult>> Parser<T> {
    pub fn new(mut stream: T) -> Self {
        Parser {
            current: stream.next(),
            stream,
        }
    }

    pub fn peek(&self) -> Option<&TokenResult> {
        self.current.as_ref()
    }

    pub fn parse_computation(&mut self) -> ParseResult<Computation> {
        todo!()
    }

    pub fn parse_expr(&mut self) -> ParseResult<Expr> {
        let root = self.parse_term()?;
        let mut ops = vec![];

        while let Some(op) = self.consume_termop_if_exists() {
            let next = self.parse_term()?;
            ops.push((op, next))
        }

        Ok(Expr { root, ops })
    }

    pub fn parse_factor(&mut self) -> ParseResult<Factor> {
        if self.expect_punctuation_matching('(') {
            let subexpr = Box::new(self.parse_expr()?);

            if self.expect_punctuation_matching(')') {
                Ok(Factor::SubExpr(subexpr))
            } else {
                Err(())
            }
        } else if let Some(n) = self.consume_number_if_exists() {
            Ok(Factor::Number(n))
        } else {
            match self.consume_ident_if_exists() {
                Some(call_keyword) if call_keyword == "call" => Ok(Factor::Call(self.parse_func_call()?)),
                Some(ident) => Ok(Factor::VarRef(ident)),
                None => Err(()),
            }
        }
    }

    pub fn parse_func_call(&mut self) -> ParseResult<FuncCall> {
        todo!()
    }

    pub fn parse_relation(&mut self) -> ParseResult<Relation> {
        let lhs = self.parse_expr()?;
        let op = self.expect_relop()?;
        let rhs = self.parse_expr()?;

        Ok(Relation { lhs, rhs, op })
    }

    pub fn parse_term(&mut self) -> ParseResult<Term> {
        let root = self.parse_factor()?;
        let mut ops = vec![];

        while let Some(op) = self.consume_factorop_if_exists() {
            let next = self.parse_factor()?;
            ops.push((op, next))
        }

        Ok(Term { root, ops })
    }

    fn advance(&mut self) -> Option<TokenResult> {
        let prev = self.current.take();
        self.current = self.stream.next();

        prev
    }

    pub fn expect_relop(&mut self) -> ParseResult<RelOp> {
        match self.current {
            Some(Ok(Token::RelOp(op))) => {
                self.advance();
                Ok(op)
            }

            _ => Err(()),
        }
    }

    pub fn expect_punctuation_matching(&mut self, c: char) -> bool {
        match self.current {
            Some(Ok(Token::Punctuation(ch))) if ch == c => {
                self.advance();
                true
            }

            _ => false,
        }
    }

    pub fn consume_number_if_exists(&mut self) -> Option<u32> {
        match self.current {
            Some(Ok(Token::Number(n))) => {
                self.advance();
                Some(n)
            },
            _ => None,
        }
    }

    pub fn consume_ident_if_exists(&mut self) -> Option<String> {
        match self.current {
            Some(Ok(Token::Ident(_))) => match self.advance() {
                Some(Ok(Token::Ident(ident))) => Some(ident),
                _ => unreachable!(),
            },
            _ => None,
        }
    }
    pub fn consume_termop_if_exists(&mut self) -> Option<TermOp> {
        match self.current {
            Some(Ok(Token::Punctuation('+'))) => {
                self.advance();
                Some(TermOp::Add)
            }

            Some(Ok(Token::Punctuation('-'))) => {
                self.advance();
                Some(TermOp::Sub)
            }

            _ => None,
        }
    }

    pub fn consume_factorop_if_exists(&mut self) -> Option<FactorOp> {
        match self.current {
            Some(Ok(Token::Punctuation('*'))) => {
                self.advance();
                Some(FactorOp::Mul)
            }

            Some(Ok(Token::Punctuation('/'))) => {
                self.advance();
                Some(FactorOp::Div)
            }

            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tok::Token;

    #[test]
    fn expr_parse_single_term() {
        let n = 5;
        let tokens = vec![Ok(Token::Number(n))];
        let mut parser = Parser::new(tokens.into_iter());

        assert_eq!(parser.parse_expr(), Ok(Expr {
            root: Term {
                root: Factor::Number(n),
                ops: vec![],
            },
            ops: vec![],
        }));
    }

    #[test]
    fn expr_parse_two_terms() {
        let n = 5;
        let m = 6;
        let op = TermOp::Add;
        let tokens = vec![
            Ok(Token::Number(n)),
            Ok(Token::Punctuation(op.into())),
            Ok(Token::Number(m)),
        ];
        let mut parser = Parser::new(tokens.into_iter());

        assert_eq!(parser.parse_expr(), Ok(Expr {
            root: Term {
                root: Factor::Number(n),
                ops: vec![],
            },
            ops: vec![
                (op, Term {
                    root: Factor::Number(m),
                    ops: vec![],
                }),
            ],
        }));
    }

    #[test]
    fn expr_parse_many_terms() {
        let x = 5;
        let y = 6;
        let z = 7;
        let op1 = TermOp::Add;
        let op2 = TermOp::Sub;
        let tokens = vec![
            Ok(Token::Number(x)),
            Ok(Token::Punctuation(op1.into())),
            Ok(Token::Number(y)),
            Ok(Token::Punctuation(op2.into())),
            Ok(Token::Number(z)),
        ];
        let mut parser = Parser::new(tokens.into_iter());

        assert_eq!(parser.parse_expr(), Ok(Expr {
            root: Term {
                root: Factor::Number(x),
                ops: vec![],
            },
            ops: vec![
                (op1, Term {
                    root: Factor::Number(y),
                    ops: vec![],
                }),
                (op2, Term {
                    root: Factor::Number(z),
                    ops: vec![],
                }),
            ],
        }));
    }
}
