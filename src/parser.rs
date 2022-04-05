use crate::{
    ast::{Assignment, Block, Computation, Expr, Factor, FactorOp, FuncCall, IfStmt, Loop, Relation, Return, Stmt, Term, TermOp},
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

    pub fn parse_assignment(&mut self) -> ParseResult<Assignment> {
        if self.expect_keyword("let") {
            let place = self.consume_ident_if_exists().ok_or(())?;

            if self.expect_assign_op() {
                let value = self.parse_expr()?;
                Ok(Assignment { place, value })
            } else {
                Err(())
            }
        } else {
            Err(())
        }
    }

    pub fn parse_block(&mut self) -> ParseResult<Block> {
        let mut body = vec![self.parse_stmt()?];

        while self.expect_punctuation_matching(';') {
            // FIXME: this doesn't feel robust enough
            if let Ok(stmt) = self.parse_stmt() {
                body.push(stmt);
            }
        }

        Ok(Block { body })
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
            match self.peek_ident_if_exists() {
                Some(call_keyword) if call_keyword == "call" => Ok(Factor::Call(self.parse_func_call()?)),
                Some(_) => Ok(Factor::VarRef(self.consume_ident_if_exists().unwrap())),
                None => Err(()),
            }
        }
    }

    pub fn parse_func_call(&mut self) -> ParseResult<FuncCall> {
        if self.expect_keyword("call") {
            let name = self.consume_ident_if_exists().ok_or(())?;
            let mut args = vec![];

            if self.expect_punctuation_matching('(') {
                if !self.expect_punctuation_matching(')') {
                    // parse first arg
                    args.push(self.parse_expr()?);

                    while !self.expect_punctuation_matching(')') {
                        self.expect_punctuation_matching(',');
                        args.push(self.parse_expr()?);
                    }
                }
            }

            Ok(FuncCall { name, args })
        } else {
            Err(())
        }
    }

    pub fn parse_if_stmt(&mut self) -> ParseResult<IfStmt> {
        self.expect_keyword_or_err("if")?;
        let condition = self.parse_relation()?;

        self.expect_keyword_or_err("then")?;
        let then_block = self.parse_block()?;

        let else_block = if self.expect_keyword("else") {
            Some(self.parse_block()?)
        } else {
            None
        };

        self.expect_keyword_or_err("fi")?;
        Ok(IfStmt { condition, then_block, else_block })
    }

    pub fn parse_loop(&mut self) -> ParseResult<Loop> {
        self.expect_keyword_or_err("while")?;
        let condition = self.parse_relation()?;

        self.expect_keyword_or_err("do")?;
        let body = self.parse_block()?;

        self.expect_keyword_or_err("od")?;
        Ok(Loop { condition, body })
    }

    pub fn parse_relation(&mut self) -> ParseResult<Relation> {
        let lhs = self.parse_expr()?;
        let op = self.expect_relop()?;
        let rhs = self.parse_expr()?;

        Ok(Relation { lhs, rhs, op })
    }

    pub fn parse_return(&mut self) -> ParseResult<Return> {
        self.expect_keyword_or_err("return")?;
        let value = self.parse_expr().ok(); // FIXME: this is probably very bad (lookahead?)

        Ok(Return { value })
    }

    pub fn parse_stmt(&mut self) -> ParseResult<Stmt> {
        match self.peek_ident_if_exists() {
            Some("let") => self.parse_assignment().map(|a| a.into()),
            Some("call") => self.parse_func_call().map(|c| c.into()),
            Some("if") => self.parse_if_stmt().map(|i| i.into()),
            Some("while") => self.parse_loop().map(|l| l.into()),
            Some("return") => self.parse_return().map(|r| r.into()),
            _ => Err(()),
        }
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

    pub fn expect_assign_op(&mut self) -> bool {
        match self.current {
            Some(Ok(Token::AssignOp)) => {
                self.advance();
                true
            },
            _ => false,
        }
    }

    pub fn expect_keyword<K: AsRef<str>>(&mut self, keyword: K) -> bool {
        match self.peek_ident_if_exists() {
            Some(ident) if ident == keyword.as_ref() => {
                self.advance();
                true
            },
            _ => false,
        }
    }

    pub fn expect_keyword_or_err<K: AsRef<str>>(&mut self, keyword: K) -> ParseResult<()> {
        if self.expect_keyword(keyword) {
            Ok(())
        } else {
            Err(())
        }
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

    pub fn peek_ident_if_exists(&self) -> Option<&str> {
        match self.current {
            Some(Ok(Token::Ident(ref ident))) => Some(ident),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tok::Token;

    fn stream_from_tokens(tokens: Vec<Token>) -> impl Iterator<Item = TokenResult> {
        tokens.into_iter().map(|tok| Ok(tok))
    }

    #[test]
    fn parse_assignment_simple() {
        // asg = 123
        let var = "asg".to_string();
        let val = 123;
        let tokens = stream_from_tokens(vec![
            Token::Ident("let".to_string()),
            Token::Ident(var.clone()),
            Token::AssignOp,
            Token::Number(val),
        ]);
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_assignment(), Ok(Assignment {
            place: var,
            value: Expr {
                root: Term {
                    root: Factor::Number(val),
                    ops: vec![],
                },
                ops: vec![],
            },
        }));
    }

    #[test]
    fn parse_assignment_complex() {
        // result = a * 2 + b
        let tokens = stream_from_tokens(vec![
            Token::Ident("let".to_string()),
            Token::Ident("result".to_string()),
            Token::AssignOp,
            Token::Ident("a".to_string()),
            Token::Punctuation('*'),
            Token::Number(2),
            Token::Punctuation('+'),
            Token::Ident("b".to_string()),
        ]);
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_assignment(), Ok(Assignment {
            place: "result".to_string(),
            value: Expr {
                root: Term {
                    root: Factor::VarRef("a".to_string()),
                    ops: vec![(FactorOp::Mul, Factor::Number(2))],
                },
                ops: vec![(TermOp::Add, Term {
                    root: Factor::VarRef("b".to_string()),
                    ops: vec![],
                })],
            },
        }));
    }

    #[test]
    fn parse_expr_single_term() {
        let n = 5;
        let tokens = stream_from_tokens(vec![Token::Number(n)]);
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_expr(), Ok(Expr {
            root: Term {
                root: Factor::Number(n),
                ops: vec![],
            },
            ops: vec![],
        }));
    }

    #[test]
    fn parse_expr_two_terms() {
        let n = 5;
        let m = 6;
        let op = TermOp::Add;
        let tokens = stream_from_tokens(vec![
            Token::Number(n),
            Token::Punctuation(op.into()),
            Token::Number(m),
        ]);
        let mut parser = Parser::new(tokens);

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
    fn parse_expr_many_terms() {
        let x = 5;
        let y = 6;
        let z = 7;
        let op1 = TermOp::Add;
        let op2 = TermOp::Sub;
        let tokens = stream_from_tokens(vec![
            Token::Number(x),
            Token::Punctuation(op1.into()),
            Token::Number(y),
            Token::Punctuation(op2.into()),
            Token::Number(z),
        ]);
        let mut parser = Parser::new(tokens);

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

    #[test]
    fn parse_factor_var_ref() {
        let ident = "asg".to_string();
        let tokens = stream_from_tokens(vec![Token::Ident(ident.clone())]);
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_factor(), Ok(Factor::VarRef(ident)));
    }

    #[test]
    fn parse_factor_number() {
        let n = 5;
        let tokens = stream_from_tokens(vec![Token::Number(n)]);
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_factor(), Ok(Factor::Number(n)));
    }

    #[test]
    fn parse_factor_subexpr() {
        let n = 5;
        let tokens = stream_from_tokens(vec![
            Token::Punctuation('('),
            Token::Number(n),
            Token::Punctuation(')'),
        ]);
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_factor(), Ok(Factor::SubExpr(
            Box::new(Expr {
                root: Term {
                    root: Factor::Number(n),
                    ops: vec![],
                },
                ops: vec![],
            })
        )));
    }

    #[test]
    fn parse_factor_func_call() {
        let func = "double".to_string();
        let arg = 5;
        let tokens = stream_from_tokens(vec![
            Token::Ident("call".to_string()),
            Token::Ident(func.clone()),
            Token::Punctuation('('),
            Token::Number(arg),
            Token::Punctuation(')'),
        ]);
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_factor(), Ok(Factor::Call(
            FuncCall {
                name: func,
                args: vec![Expr {
                    root: Term {
                        root: Factor::Number(arg),
                        ops: vec![],
                    },
                    ops: vec![],
                }],
            }
        )));
    }

    #[test]
    fn parse_func_call_no_args_no_parens() {
        // call run
        let func = "run".to_string();
        let tokens = stream_from_tokens(vec![
            Token::Ident("call".to_string()),
            Token::Ident(func.clone()),
        ]);
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_func_call(), Ok(FuncCall {
            name: func,
            args: vec![],
        }));
    }

    #[test]
    fn parse_func_call_no_args_with_parens() {
        // call run()
        let func = "run".to_string();
        let tokens = stream_from_tokens(vec![
            Token::Ident("call".to_string()),
            Token::Ident(func.clone()),
            Token::Punctuation('('),
            Token::Punctuation(')'),
        ]);
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_func_call(), Ok(FuncCall {
            name: func,
            args: vec![],
        }));
    }

    #[test]
    fn parse_func_call_one_arg() {
        // call square(3)
        let func = "square".to_string();
        let arg = 3;
        let tokens = stream_from_tokens(vec![
            Token::Ident("call".to_string()),
            Token::Ident(func.clone()),
            Token::Punctuation('('),
            Token::Number(arg),
            Token::Punctuation(')'),
        ]);
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_func_call(), Ok(FuncCall {
            name: func,
            args: vec![Expr {
                root: Term {
                    root: Factor::Number(arg),
                    ops: vec![],
                },
                ops: vec![],
            }],
        }));
    }

    #[test]
    fn parse_func_call_two_args() {
        // call add(x, y)
        let func = "add".to_string();
        let tokens = stream_from_tokens(vec![
            Token::Ident("call".to_string()),
            Token::Ident(func.clone()),
            Token::Punctuation('('),
            Token::Ident("x".to_string()),
            Token::Punctuation(','),
            Token::Ident("y".to_string()),
            Token::Punctuation(')'),
        ]);
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_func_call(), Ok(FuncCall {
            name: func,
            args: vec![
                Expr {
                    root: Term {
                        root: Factor::VarRef("x".to_string()),
                        ops: vec![],
                    },
                    ops: vec![],
                },
                Expr {
                    root: Term {
                        root: Factor::VarRef("y".to_string()),
                        ops: vec![],
                    },
                    ops: vec![],
                },
            ],
        }));
    }

    #[test]
    fn parse_func_call_many_args() {
        // call max(x, y, 5)
        let tokens = stream_from_tokens(vec![
            Token::Ident("call".to_string()),
            Token::Ident("max".to_string()),
            Token::Punctuation('('),
            Token::Ident("x".to_string()),
            Token::Punctuation(','),
            Token::Ident("y".to_string()),
            Token::Punctuation(','),
            Token::Number(5),
            Token::Punctuation(')'),
        ]);
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_func_call(), Ok(FuncCall {
            name: "max".to_string(),
            args: vec![
                Expr {
                    root: Term {
                        root: Factor::VarRef("x".to_string()),
                        ops: vec![],
                    },
                    ops: vec![],
                },
                Expr {
                    root: Term {
                        root: Factor::VarRef("y".to_string()),
                        ops: vec![],
                    },
                    ops: vec![],
                },
                Expr {
                    root: Term {
                        root: Factor::Number(5),
                        ops: vec![],
                    },
                    ops: vec![],
                },
            ],
        }));
    }

    #[test]
    fn parse_if_stmt_simple_then_no_else() {
        /*
            if a > 0
            then
                let b <- 1
            fi
        */

        let tokens = stream_from_tokens(vec![
            Token::Ident("if".to_string()),
            Token::Ident("a".to_string()),
            Token::RelOp(RelOp::Gt),
            Token::Number(0),
            Token::Ident("then".to_string()),
            Token::Ident("let".to_string()),
            Token::Ident("b".to_string()),
            Token::AssignOp,
            Token::Number(1),
            Token::Ident("fi".to_string()),
        ]);
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_if_stmt(), Ok(IfStmt {
            condition: Relation {
                lhs: Expr {
                    root: Term {
                        root: Factor::VarRef("a".to_string()),
                        ops: vec![],
                    },
                    ops: vec![],
                },
                rhs: Expr {
                    root: Term {
                        root: Factor::Number(0),
                        ops: vec![],
                    },
                    ops: vec![],
                },
                op: RelOp::Gt,
            },

            then_block: Block {
                body: vec![Stmt::Assignment(Assignment {
                    place: "b".to_string(),
                    value: Expr {
                        root: Term {
                            root: Factor::Number(1),
                            ops: vec![],
                        },
                        ops: vec![],
                    },
                })],
            },

            else_block: None,
        }));
    }

    #[test]
    fn parse_if_stmt_simple_then_simple_else() {
        /*
            if a > 0
            then
                let b <- 1
            else
                let b <- 2;
            fi
        */

        let tokens = stream_from_tokens(vec![
            Token::Ident("if".to_string()),
            Token::Ident("a".to_string()),
            Token::RelOp(RelOp::Gt),
            Token::Number(0),
            Token::Ident("then".to_string()),
            Token::Ident("let".to_string()),
            Token::Ident("b".to_string()),
            Token::AssignOp,
            Token::Number(1),
            Token::Ident("else".to_string()),
            Token::Ident("let".to_string()),
            Token::Ident("b".to_string()),
            Token::AssignOp,
            Token::Number(2),
            Token::Punctuation(';'),
            Token::Ident("fi".to_string()),
        ]);
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_if_stmt(), Ok(IfStmt {
            condition: Relation {
                lhs: Expr {
                    root: Term {
                        root: Factor::VarRef("a".to_string()),
                        ops: vec![],
                    },
                    ops: vec![],
                },
                rhs: Expr {
                    root: Term {
                        root: Factor::Number(0),
                        ops: vec![],
                    },
                    ops: vec![],
                },
                op: RelOp::Gt,
            },

            then_block: Block {
                body: vec![Stmt::Assignment(Assignment {
                    place: "b".to_string(),
                    value: Expr {
                        root: Term {
                            root: Factor::Number(1),
                            ops: vec![],
                        },
                        ops: vec![],
                    },
                })],
            },

            else_block: Some(Block {
                body: vec![Stmt::Assignment(Assignment {
                    place: "b".to_string(),
                    value: Expr {
                        root: Term {
                            root: Factor::Number(2),
                            ops: vec![],
                        },
                        ops: vec![],
                    },
                })],
            }),
        }));
    }

    #[test]
    fn parse_if_stmt_simple_then_complex_else() {
        /*
            if a > 0
            then
                let b <- 1;
            else
                let b <- 2;
                let c <- a + b;
            fi
        */

        let tokens = stream_from_tokens(vec![
            Token::Ident("if".to_string()),
            Token::Ident("a".to_string()),
            Token::RelOp(RelOp::Gt),
            Token::Number(0),
            Token::Ident("then".to_string()),
            Token::Ident("let".to_string()),
            Token::Ident("b".to_string()),
            Token::AssignOp,
            Token::Number(1),
            Token::Ident("else".to_string()),
            Token::Ident("let".to_string()),
            Token::Ident("b".to_string()),
            Token::AssignOp,
            Token::Number(2),
            Token::Punctuation(';'),
            Token::Ident("let".to_string()),
            Token::Ident("c".to_string()),
            Token::AssignOp,
            Token::Ident("a".to_string()),
            Token::Punctuation('+'),
            Token::Ident("b".to_string()),
            Token::Punctuation(';'),
            Token::Ident("fi".to_string()),
        ]);
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_if_stmt(), Ok(IfStmt {
            condition: Relation {
                lhs: Expr {
                    root: Term {
                        root: Factor::VarRef("a".to_string()),
                        ops: vec![],
                    },
                    ops: vec![],
                },
                rhs: Expr {
                    root: Term {
                        root: Factor::Number(0),
                        ops: vec![],
                    },
                    ops: vec![],
                },
                op: RelOp::Gt,
            },

            then_block: Block {
                body: vec![Stmt::Assignment(Assignment {
                    place: "b".to_string(),
                    value: Expr {
                        root: Term {
                            root: Factor::Number(1),
                            ops: vec![],
                        },
                        ops: vec![],
                    },
                })],
            },

            else_block: Some(Block {
                body: vec![
                    Stmt::Assignment(Assignment {
                        place: "b".to_string(),
                        value: Expr {
                            root: Term {
                                root: Factor::Number(2),
                                ops: vec![],
                            },
                            ops: vec![],
                        },
                    }),
                    Stmt::Assignment(Assignment {
                        place: "c".to_string(),
                        value: Expr {
                            root: Term {
                                root: Factor::VarRef("a".to_string()),
                                ops: vec![],
                            },
                            ops: vec![(TermOp::Add, Term {
                                root: Factor::VarRef("b".to_string()),
                                ops: vec![],
                            })],
                        },
                    }),
                ],
            }),
        }));
    }

    #[test]
    fn parse_if_stmt_complex_then_no_else() {
        /*
            if a > 0
            then
                let b <- 1;
                let c <- a + b;
            fi
        */

        let tokens = stream_from_tokens(vec![
            Token::Ident("if".to_string()),
            Token::Ident("a".to_string()),
            Token::RelOp(RelOp::Gt),
            Token::Number(0),
            Token::Ident("then".to_string()),
            Token::Ident("let".to_string()),
            Token::Ident("b".to_string()),
            Token::AssignOp,
            Token::Number(1),
            Token::Punctuation(';'),
            Token::Ident("let".to_string()),
            Token::Ident("c".to_string()),
            Token::AssignOp,
            Token::Ident("a".to_string()),
            Token::Punctuation('+'),
            Token::Ident("b".to_string()),
            Token::Punctuation(';'),
            Token::Ident("fi".to_string()),
        ]);
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_if_stmt(), Ok(IfStmt {
            condition: Relation {
                lhs: Expr {
                    root: Term {
                        root: Factor::VarRef("a".to_string()),
                        ops: vec![],
                    },
                    ops: vec![],
                },
                rhs: Expr {
                    root: Term {
                        root: Factor::Number(0),
                        ops: vec![],
                    },
                    ops: vec![],
                },
                op: RelOp::Gt,
            },

            then_block: Block {
                body: vec![
                    Stmt::Assignment(Assignment {
                        place: "b".to_string(),
                        value: Expr {
                            root: Term {
                                root: Factor::Number(1),
                                ops: vec![],
                            },
                            ops: vec![],
                        },
                    }),
                    Stmt::Assignment(Assignment {
                        place: "c".to_string(),
                        value: Expr {
                            root: Term {
                                root: Factor::VarRef("a".to_string()),
                                ops: vec![],
                            },
                            ops: vec![(TermOp::Add, Term {
                                root: Factor::VarRef("b".to_string()),
                                ops: vec![],
                            })],
                        },
                    }),
                ],
            },

            else_block: None,
        }));
    }

    #[test]
    fn parse_if_stmt_complex_then_simple_else() {
        /*
            if a > 0
            then
                let b <- 1;
                let c <- a + b;
            else
                let b <- 2;
            fi
        */

        let tokens = stream_from_tokens(vec![
            Token::Ident("if".to_string()),
            Token::Ident("a".to_string()),
            Token::RelOp(RelOp::Gt),
            Token::Number(0),
            Token::Ident("then".to_string()),
            Token::Ident("let".to_string()),
            Token::Ident("b".to_string()),
            Token::AssignOp,
            Token::Number(1),
            Token::Punctuation(';'),
            Token::Ident("let".to_string()),
            Token::Ident("c".to_string()),
            Token::AssignOp,
            Token::Ident("a".to_string()),
            Token::Punctuation('+'),
            Token::Ident("b".to_string()),
            Token::Punctuation(';'),
            Token::Ident("else".to_string()),
            Token::Ident("let".to_string()),
            Token::Ident("b".to_string()),
            Token::AssignOp,
            Token::Number(2),
            Token::Punctuation(';'),
            Token::Ident("fi".to_string()),
        ]);
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_if_stmt(), Ok(IfStmt {
            condition: Relation {
                lhs: Expr {
                    root: Term {
                        root: Factor::VarRef("a".to_string()),
                        ops: vec![],
                    },
                    ops: vec![],
                },
                rhs: Expr {
                    root: Term {
                        root: Factor::Number(0),
                        ops: vec![],
                    },
                    ops: vec![],
                },
                op: RelOp::Gt,
            },

            then_block: Block {
                body: vec![
                    Stmt::Assignment(Assignment {
                        place: "b".to_string(),
                        value: Expr {
                            root: Term {
                                root: Factor::Number(1),
                                ops: vec![],
                            },
                            ops: vec![],
                        },
                    }),
                    Stmt::Assignment(Assignment {
                        place: "c".to_string(),
                        value: Expr {
                            root: Term {
                                root: Factor::VarRef("a".to_string()),
                                ops: vec![],
                            },
                            ops: vec![(TermOp::Add, Term {
                                root: Factor::VarRef("b".to_string()),
                                ops: vec![],
                            })],
                        },
                    }),
                ],
            },

            else_block: Some(Block {
                body: vec![Stmt::Assignment(Assignment {
                    place: "b".to_string(),
                    value: Expr {
                        root: Term {
                            root: Factor::Number(2),
                            ops: vec![],
                        },
                        ops: vec![],
                    },
                })],
            }),
        }));
    }

    #[test]
    fn parse_if_stmt_complex_then_complex_else() {
        /*
            if a > 0
            then
                let b <- 1;
                let c <- a + b;
            else
                let b <- 2;
                let c <- a * 2;
            fi
        */

        let tokens = stream_from_tokens(vec![
            Token::Ident("if".to_string()),
            Token::Ident("a".to_string()),
            Token::RelOp(RelOp::Gt),
            Token::Number(0),
            Token::Ident("then".to_string()),
            Token::Ident("let".to_string()),
            Token::Ident("b".to_string()),
            Token::AssignOp,
            Token::Number(1),
            Token::Punctuation(';'),
            Token::Ident("let".to_string()),
            Token::Ident("c".to_string()),
            Token::AssignOp,
            Token::Ident("a".to_string()),
            Token::Punctuation('+'),
            Token::Ident("b".to_string()),
            Token::Punctuation(';'),
            Token::Ident("else".to_string()),
            Token::Ident("let".to_string()),
            Token::Ident("b".to_string()),
            Token::AssignOp,
            Token::Number(2),
            Token::Punctuation(';'),
            Token::Ident("let".to_string()),
            Token::Ident("c".to_string()),
            Token::AssignOp,
            Token::Ident("a".to_string()),
            Token::Punctuation('*'),
            Token::Number(2),
            Token::Punctuation(';'),
            Token::Ident("fi".to_string()),
        ]);
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_if_stmt(), Ok(IfStmt {
            condition: Relation {
                lhs: Expr {
                    root: Term {
                        root: Factor::VarRef("a".to_string()),
                        ops: vec![],
                    },
                    ops: vec![],
                },
                rhs: Expr {
                    root: Term {
                        root: Factor::Number(0),
                        ops: vec![],
                    },
                    ops: vec![],
                },
                op: RelOp::Gt,
            },

            then_block: Block {
                body: vec![
                    Stmt::Assignment(Assignment {
                        place: "b".to_string(),
                        value: Expr {
                            root: Term {
                                root: Factor::Number(1),
                                ops: vec![],
                            },
                            ops: vec![],
                        },
                    }),
                    Stmt::Assignment(Assignment {
                        place: "c".to_string(),
                        value: Expr {
                            root: Term {
                                root: Factor::VarRef("a".to_string()),
                                ops: vec![],
                            },
                            ops: vec![(TermOp::Add, Term {
                                root: Factor::VarRef("b".to_string()),
                                ops: vec![],
                            })],
                        },
                    }),
                ],
            },

            else_block: Some(Block {
                body: vec![
                    Stmt::Assignment(Assignment {
                        place: "b".to_string(),
                        value: Expr {
                            root: Term {
                                root: Factor::Number(2),
                                ops: vec![],
                            },
                            ops: vec![],
                        },
                    }),
                    Stmt::Assignment(Assignment {
                        place: "c".to_string(),
                        value: Expr {
                            root: Term {
                                root: Factor::VarRef("a".to_string()),
                                ops: vec![(FactorOp::Mul, Factor::Number(2))],
                            },
                            ops: vec![],
                        },
                    }),
                ],
            }),
        }));
    }

    #[test]
    fn parse_if_stmt_complex_condition() {
        /*
            if a + 1 > b * 2 + c
            then
                let result <- 0;
            fi
        */

        let tokens = stream_from_tokens(vec![
            Token::Ident("if".to_string()),
            Token::Ident("a".to_string()),
            Token::Punctuation('+'),
            Token::Number(1),
            Token::RelOp(RelOp::Gt),
            Token::Ident("b".to_string()),
            Token::Punctuation('*'),
            Token::Number(2),
            Token::Punctuation('+'),
            Token::Ident("c".to_string()),
            Token::Ident("then".to_string()),
            Token::Ident("let".to_string()),
            Token::Ident("result".to_string()),
            Token::AssignOp,
            Token::Number(0),
            Token::Ident("fi".to_string()),
        ]);
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_if_stmt(), Ok(IfStmt {
            condition: Relation {
                lhs: Expr {
                    root: Term {
                        root: Factor::VarRef("a".to_string()),
                        ops: vec![],
                    },
                    ops: vec![(TermOp::Add, Term {
                        root: Factor::Number(1),
                        ops: vec![],
                    })],
                },
                rhs: Expr {
                    root: Term {
                        root: Factor::VarRef("b".to_string()),
                        ops: vec![(FactorOp::Mul, Factor::Number(2))],
                    },
                    ops: vec![(TermOp::Add, Term {
                        root: Factor::VarRef("c".to_string()),
                        ops: vec![],
                    })],
                },
                op: RelOp::Gt,
            },

            then_block: Block {
                body: vec![Stmt::Assignment(Assignment {
                    place: "result".to_string(),
                    value: Expr {
                        root: Term {
                            root: Factor::Number(0),
                            ops: vec![],
                        },
                        ops: vec![],
                    },
                })],
            },

            else_block: None,
        }));
    }

    #[test]
    fn parse_if_stmt_as_stmt() {
        /*
            if a > 0
            then
                let b <- 1;
            else
                let b <- 2;
            fi
        */

        let tokens = stream_from_tokens(vec![
            Token::Ident("if".to_string()),
            Token::Ident("a".to_string()),
            Token::RelOp(RelOp::Gt),
            Token::Number(0),
            Token::Ident("then".to_string()),
            Token::Ident("let".to_string()),
            Token::Ident("b".to_string()),
            Token::AssignOp,
            Token::Number(1),
            Token::Ident("else".to_string()),
            Token::Ident("let".to_string()),
            Token::Ident("b".to_string()),
            Token::AssignOp,
            Token::Number(2),
            Token::Punctuation(';'),
            Token::Ident("fi".to_string()),
        ]);
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_stmt(), Ok(Stmt::If(IfStmt {
            condition: Relation {
                lhs: Expr {
                    root: Term {
                        root: Factor::VarRef("a".to_string()),
                        ops: vec![],
                    },
                    ops: vec![],
                },
                rhs: Expr {
                    root: Term {
                        root: Factor::Number(0),
                        ops: vec![],
                    },
                    ops: vec![],
                },
                op: RelOp::Gt,
            },

            then_block: Block {
                body: vec![Stmt::Assignment(Assignment {
                    place: "b".to_string(),
                    value: Expr {
                        root: Term {
                            root: Factor::Number(1),
                            ops: vec![],
                        },
                        ops: vec![],
                    },
                })],
            },

            else_block: Some(Block {
                body: vec![Stmt::Assignment(Assignment {
                    place: "b".to_string(),
                    value: Expr {
                        root: Term {
                            root: Factor::Number(2),
                            ops: vec![],
                        },
                        ops: vec![],
                    },
                })],
            }),
        })));
    }

    #[test]
    fn parse_loop_simple_body() {
        /*
            while a > 0
            do
                let a <- a - 1;
            od
        */

        let tokens = stream_from_tokens(vec![
            Token::Ident("while".to_string()),
            Token::Ident("a".to_string()),
            Token::RelOp(RelOp::Gt),
            Token::Number(0),
            Token::Ident("do".to_string()),
            Token::Ident("let".to_string()),
            Token::Ident("a".to_string()),
            Token::AssignOp,
            Token::Ident("a".to_string()),
            Token::Punctuation('-'),
            Token::Number(1),
            Token::Punctuation(';'),
            Token::Ident("od".to_string()),
        ]);
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_loop(), Ok(Loop {
            condition: Relation {
                lhs: Expr {
                    root: Term {
                        root: Factor::VarRef("a".to_string()),
                        ops: vec![],
                    },
                    ops: vec![],
                },
                rhs: Expr {
                    root: Term {
                        root: Factor::Number(0),
                        ops: vec![],
                    },
                    ops: vec![],
                },
                op: RelOp::Gt,
            },

            body: Block {
                body: vec![
                    Stmt::Assignment(Assignment {
                        place: "a".to_string(),
                        value: Expr {
                            root: Term {
                                root: Factor::VarRef("a".to_string()),
                                ops: vec![],
                            },
                            ops: vec![(TermOp::Sub, Term {
                                root: Factor::Number(1),
                                ops: vec![],
                            })],
                        },
                    }),
                ],
            },
        }));
    }

    #[test]
    fn parse_loop_complex_body() {
        /*
            while a > 0
            do
                let b <- b + a;
                let a <- a - 1;
        */

        let tokens = stream_from_tokens(vec![
            Token::Ident("while".to_string()),
            Token::Ident("a".to_string()),
            Token::RelOp(RelOp::Gt),
            Token::Number(0),
            Token::Ident("do".to_string()),
            Token::Ident("let".to_string()),
            Token::Ident("b".to_string()),
            Token::AssignOp,
            Token::Ident("b".to_string()),
            Token::Punctuation('+'),
            Token::Ident("a".to_string()),
            Token::Punctuation(';'),
            Token::Ident("let".to_string()),
            Token::Ident("a".to_string()),
            Token::AssignOp,
            Token::Ident("a".to_string()),
            Token::Punctuation('-'),
            Token::Number(1),
            Token::Punctuation(';'),
            Token::Ident("od".to_string()),
        ]);
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_loop(), Ok(Loop {
            condition: Relation {
                lhs: Expr {
                    root: Term {
                        root: Factor::VarRef("a".to_string()),
                        ops: vec![],
                    },
                    ops: vec![],
                },
                rhs: Expr {
                    root: Term {
                        root: Factor::Number(0),
                        ops: vec![],
                    },
                    ops: vec![],
                },
                op: RelOp::Gt,
            },

            body: Block {
                body: vec![
                    Stmt::Assignment(Assignment {
                        place: "b".to_string(),
                        value: Expr {
                            root: Term {
                                root: Factor::VarRef("b".to_string()),
                                ops: vec![],
                            },
                            ops: vec![(TermOp::Add, Term {
                                root: Factor::VarRef("a".to_string()),
                                ops: vec![],
                            })],
                        },
                    }),
                    Stmt::Assignment(Assignment {
                        place: "a".to_string(),
                        value: Expr {
                            root: Term {
                                root: Factor::VarRef("a".to_string()),
                                ops: vec![],
                            },
                            ops: vec![(TermOp::Sub, Term {
                                root: Factor::Number(1),
                                ops: vec![],
                            })],
                        },
                    }),
                ],
            },
        }));
    }

    #[test]
    fn parse_loop_complex_condition() {
        /*
            while a - b > c * 2
            do
                let a <- a - b;
            od
        */

        let tokens = stream_from_tokens(vec![
            Token::Ident("while".to_string()),
            Token::Ident("a".to_string()),
            Token::Punctuation('-'),
            Token::Ident("b".to_string()),
            Token::RelOp(RelOp::Gt),
            Token::Ident("c".to_string()),
            Token::Punctuation('*'),
            Token::Number(2),
            Token::Ident("do".to_string()),
            Token::Ident("let".to_string()),
            Token::Ident("a".to_string()),
            Token::AssignOp,
            Token::Ident("a".to_string()),
            Token::Punctuation('-'),
            Token::Ident("b".to_string()),
            Token::Punctuation(';'),
            Token::Ident("od".to_string()),
        ]);
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_loop(), Ok(Loop {
            condition: Relation {
                lhs: Expr {
                    root: Term {
                        root: Factor::VarRef("a".to_string()),
                        ops: vec![],
                    },
                    ops: vec![(TermOp::Sub, Term {
                        root: Factor::VarRef("b".to_string()),
                        ops: vec![],
                    })],
                },
                rhs: Expr {
                    root: Term {
                        root: Factor::VarRef("c".to_string()),
                        ops: vec![(FactorOp::Mul, Factor::Number(2))],
                    },
                    ops: vec![],
                },
                op: RelOp::Gt,
            },

            body: Block {
                body: vec![
                    Stmt::Assignment(Assignment {
                        place: "a".to_string(),
                        value: Expr {
                            root: Term {
                                root: Factor::VarRef("a".to_string()),
                                ops: vec![],
                            },
                            ops: vec![(TermOp::Sub, Term {
                                root: Factor::VarRef("b".to_string()),
                                ops: vec![],
                            })],
                        },
                    }),
                ],
            },
        }));
    }

    #[test]
    fn parse_relation_simple() {
        // a > 0
        let var = "asg".to_string();
        let n = 0;
        let op = RelOp::Gt;
        let tokens = stream_from_tokens(vec![
            Token::Ident(var.clone()),
            Token::RelOp(op),
            Token::Number(n),
        ]);
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_relation(), Ok(Relation {
            lhs: Expr {
                root: Term {
                    root: Factor::VarRef(var),
                    ops: vec![],
                },
                ops: vec![],
            },
            rhs: Expr {
                root: Term {
                    root: Factor::Number(n),
                    ops: vec![],
                },
                ops: vec![],
            },
            op,
        }));
    }

    #[test]
    fn parse_relation_complex() {
        // first - 1 > (second + 1) * 2
        let var1 = "first".to_string();
        let var2 = "second".to_string();
        let op = RelOp::Gt;
        let tokens = stream_from_tokens(vec![
            Token::Ident(var1.clone()),
            Token::Punctuation('-'),
            Token::Number(1),
            Token::RelOp(op),
            Token::Punctuation('('),
            Token::Ident(var2.clone()),
            Token::Punctuation('+'),
            Token::Number(1),
            Token::Punctuation(')'),
            Token::Punctuation('*'),
            Token::Number(2),
        ]);
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_relation(), Ok(Relation {
            lhs: Expr {
                root: Term {
                    root: Factor::VarRef(var1),
                    ops: vec![],
                },
                ops: vec![(TermOp::Sub, Term {
                    root: Factor::Number(1),
                    ops: vec![],
                })],
            },
            rhs: Expr {
                root: Term {
                    root: Factor::SubExpr(Box::new(Expr {
                        root: Term {
                            root: Factor::VarRef(var2),
                            ops: vec![],
                        },
                        ops: vec![(TermOp::Add, Term {
                            root: Factor::Number(1),
                            ops: vec![],
                        })],
                    })),
                    ops: vec![(FactorOp::Mul, Factor::Number(2))],
                },
                ops: vec![],
            },
            op,
        }));
    }

    #[test]
    fn parse_return_no_val() {
        let tokens = stream_from_tokens(vec![Token::Ident("return".to_string())]);
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_return(), Ok(Return { value: None }));
    }

    #[test]
    fn parse_return_with_val() {
        // return a + 1
        let tokens = stream_from_tokens(vec![
            Token::Ident("return".to_string()),
            Token::Ident("a".to_string()),
            Token::Punctuation('+'),
            Token::Number(1),
        ]);
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_return(), Ok(Return {
            value: Some(Expr {
                root: Term {
                    root: Factor::VarRef("a".to_string()),
                    ops: vec![],
                },
                ops: vec![(TermOp::Add, Term {
                    root: Factor::Number(1),
                    ops: vec![],
                })],
            }),
        }));
    }

    #[test]
    fn parse_return_as_stmt() {
        // return a + 1
        let tokens = stream_from_tokens(vec![
            Token::Ident("return".to_string()),
            Token::Ident("a".to_string()),
            Token::Punctuation('+'),
            Token::Number(1),
        ]);
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_stmt(), Ok(Stmt::Return(Return {
            value: Some(Expr {
                root: Term {
                    root: Factor::VarRef("a".to_string()),
                    ops: vec![],
                },
                ops: vec![(TermOp::Add, Term {
                    root: Factor::Number(1),
                    ops: vec![],
                })],
            }),
        })));
    }

    #[test]
    fn parse_term_single_factor() {
        let n = 5;
        let tokens = stream_from_tokens(vec![Token::Number(n)]);
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_term(), Ok(Term {
            root: Factor::Number(n),
            ops: vec![],
        }));
    }

    #[test]
    fn parse_term_two_factors() {
        let n = 5;
        let m = 6;
        let op = FactorOp::Mul;
        let tokens = stream_from_tokens(vec![
            Token::Number(n),
            Token::Punctuation(op.into()),
            Token::Number(m),
        ]);
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_term(), Ok(Term {
            root: Factor::Number(n),
            ops: vec![(op, Factor::Number(m))],
        }));
    }

    #[test]
    fn parse_term_many_factor() {
        let x = 6;
        let y = 4;
        let z = 2;
        let op1 = FactorOp::Mul;
        let op2 = FactorOp::Div;
        let tokens = stream_from_tokens(vec![
            Token::Number(x),
            Token::Punctuation(op1.into()),
            Token::Number(y),
            Token::Punctuation(op2.into()),
            Token::Number(z),
        ]);
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_term(), Ok(Term {
            root: Factor::Number(x),
            ops: vec![
                (op1, Factor::Number(y)),
                (op2, Factor::Number(z)),
            ],
        }));
    }
}
