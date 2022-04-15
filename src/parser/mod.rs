mod err;
mod stream;

use crate::{
    ast::{
        Assignment, Block, Computation, Expr, Factor, FuncCall, FuncDecl, IfStmt, Loop,
        Relation, Return, Stmt, Term,
    },
    scanner::TokenResult,
    semcheck,
    sym::SymbolTable,
    tok::Token,
    utils::Keyword,
};
pub use self::err::ParseError;
use self::stream::TokenStream;

pub type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<T: Iterator<Item = TokenResult>> {
    stream: TokenStream<T>,
    sym_table: SymbolTable,
}

impl<T: Iterator<Item = TokenResult>> Parser<T> {
    pub fn new(tokens: T) -> ParseResult<Self> {
        Ok(Parser {
            stream: TokenStream::new(tokens)?,
            sym_table: SymbolTable::new(),
        })
    }

    #[cfg(test)]
    pub fn debug(tokens: T) -> ParseResult<Self> {
        Ok(Parser {
            stream: TokenStream::new(tokens)?,
            sym_table: SymbolTable::debug(),
        })
    }

    fn check_for_factor(&self) -> bool {
        matches!(
            self.stream.peek(),
            Some(Token::Punctuation('(') | Token::Number(_) | Token::Keyword(Keyword::Call) | Token::Ident(_))
        )
    }

    fn check_for_stmt(&self) -> bool {
        matches!(
            self.stream.try_peek_keyword(),
            Some(Keyword::Let | Keyword::Call | Keyword::If | Keyword::While | Keyword::Return)
        )
    }

    pub fn parse_assignment(&mut self, scope: &str) -> ParseResult<Assignment> {
        self.stream.try_consume_matching_keyword(Keyword::Let)?;
        let place = self.stream.try_consume_ident()?;
        self.stream.try_consume_assign_op()?;
        let value = self.parse_expr()?;
        semcheck::check_var_is_declared(&self.sym_table, scope, &place)?;

        Ok(Assignment { place, value })
    }

    pub fn parse_block(&mut self, scope: &str) -> ParseResult<Block> {
        // regression: if you forget to terminate a statement with semicolon, it gives expected '}' which isn't great
        let mut body = vec![self.parse_stmt(scope)?];

        while self.stream.try_consume_matching_punctuation(';').is_ok() {
            if self.check_for_stmt() {
                body.push(self.parse_stmt(scope)?);
            }
        }

        Ok(Block { body })
    }

    pub fn parse_computation(&mut self) -> ParseResult<Computation> {
        self.stream.try_consume_matching_keyword(Keyword::Main)?;

        let scope_name = Keyword::Main.to_string();
        self.sym_table.insert_scope(scope_name.clone());

        while let Some(Keyword::Var) = self.stream.try_peek_keyword() {
            self.parse_var_decl(&scope_name)?;
        }

        let mut funcs = vec![];

        while self.stream.try_consume_matching_punctuation('{').is_err() {
            funcs.push(self.parse_func_decl()?);
        }

        let body = self.parse_block(&scope_name)?;
        self.stream.try_consume_matching_punctuation('}')?;
        self.stream.try_consume_matching_punctuation('.')?;

        Ok(Computation { funcs, body })
    }

    pub fn parse_expr(&mut self) -> ParseResult<Expr> {
        let root = self.parse_term()?;
        let mut ops = vec![];

        while let Some(op) = self.stream.try_consume_termop()? {
            let next = self.parse_term()?;
            ops.push((op, next))
        }

        Ok(Expr { root, ops })
    }

    pub fn parse_factor(&mut self) -> ParseResult<Factor> {
        if self.stream.try_consume_matching_punctuation('(').is_ok() {
            let subexpr = Box::new(self.parse_expr()?);

            self.stream.try_consume_matching_punctuation(')')
                .map(|_| Factor::SubExpr(subexpr))
        } else if let Some(n) = self.stream.try_consume_number()? {
            Ok(Factor::Number(n))
        } else {
            match self.stream.try_peek_keyword() {
                Some(Keyword::Call) => Ok(Factor::Call(self.parse_func_call()?)),
                _ => Ok(Factor::VarRef(self.stream.try_consume_ident()?)),
            }
        }
    }

    pub fn parse_func_call(&mut self) -> ParseResult<FuncCall> {
        self.stream.try_consume_matching_keyword(Keyword::Call)?;
        let name = self.stream.try_consume_ident()?;
        let mut args = vec![];

        if self.stream.try_consume_matching_punctuation('(').is_ok() && self.stream.try_consume_matching_punctuation(')').is_err() {
            args.push(self.parse_expr()?);

            while self.stream.try_consume_matching_punctuation(')').is_err() {
                self.stream.try_consume_matching_punctuation(',')?;
                args.push(self.parse_expr()?);
            }
        }

        semcheck::check_func_is_defined(&self.sym_table, &name)?;
        Ok(FuncCall { name, args })
    }

    pub fn parse_func_decl(&mut self) -> ParseResult<FuncDecl> {
        let returns_void = self.stream.try_consume_matching_keyword(Keyword::Void).is_ok();
        self.stream.try_consume_matching_keyword(Keyword::Function)?;
        let name = self.stream.try_consume_ident()?;
        self.stream.try_consume_matching_punctuation('(')?;
        self.sym_table.insert_scope(name.clone());

        let params = if self.stream.try_consume_matching_punctuation(')').is_ok() {
            vec![]
        } else {
            let mut params = vec![self.stream.try_consume_ident()?];

            while self.stream.try_consume_matching_punctuation(')').is_err() {
                self.stream.try_consume_matching_punctuation(',')?;
                params.push(self.stream.try_consume_ident()?);
            }

            params
        };

        for param in params.iter().cloned() {
            self.sym_table.insert_var(&name, param)?;
        }

        self.stream.try_consume_matching_punctuation(';')?;

        while self.stream.try_consume_matching_punctuation('{').is_err() {
            self.parse_var_decl(&name)?;
        }

        let body = if self.stream.try_consume_matching_punctuation('}').is_ok() {
            Block::empty()
        } else {
            let body = self.parse_block(&name)?;
            self.stream.try_consume_matching_punctuation('}')?;

            body
        };

        self.stream.try_consume_matching_punctuation(';')
            .map(|_| FuncDecl { returns_void, name, params, body })
    }

    pub fn parse_if_stmt(&mut self, scope: &str) -> ParseResult<IfStmt> {
        self.stream.try_consume_matching_keyword(Keyword::If)?;
        let condition = self.parse_relation()?;

        self.stream.try_consume_matching_keyword(Keyword::Then)?;
        let then_block = self.parse_block(scope)?;

        let else_block = if self.stream.try_consume_matching_keyword(Keyword::Else).is_ok() {
            Some(self.parse_block(scope)?)
        } else {
            None
        };

        self.stream.try_consume_matching_keyword(Keyword::Fi)?;

        Ok(IfStmt {
            condition,
            then_block,
            else_block,
        })
    }

    pub fn parse_loop(&mut self, scope: &str) -> ParseResult<Loop> {
        self.stream.try_consume_matching_keyword(Keyword::While)?;
        let condition = self.parse_relation()?;

        self.stream.try_consume_matching_keyword(Keyword::Do)?;
        let body = self.parse_block(scope)?;

        self.stream.try_consume_matching_keyword(Keyword::Od)?;
        Ok(Loop { condition, body })
    }

    pub fn parse_relation(&mut self) -> ParseResult<Relation> {
        let lhs = self.parse_expr()?;
        let op = self.stream.try_consume_relop()?;
        let rhs = self.parse_expr()?;

        Ok(Relation { lhs, rhs, op })
    }

    pub fn parse_return(&mut self) -> ParseResult<Return> {
        self.stream.try_consume_matching_keyword(Keyword::Return)?;
        let value = if self.check_for_factor() {
            // expr starts w/ term, which starts w/ factor, so check_for_factor() checks for expr implicitly
            Some(self.parse_expr()?)
        } else {
            None
        };

        Ok(Return { value })
    }

    pub fn parse_stmt(&mut self, scope: &str) -> ParseResult<Stmt> {
        match self.stream.try_peek_keyword() {
            Some(Keyword::Let) => self.parse_assignment(scope).map(|a| a.into()),
            Some(Keyword::Call) => self.parse_func_call().map(|c| c.into()),
            Some(Keyword::If) => self.parse_if_stmt(scope).map(|i| i.into()),
            Some(Keyword::While) => self.parse_loop(scope).map(|l| l.into()),
            Some(Keyword::Return) => self.parse_return().map(|r| r.into()),
            _ => Err(ParseError::ExpectedStatement),
        }
    }

    pub fn parse_term(&mut self) -> ParseResult<Term> {
        let root = self.parse_factor()?;
        let mut ops = vec![];

        while let Some(op) = self.stream.try_consume_factorop()? {
            let next = self.parse_factor()?;
            ops.push((op, next))
        }

        Ok(Term { root, ops })
    }

    pub fn parse_var_decl(&mut self, scope: &str) -> ParseResult<()> {
        self.stream.try_consume_matching_keyword(Keyword::Var)?;

        let var = self.stream.try_consume_ident()?;
        self.sym_table.insert_var(scope, var).ok();

        while self.stream.try_consume_matching_punctuation(';').is_err() {
            self.stream.try_consume_matching_punctuation(',')?;

            let var = self.stream.try_consume_ident()?;
            self.sym_table.insert_var(scope, var).ok();
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::{FactorOp, TermOp},
        scanner::InvalidCharError,
        sym::SymbolTable,
        tok::Token,
        utils::RelOp,
    };

    fn stream_from_tokens(tokens: Vec<Token>) -> impl Iterator<Item = TokenResult> {
        tokens.into_iter().map(|tok| Ok(tok))
    }

    #[test]
    fn parse_assignment_simple() {
        // asg = 123
        let var = "asg".to_string();
        let val = 123;
        let tokens = stream_from_tokens(vec![
            Token::Keyword(Keyword::Let),
            Token::Ident(var.clone()),
            Token::AssignOp,
            Token::Number(val),
        ]);
        let mut parser = Parser::debug(tokens).unwrap();
        parser.sym_table.insert_var(SymbolTable::DEBUG_SCOPE, var.clone()).ok();

        assert_eq!(
            parser.parse_assignment(SymbolTable::DEBUG_SCOPE),
            Ok(Assignment {
                place: var,
                value: Expr {
                    root: Term {
                        root: Factor::Number(val),
                        ops: vec![],
                    },
                    ops: vec![],
                },
            })
        );
    }

    #[test]
    fn parse_assignment_complex() {
        // result = 1 * 2 + 3
        let tokens = stream_from_tokens(vec![
            Token::Keyword(Keyword::Let),
            Token::Ident("result".to_string()),
            Token::AssignOp,
            Token::Number(1),
            Token::Punctuation('*'),
            Token::Number(2),
            Token::Punctuation('+'),
            Token::Number(3),
        ]);
        let mut parser = Parser::debug(tokens).unwrap();
        parser.sym_table.insert_var(SymbolTable::DEBUG_SCOPE, "result".to_string()).ok();

        assert_eq!(
            parser.parse_assignment(SymbolTable::DEBUG_SCOPE),
            Ok(Assignment {
                place: "result".to_string(),
                value: Expr {
                    root: Term {
                        root: Factor::Number(1),
                        ops: vec![(FactorOp::Mul, Factor::Number(2))],
                    },
                    ops: vec![(
                        TermOp::Add,
                        Term {
                            root: Factor::Number(3),
                            ops: vec![],
                        }
                    )],
                },
            })
        );
    }

    #[test]
    fn parse_assignment_as_stmt() {
        // asg = 123
        let var = "asg".to_string();
        let val = 123;
        let tokens = stream_from_tokens(vec![
            Token::Keyword(Keyword::Let),
            Token::Ident(var.clone()),
            Token::AssignOp,
            Token::Number(val),
        ]);
        let mut parser = Parser::debug(tokens).unwrap();
        parser.sym_table.insert_var(SymbolTable::DEBUG_SCOPE, var.clone()).unwrap();

        assert_eq!(
            parser.parse_stmt(SymbolTable::DEBUG_SCOPE),
            Ok(Stmt::Assignment(Assignment {
                place: var,
                value: Expr {
                    root: Term {
                        root: Factor::Number(val),
                        ops: vec![],
                    },
                    ops: vec![],
                },
            }))
        );
    }

    #[test]
    fn parse_computation_bare() {
        /*
            main
            var x;
            {
                let x <- 1;
            }.
        */

        let tokens = stream_from_tokens(vec![
            Token::Keyword(Keyword::Main),
            Token::Keyword(Keyword::Var),
            Token::Ident("x".to_string()),
            Token::Punctuation(';'),
            Token::Punctuation('{'),
            Token::Keyword(Keyword::Let),
            Token::Ident("x".to_string()),
            Token::AssignOp,
            Token::Number(1),
            Token::Punctuation(';'),
            Token::Punctuation('}'),
            Token::Punctuation('.'),
        ]);
        let mut parser = Parser::new(tokens).unwrap();

        assert_eq!(parser.parse_computation(), Ok(Computation {
            funcs: vec![],
            body: Block {
                body: vec![
                    Stmt::Assignment(Assignment {
                        place: "x".to_string(),
                        value: Expr {
                            root: Term {
                                root: Factor::Number(1),
                                ops: vec![],
                            },
                            ops: vec![],
                        }
                    })
                ],
            },
        }));
    }

    #[test]
    fn parse_computation_many_vars() {
        /*
            main
            var a, b;
            var c;
            {
                let c <- a + b;
            }.
        */

        let tokens = stream_from_tokens(vec![
            Token::Keyword(Keyword::Main),
            Token::Keyword(Keyword::Var),
            Token::Ident("a".to_string()),
            Token::Punctuation(','),
            Token::Ident("b".to_string()),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Var),
            Token::Ident("c".to_string()),
            Token::Punctuation(';'),
            Token::Punctuation('{'),
            Token::Keyword(Keyword::Let),
            Token::Ident("c".to_string()),
            Token::AssignOp,
            Token::Ident("a".to_string()),
            Token::Punctuation('+'),
            Token::Ident("b".to_string()),
            Token::Punctuation(';'),
            Token::Punctuation('}'),
            Token::Punctuation('.'),
        ]);
        let mut parser = Parser::new(tokens).unwrap();

        assert_eq!(parser.parse_computation(), Ok(Computation {
            funcs: vec![],
            body: Block {
                body: vec![
                    Stmt::Assignment(Assignment {
                        place: "c".to_string(),
                        value: Expr {
                            root: Term {
                                root: Factor::VarRef("a".to_string()),
                                ops: vec![],
                            },
                            ops: vec![
                                (TermOp::Add, Term {
                                    root: Factor::VarRef("b".to_string()),
                                    ops: vec![],
                                })
                            ],
                        }
                    })
                ],
            }
        }));
    }

    #[test]
    fn parse_computation_one_func() {
        /*
            main

            function add(x, y);
            {
                return x + y;
            };

            {
                call OutputNum(call add(1, 2));
            }.
        */

        let tokens = stream_from_tokens(vec![
            Token::Keyword(Keyword::Main),
            Token::Keyword(Keyword::Function),
            Token::Ident("add".to_string()),
            Token::Punctuation('('),
            Token::Ident("x".to_string()),
            Token::Punctuation(','),
            Token::Ident("y".to_string()),
            Token::Punctuation(')'),
            Token::Punctuation(';'),
            Token::Punctuation('{'),
            Token::Keyword(Keyword::Return),
            Token::Ident("x".to_string()),
            Token::Punctuation('+'),
            Token::Ident("y".to_string()),
            Token::Punctuation(';'),
            Token::Punctuation('}'),
            Token::Punctuation(';'),
            Token::Punctuation('{'),
            Token::Keyword(Keyword::Call),
            Token::Ident("OutputNum".to_string()),
            Token::Punctuation('('),
            Token::Keyword(Keyword::Call),
            Token::Ident("add".to_string()),
            Token::Punctuation('('),
            Token::Number(1),
            Token::Punctuation(','),
            Token::Number(2),
            Token::Punctuation(')'),
            Token::Punctuation(')'),
            Token::Punctuation(';'),
            Token::Punctuation('}'),
            Token::Punctuation('.'),
        ]);
        let mut parser = Parser::new(tokens).unwrap();

        assert_eq!(parser.parse_computation(), Ok(Computation {
            funcs: vec![
                FuncDecl {
                    returns_void: false,
                    name: "add".to_string(),
                    params: vec!["x".to_string(), "y".to_string()],
                    body: Block {
                        body: vec![
                            Stmt::Return(Return {
                                value: Some(Expr {
                                    root: Term {
                                        root: Factor::VarRef("x".to_string()),
                                        ops: vec![],
                                    },
                                    ops: vec![
                                        (TermOp::Add, Term {
                                            root: Factor::VarRef("y".to_string()),
                                            ops: vec![],
                                        })
                                    ]
                                })
                            })
                        ]
                    },
                }
            ],
            body: Block {
                body: vec![
                    Stmt::FuncCall(FuncCall {
                        name: "OutputNum".to_string(),
                        args: vec![Expr {
                            root: Term {
                                root: Factor::Call(FuncCall {
                                    name: "add".to_string(),
                                    args: vec![
                                        Expr {
                                            root: Term {
                                                root: Factor::Number(1),
                                                ops: vec![],
                                            },
                                            ops: vec![],
                                        },
                                        Expr {
                                            root: Term {
                                                root: Factor::Number(2),
                                                ops: vec![],
                                            },
                                            ops: vec![],
                                        },
                                    ]
                                }),
                                ops: vec![],
                            },
                            ops: vec![],
                        }]
                    })
                ],
            }
        }));
    }

    #[test]
    fn parse_computation_many_funcs() {
        /*
            main
            var x, y;

            function add(x, y);
            {
                return x + y;
            };

            function double(n);
            {
                return n * 2;
            };

            {
                let x <- 1;
                let y <- call double(x);
                call OutputNum(call add(x, y));
            }.
        */

        let tokens = stream_from_tokens(vec![
            Token::Keyword(Keyword::Main),
            Token::Keyword(Keyword::Var),
            Token::Ident("x".to_string()),
            Token::Punctuation(','),
            Token::Ident("y".to_string()),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Function),
            Token::Ident("add".to_string()),
            Token::Punctuation('('),
            Token::Ident("x".to_string()),
            Token::Punctuation(','),
            Token::Ident("y".to_string()),
            Token::Punctuation(')'),
            Token::Punctuation(';'),
            Token::Punctuation('{'),
            Token::Keyword(Keyword::Return),
            Token::Ident("x".to_string()),
            Token::Punctuation('+'),
            Token::Ident("y".to_string()),
            Token::Punctuation(';'),
            Token::Punctuation('}'),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Function),
            Token::Ident("double".to_string()),
            Token::Punctuation('('),
            Token::Ident("n".to_string()),
            Token::Punctuation(')'),
            Token::Punctuation(';'),
            Token::Punctuation('{'),
            Token::Keyword(Keyword::Return),
            Token::Ident("n".to_string()),
            Token::Punctuation('*'),
            Token::Number(2),
            Token::Punctuation(';'),
            Token::Punctuation('}'),
            Token::Punctuation(';'),
            Token::Punctuation('{'),
            Token::Keyword(Keyword::Let),
            Token::Ident("x".to_string()),
            Token::AssignOp,
            Token::Number(1),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Let),
            Token::Ident("y".to_string()),
            Token::AssignOp,
            Token::Keyword(Keyword::Call),
            Token::Ident("double".to_string()),
            Token::Punctuation('('),
            Token::Ident("x".to_string()),
            Token::Punctuation(')'),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Call),
            Token::Ident("OutputNum".to_string()),
            Token::Punctuation('('),
            Token::Keyword(Keyword::Call),
            Token::Ident("add".to_string()),
            Token::Punctuation('('),
            Token::Ident("x".to_string()),
            Token::Punctuation(','),
            Token::Ident("y".to_string()),
            Token::Punctuation(')'),
            Token::Punctuation(')'),
            Token::Punctuation(';'),
            Token::Punctuation('}'),
            Token::Punctuation('.'),
        ]);
        let mut parser = Parser::new(tokens).unwrap();

        assert_eq!(parser.parse_computation(), Ok(Computation {
            funcs: vec![
                FuncDecl {
                    returns_void: false,
                    name: "add".to_string(),
                    params: vec!["x".to_string(), "y".to_string()],
                    body: Block {
                        body: vec![
                            Stmt::Return(Return {
                                value: Some(Expr {
                                    root: Term {
                                        root: Factor::VarRef("x".to_string()),
                                        ops: vec![],
                                    },
                                    ops: vec![
                                        (TermOp::Add, Term {
                                            root: Factor::VarRef("y".to_string()),
                                            ops: vec![],
                                        })
                                    ]
                                })
                            })
                        ]
                    },
                },

                FuncDecl {
                    returns_void: false,
                    name: "double".to_string(),
                    params: vec!["n".to_string()],
                    body: Block {
                        body: vec![
                            Stmt::Return(Return {
                                value: Some(Expr {
                                    root: Term {
                                        root: Factor::VarRef("n".to_string()),
                                        ops: vec![(FactorOp::Mul, Factor::Number(2))],
                                    },
                                    ops: vec![],
                                })
                            })
                        ]
                    },
                },
            ],
            body: Block {
                body: vec![
                    Stmt::Assignment(Assignment {
                        place: "x".to_string(),
                        value: Expr {
                            root: Term {
                                root: Factor::Number(1),
                                ops: vec![],
                            },
                            ops: vec![],
                        },
                    }),

                    Stmt::Assignment(Assignment {
                        place: "y".to_string(),
                        value: Expr {
                            root: Term {
                                root: Factor::Call(FuncCall {
                                    name: "double".to_string(),
                                    args: vec![
                                        Expr {
                                            root: Term {
                                                root: Factor::VarRef("x".to_string()),
                                                ops: vec![],
                                            },
                                            ops: vec![],
                                        }
                                    ]
                                }),
                                ops: vec![],
                            },
                            ops: vec![],
                        }
                    }),

                    Stmt::FuncCall(FuncCall {
                        name: "OutputNum".to_string(),
                        args: vec![Expr {
                            root: Term {
                                root: Factor::Call(FuncCall {
                                    name: "add".to_string(),
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
                                    ]
                                }),
                                ops: vec![],
                            },
                            ops: vec![],
                        }]
                    })
                ],
            }
        }));
    }

    #[test]
    fn parse_computation_complex() {
        /*
            main
            var in, cmp, result;

            function square(n);
            {
                return n * n;
            };

            function big(n);
            {
                if n > 5
                then
                    return 1;
                else
                    return 0;
                fi
            };

            {
                let in <- call InputNum();
                let cmp <- call big(in);

                if cmp == 1
                then
                    let result <- in + 1;
                else
                    let result <- call square(in);
                fi

                call OutputNum(result);
            }.
        */

        let tokens = stream_from_tokens(vec![
            Token::Keyword(Keyword::Main),
            Token::Keyword(Keyword::Var),
            Token::Ident("in".to_string()),
            Token::Punctuation(','),
            Token::Ident("cmp".to_string()),
            Token::Punctuation(','),
            Token::Ident("result".to_string()),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Function),
            Token::Ident("square".to_string()),
            Token::Punctuation('('),
            Token::Ident("n".to_string()),
            Token::Punctuation(')'),
            Token::Punctuation(';'),
            Token::Punctuation('{'),
            Token::Keyword(Keyword::Return),
            Token::Ident("n".to_string()),
            Token::Punctuation('*'),
            Token::Ident("n".to_string()),
            Token::Punctuation(';'),
            Token::Punctuation('}'),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Function),
            Token::Ident("big".to_string()),
            Token::Punctuation('('),
            Token::Ident("n".to_string()),
            Token::Punctuation(')'),
            Token::Punctuation(';'),
            Token::Punctuation('{'),
            Token::Keyword(Keyword::If),
            Token::Ident("n".to_string()),
            Token::RelOp(RelOp::Gt),
            Token::Number(5),
            Token::Keyword(Keyword::Then),
            Token::Keyword(Keyword::Return),
            Token::Number(1),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Else),
            Token::Keyword(Keyword::Return),
            Token::Number(0),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Fi),
            Token::Punctuation(';'),
            Token::Punctuation('}'),
            Token::Punctuation(';'),
            Token::Punctuation('{'),
            Token::Keyword(Keyword::Let),
            Token::Ident("in".to_string()),
            Token::AssignOp,
            Token::Keyword(Keyword::Call),
            Token::Ident("InputNum".to_string()),
            Token::Punctuation('('),
            Token::Punctuation(')'),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Let),
            Token::Ident("cmp".to_string()),
            Token::AssignOp,
            Token::Keyword(Keyword::Call),
            Token::Ident("big".to_string()),
            Token::Punctuation('('),
            Token::Ident("in".to_string()),
            Token::Punctuation(')'),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::If),
            Token::Ident("cmp".to_string()),
            Token::RelOp(RelOp::Eq),
            Token::Number(1),
            Token::Keyword(Keyword::Then),
            Token::Keyword(Keyword::Let),
            Token::Ident("result".to_string()),
            Token::AssignOp,
            Token::Ident("in".to_string()),
            Token::Punctuation('+'),
            Token::Number(1),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Else),
            Token::Keyword(Keyword::Let),
            Token::Ident("result".to_string()),
            Token::AssignOp,
            Token::Keyword(Keyword::Call),
            Token::Ident("square".to_string()),
            Token::Punctuation('('),
            Token::Ident("in".to_string()),
            Token::Punctuation(')'),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Fi),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Call),
            Token::Ident("OutputNum".to_string()),
            Token::Punctuation('('),
            Token::Ident("result".to_string()),
            Token::Punctuation(')'),
            Token::Punctuation(';'),
            Token::Punctuation('}'),
            Token::Punctuation('.'),
        ]);
        let mut parser = Parser::new(tokens).unwrap();

        assert_eq!(parser.parse_computation(), Ok(Computation {
            funcs: vec![
                FuncDecl {
                    returns_void: false,
                    name: "square".to_string(),
                    params: vec!["n".to_string()],
                    body: Block {
                        body: vec![
                            Stmt::Return(Return {
                                value: Some(Expr {
                                    root: Term {
                                        root: Factor::VarRef("n".to_string()),
                                        ops: vec![
                                            (FactorOp::Mul, Factor::VarRef("n".to_string()))
                                        ],
                                    },
                                    ops: vec![],
                                })
                            })
                        ]
                    },
                },

                FuncDecl {
                    returns_void: false,
                    name: "big".to_string(),
                    params: vec!["n".to_string()],
                    body: Block {
                        body: vec![
                            Stmt::If(IfStmt {
                                condition: Relation {
                                    lhs: Expr {
                                        root: Term {
                                            root: Factor::VarRef("n".to_string()),
                                            ops: vec![],
                                        },
                                        ops: vec![],
                                    },
                                    rhs: Expr {
                                        root: Term {
                                            root: Factor::Number(5),
                                            ops: vec![],
                                        },
                                        ops: vec![],
                                    },
                                    op: RelOp::Gt,
                                },

                                then_block: Block {
                                    body: vec![
                                        Stmt::Return(Return {
                                            value: Some(Expr {
                                                root: Term {
                                                    root: Factor::Number(1),
                                                    ops: vec![],
                                                },
                                                ops: vec![],
                                            })
                                        })
                                    ],
                                },

                                else_block: Some(Block {
                                    body: vec![
                                        Stmt::Return(Return {
                                            value: Some(Expr {
                                                root: Term {
                                                    root: Factor::Number(0),
                                                    ops: vec![],
                                                },
                                                ops: vec![],
                                            })
                                        })
                                    ],
                                }),
                            })
                        ]
                    },
                },
            ],
            body: Block {
                body: vec![
                    Stmt::Assignment(Assignment {
                        place: "in".to_string(),
                        value: Expr {
                            root: Term {
                                root: Factor::Call(FuncCall {
                                    name: "InputNum".to_string(),
                                    args: vec![],
                                }),
                                ops: vec![],
                            },
                            ops: vec![],
                        },
                    }),

                    Stmt::Assignment(Assignment {
                        place: "cmp".to_string(),
                        value: Expr {
                            root: Term {
                                root: Factor::Call(FuncCall {
                                    name: "big".to_string(),
                                    args: vec![
                                        Expr {
                                            root: Term {
                                                root: Factor::VarRef("in".to_string()),
                                                ops: vec![],
                                            },
                                            ops: vec![],
                                        }
                                    ]
                                }),
                                ops: vec![],
                            },
                            ops: vec![],
                        }
                    }),

                    Stmt::If(IfStmt {
                        condition: Relation {
                            lhs: Expr {
                                root: Term {
                                    root: Factor::VarRef("cmp".to_string()),
                                    ops: vec![],
                                },
                                ops: vec![],
                            },
                            rhs: Expr {
                                root: Term {
                                    root: Factor::Number(1),
                                    ops: vec![],
                                },
                                ops: vec![],
                            },
                            op: RelOp::Eq,
                        },

                        then_block: Block {
                            body: vec![
                                Stmt::Assignment(Assignment {
                                    place: "result".to_string(),
                                    value: Expr {
                                        root: Term {
                                            root: Factor::VarRef("in".to_string()),
                                            ops: vec![],
                                        },
                                        ops: vec![
                                            (TermOp::Add, Term {
                                                root: Factor::Number(1),
                                                ops: vec![],
                                            }),
                                        ],
                                    }
                                })
                            ]
                        },

                        else_block: Some(Block {
                            body: vec![
                                Stmt::Assignment(Assignment {
                                    place: "result".to_string(),
                                    value: Expr {
                                        root: Term {
                                            root: Factor::Call(FuncCall {
                                                name: "square".to_string(),
                                                args: vec![
                                                    Expr {
                                                        root: Term {
                                                            root: Factor::VarRef("in".to_string()),
                                                            ops: vec![],
                                                        },
                                                        ops: vec![],
                                                    }
                                                ],
                                            }),
                                            ops: vec![],
                                        },
                                        ops: vec![],
                                    }
                                })
                            ]
                        }),
                    }),

                    Stmt::FuncCall(FuncCall {
                        name: "OutputNum".to_string(),
                        args: vec![Expr {
                            root: Term {
                                root: Factor::VarRef("result".to_string()),
                                ops: vec![],
                            },
                            ops: vec![],
                        }]
                    })
                ],
            }
        }));
    }

    #[test]
    fn parse_expr_single_term() {
        let n = 5;
        let tokens = stream_from_tokens(vec![Token::Number(n)]);
        let mut parser = Parser::new(tokens).unwrap();

        assert_eq!(
            parser.parse_expr(),
            Ok(Expr {
                root: Term {
                    root: Factor::Number(n),
                    ops: vec![],
                },
                ops: vec![],
            })
        );
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
        let mut parser = Parser::new(tokens).unwrap();

        assert_eq!(
            parser.parse_expr(),
            Ok(Expr {
                root: Term {
                    root: Factor::Number(n),
                    ops: vec![],
                },
                ops: vec![(
                    op,
                    Term {
                        root: Factor::Number(m),
                        ops: vec![],
                    }
                ),],
            })
        );
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
        let mut parser = Parser::new(tokens).unwrap();

        assert_eq!(
            parser.parse_expr(),
            Ok(Expr {
                root: Term {
                    root: Factor::Number(x),
                    ops: vec![],
                },
                ops: vec![
                    (
                        op1,
                        Term {
                            root: Factor::Number(y),
                            ops: vec![],
                        }
                    ),
                    (
                        op2,
                        Term {
                            root: Factor::Number(z),
                            ops: vec![],
                        }
                    ),
                ],
            })
        );
    }

    #[test]
    fn parse_factor_var_ref() {
        let ident = "asg".to_string();
        let tokens = stream_from_tokens(vec![Token::Ident(ident.clone())]);
        let mut parser = Parser::new(tokens).unwrap();

        assert_eq!(parser.parse_factor(), Ok(Factor::VarRef(ident)));
    }

    #[test]
    fn parse_factor_number() {
        let n = 5;
        let tokens = stream_from_tokens(vec![Token::Number(n)]);
        let mut parser = Parser::new(tokens).unwrap();

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
        let mut parser = Parser::new(tokens).unwrap();

        assert_eq!(
            parser.parse_factor(),
            Ok(Factor::SubExpr(Box::new(Expr {
                root: Term {
                    root: Factor::Number(n),
                    ops: vec![],
                },
                ops: vec![],
            })))
        );
    }

    #[test]
    fn parse_factor_func_call() {
        // call OutputNum(5)
        let func = "OutputNum".to_string();
        let arg = 5;
        let tokens = stream_from_tokens(vec![
            Token::Keyword(Keyword::Call),
            Token::Ident(func.clone()),
            Token::Punctuation('('),
            Token::Number(arg),
            Token::Punctuation(')'),
        ]);
        let mut parser = Parser::new(tokens).unwrap();

        assert_eq!(
            parser.parse_factor(),
            Ok(Factor::Call(FuncCall {
                name: func,
                args: vec![Expr {
                    root: Term {
                        root: Factor::Number(arg),
                        ops: vec![],
                    },
                    ops: vec![],
                }],
            }))
        );
    }

    #[test]
    fn parse_func_call_no_args_no_parens() {
        // call InputNum()
        let func = "InputNum".to_string();
        let tokens = stream_from_tokens(vec![
            Token::Keyword(Keyword::Call),
            Token::Ident(func.clone()),
        ]);
        let mut parser = Parser::new(tokens).unwrap();

        assert_eq!(
            parser.parse_func_call(),
            Ok(FuncCall {
                name: func,
                args: vec![],
            })
        );
    }

    #[test]
    fn parse_func_call_no_args_with_parens() {
        // call InputNum()
        let func = "InputNum".to_string();
        let tokens = stream_from_tokens(vec![
            Token::Keyword(Keyword::Call),
            Token::Ident(func.clone()),
            Token::Punctuation('('),
            Token::Punctuation(')'),
        ]);
        let mut parser = Parser::new(tokens).unwrap();

        assert_eq!(
            parser.parse_func_call(),
            Ok(FuncCall {
                name: func,
                args: vec![],
            })
        );
    }

    #[test]
    fn parse_func_call_one_arg() {
        // call OutputNum(3)
        let func = "OutputNum".to_string();
        let arg = 3;
        let tokens = stream_from_tokens(vec![
            Token::Keyword(Keyword::Call),
            Token::Ident(func.clone()),
            Token::Punctuation('('),
            Token::Number(arg),
            Token::Punctuation(')'),
        ]);
        let mut parser = Parser::new(tokens).unwrap();

        assert_eq!(
            parser.parse_func_call(),
            Ok(FuncCall {
                name: func,
                args: vec![Expr {
                    root: Term {
                        root: Factor::Number(arg),
                        ops: vec![],
                    },
                    ops: vec![],
                }],
            })
        );
    }

    #[test]
    fn parse_func_call_two_args() {
        // call add(x, y)
        let func = "add".to_string();
        let tokens = stream_from_tokens(vec![
            Token::Keyword(Keyword::Call),
            Token::Ident(func.clone()),
            Token::Punctuation('('),
            Token::Ident("x".to_string()),
            Token::Punctuation(','),
            Token::Ident("y".to_string()),
            Token::Punctuation(')'),
        ]);
        let mut parser = Parser::new(tokens).unwrap();
        parser.sym_table.insert_scope(func.clone());

        assert_eq!(
            parser.parse_func_call(),
            Ok(FuncCall {
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
            })
        );
    }

    #[test]
    fn parse_func_call_many_args() {
        // call max(x, y, 5)
        let tokens = stream_from_tokens(vec![
            Token::Keyword(Keyword::Call),
            Token::Ident("max".to_string()),
            Token::Punctuation('('),
            Token::Ident("x".to_string()),
            Token::Punctuation(','),
            Token::Ident("y".to_string()),
            Token::Punctuation(','),
            Token::Number(5),
            Token::Punctuation(')'),
        ]);
        let mut parser = Parser::new(tokens).unwrap();
        parser.sym_table.insert_scope("max".to_string());

        assert_eq!(
            parser.parse_func_call(),
            Ok(FuncCall {
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
            })
        );
    }

    #[test]
    fn parse_func_call_as_stmt() {
        // call OutputNum(3)
        let func = "OutputNum".to_string();
        let arg = 3;
        let tokens = stream_from_tokens(vec![
            Token::Keyword(Keyword::Call),
            Token::Ident(func.clone()),
            Token::Punctuation('('),
            Token::Number(arg),
            Token::Punctuation(')'),
        ]);
        let mut parser = Parser::debug(tokens).unwrap();

        assert_eq!(
            parser.parse_stmt(SymbolTable::DEBUG_SCOPE),
            Ok(Stmt::FuncCall(FuncCall {
                name: func,
                args: vec![Expr {
                    root: Term {
                        root: Factor::Number(arg),
                        ops: vec![],
                    },
                    ops: vec![],
                }],
            }))
        );
    }

    #[test]
    fn parse_func_decl_bare() {
        /*
            void function empty(); {

            };
        */

        let tokens = stream_from_tokens(vec![
            Token::Keyword(Keyword::Void),
            Token::Keyword(Keyword::Function),
            Token::Ident("empty".to_string()),
            Token::Punctuation('('),
            Token::Punctuation(')'),
            Token::Punctuation(';'),
            Token::Punctuation('{'),
            Token::Punctuation('}'),
            Token::Punctuation(';'),
        ]);
        let mut parser = Parser::new(tokens).unwrap();

        assert_eq!(parser.parse_func_decl(), Ok(FuncDecl {
            returns_void: true,
            name: "empty".to_string(),
            params: vec![],
            body: Block::empty(),
        }));
    }

    #[test]
    fn parse_func_decl_returns_non_void() {
        /*
            function const(); {
                return 1;
            };
        */

        let tokens = stream_from_tokens(vec![
            Token::Keyword(Keyword::Function),
            Token::Ident("const".to_string()),
            Token::Punctuation('('),
            Token::Punctuation(')'),
            Token::Punctuation(';'),
            Token::Punctuation('{'),
            Token::Keyword(Keyword::Return),
            Token::Number(1),
            Token::Punctuation(';'),
            Token::Punctuation('}'),
            Token::Punctuation(';'),
        ]);
        let mut parser = Parser::new(tokens).unwrap();

        assert_eq!(parser.parse_func_decl(), Ok(FuncDecl {
            returns_void: false,
            name: "const".to_string(),
            params: vec![],
            body: Block {
                body: vec![
                    Stmt::Return(Return { value: Some(Expr {
                        root: Term {
                            root: Factor::Number(1),
                            ops: vec![],
                        },
                        ops: vec![],
                    })}),
                ],
            },
        }));
    }

    #[test]
    fn parse_func_decl_one_param() {
        /*
            function square(n); {
                return n * n;
            };
        */

        let tokens = stream_from_tokens(vec![
            Token::Keyword(Keyword::Function),
            Token::Ident("square".to_string()),
            Token::Punctuation('('),
            Token::Ident("n".to_string()),
            Token::Punctuation(')'),
            Token::Punctuation(';'),
            Token::Punctuation('{'),
            Token::Keyword(Keyword::Return),
            Token::Ident("n".to_string()),
            Token::Punctuation('*'),
            Token::Ident("n".to_string()),
            Token::Punctuation(';'),
            Token::Punctuation('}'),
            Token::Punctuation(';'),
        ]);
        let mut parser = Parser::new(tokens).unwrap();

        assert_eq!(parser.parse_func_decl(), Ok(FuncDecl {
            returns_void: false,
            name: "square".to_string(),
            params: vec!["n".to_string()],
            body: Block {
                body: vec![
                    Stmt::Return(Return { value: Some(Expr {
                        root: Term {
                            root: Factor::VarRef("n".to_string()),
                            ops: vec![
                                (FactorOp::Mul, Factor::VarRef("n".to_string()))
                            ],
                        },
                        ops: vec![],
                    })}),
                ],
            },
        }));
    }

    #[test]
    fn parse_func_decl_many_params() {
        /*
            function add(x, y, z); {
                return x + y + z;
            };
        */

        let tokens = stream_from_tokens(vec![
            Token::Keyword(Keyword::Function),
            Token::Ident("add".to_string()),
            Token::Punctuation('('),
            Token::Ident("x".to_string()),
            Token::Punctuation(','),
            Token::Ident("y".to_string()),
            Token::Punctuation(','),
            Token::Ident("z".to_string()),
            Token::Punctuation(')'),
            Token::Punctuation(';'),
            Token::Punctuation('{'),
            Token::Keyword(Keyword::Return),
            Token::Ident("x".to_string()),
            Token::Punctuation('+'),
            Token::Ident("y".to_string()),
            Token::Punctuation('+'),
            Token::Ident("z".to_string()),
            Token::Punctuation(';'),
            Token::Punctuation('}'),
            Token::Punctuation(';'),
        ]);
        let mut parser = Parser::new(tokens).unwrap();

        assert_eq!(parser.parse_func_decl(), Ok(FuncDecl {
            returns_void: false,
            name: "add".to_string(),
            params: vec!["x".to_string(), "y".to_string(), "z".to_string()],
            body: Block {
                body: vec![
                    Stmt::Return(Return { value: Some(Expr {
                        root: Term {
                            root: Factor::VarRef("x".to_string()),
                            ops: vec![],
                        },
                        ops: vec![
                            (TermOp::Add, Term {
                                root: Factor::VarRef("y".to_string()),
                                ops: vec![],
                            }),
                            (TermOp::Add, Term {
                                root: Factor::VarRef("z".to_string()),
                                ops: vec![],
                            })
                        ],
                    })}),
                ],
            },
        }));
    }

    #[test]
    fn parse_func_decl_single_var_decl() {
        /*
            void function assign();
            var x; {
                let x <- 1;
            };
        */

        let tokens = stream_from_tokens(vec![
            Token::Keyword(Keyword::Void),
            Token::Keyword(Keyword::Function),
            Token::Ident("assign".to_string()),
            Token::Punctuation('('),
            Token::Punctuation(')'),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Var),
            Token::Ident("x".to_string()),
            Token::Punctuation(';'),
            Token::Punctuation('{'),
            Token::Keyword(Keyword::Let),
            Token::Ident("x".to_string()),
            Token::AssignOp,
            Token::Number(1),
            Token::Punctuation(';'),
            Token::Punctuation('}'),
            Token::Punctuation(';'),
        ]);
        let mut parser = Parser::new(tokens).unwrap();

        assert_eq!(parser.parse_func_decl(), Ok(FuncDecl {
            returns_void: true,
            name: "assign".to_string(),
            params: vec![],
            body: Block {
                body: vec![
                    Stmt::Assignment(Assignment {
                        place: "x".to_string(),
                        value: Expr {
                            root: Term {
                                root: Factor::Number(1),
                                ops: vec![],
                            },
                            ops: vec![],
                        },
                    }),
                ],
            },
        }));
    }

    #[test]
    fn parse_func_decl_many_var_decls() {
        /*
            void function assign();
            var x, y;
            var result; {
                let result <- x + y;
            };
        */

        let tokens = stream_from_tokens(vec![
            Token::Keyword(Keyword::Void),
            Token::Keyword(Keyword::Function),
            Token::Ident("add".to_string()),
            Token::Punctuation('('),
            Token::Punctuation(')'),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Var),
            Token::Ident("x".to_string()),
            Token::Punctuation(','),
            Token::Ident("y".to_string()),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Var),
            Token::Ident("result".to_string()),
            Token::Punctuation(';'),
            Token::Punctuation('{'),
            Token::Keyword(Keyword::Let),
            Token::Ident("result".to_string()),
            Token::AssignOp,
            Token::Ident("x".to_string()),
            Token::Punctuation('+'),
            Token::Ident("y".to_string()),
            Token::Punctuation(';'),
            Token::Punctuation('}'),
            Token::Punctuation(';'),
        ]);
        let mut parser = Parser::new(tokens).unwrap();

        assert_eq!(parser.parse_func_decl(), Ok(FuncDecl {
            returns_void: true,
            name: "add".to_string(),
            params: vec![],
            body: Block {
                body: vec![
                    Stmt::Assignment(Assignment {
                        place: "result".to_string(),
                        value: Expr {
                            root: Term {
                                root: Factor::VarRef("x".to_string()),
                                ops: vec![],
                            },
                            ops: vec![
                                (TermOp::Add, Term {
                                    root: Factor::VarRef("y".to_string()),
                                    ops: vec![],
                                })
                            ],
                        },
                    }),
                ],
            },
        }));
    }

    #[test]
    fn parse_func_decl_complex() {
        /*
            function gtz(n);
            var result; {
                if n > 0
                then
                    let result <- 1;
                else
                    let result <- 0;
                fi

                return result;
            };
        */

        let tokens = stream_from_tokens(vec![
            Token::Keyword(Keyword::Function),
            Token::Ident("gtz".to_string()),
            Token::Punctuation('('),
            Token::Ident("n".to_string()),
            Token::Punctuation(')'),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Var),
            Token::Ident("result".to_string()),
            Token::Punctuation(';'),
            Token::Punctuation('{'),
            Token::Keyword(Keyword::If),
            Token::Ident("n".to_string()),
            Token::RelOp(RelOp::Gt),
            Token::Number(0),
            Token::Keyword(Keyword::Then),
            Token::Keyword(Keyword::Let),
            Token::Ident("result".to_string()),
            Token::AssignOp,
            Token::Number(1),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Else),
            Token::Keyword(Keyword::Let),
            Token::Ident("result".to_string()),
            Token::AssignOp,
            Token::Number(0),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Fi),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Return),
            Token::Ident("result".to_string()),
            Token::Punctuation(';'),
            Token::Punctuation('}'),
            Token::Punctuation(';'),
        ]);
        let mut parser = Parser::new(tokens).unwrap();

        assert_eq!(parser.parse_func_decl(), Ok(FuncDecl {
            returns_void: false,
            name: "gtz".to_string(),
            params: vec!["n".to_string()],
            body: Block {
                body: vec![
                    Stmt::If(IfStmt {
                        condition: Relation {
                            lhs: Expr {
                                root: Term {
                                    root: Factor::VarRef("n".to_string()),
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
                                    place: "result".to_string(),
                                    value: Expr {
                                        root: Term {
                                            root: Factor::Number(1),
                                            ops: vec![],
                                        },
                                        ops: vec![],
                                    },
                                }),
                            ]
                        },

                        else_block: Some(Block {
                            body: vec![
                                Stmt::Assignment(Assignment {
                                    place: "result".to_string(),
                                    value: Expr {
                                        root: Term {
                                            root: Factor::Number(0),
                                            ops: vec![],
                                        },
                                        ops: vec![],
                                    },
                                }),
                            ]
                        }),
                    }),

                    Stmt::Return(Return {
                        value: Some(Expr {
                            root: Term {
                                root: Factor::VarRef("result".to_string()),
                                ops: vec![],
                            },
                            ops: vec![],
                        }),
                    }),
                ],
            },
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
            Token::Keyword(Keyword::If),
            Token::Ident("a".to_string()),
            Token::RelOp(RelOp::Gt),
            Token::Number(0),
            Token::Keyword(Keyword::Then),
            Token::Keyword(Keyword::Let),
            Token::Ident("b".to_string()),
            Token::AssignOp,
            Token::Number(1),
            Token::Keyword(Keyword::Fi),
        ]);
        let mut parser = Parser::debug(tokens).unwrap();
        parser.sym_table.insert_var(SymbolTable::DEBUG_SCOPE, "a".to_string()).unwrap();
        parser.sym_table.insert_var(SymbolTable::DEBUG_SCOPE, "b".to_string()).unwrap();

        assert_eq!(
            parser.parse_if_stmt(SymbolTable::DEBUG_SCOPE),
            Ok(IfStmt {
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
            })
        );
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
            Token::Keyword(Keyword::If),
            Token::Ident("a".to_string()),
            Token::RelOp(RelOp::Gt),
            Token::Number(0),
            Token::Keyword(Keyword::Then),
            Token::Keyword(Keyword::Let),
            Token::Ident("b".to_string()),
            Token::AssignOp,
            Token::Number(1),
            Token::Keyword(Keyword::Else),
            Token::Keyword(Keyword::Let),
            Token::Ident("b".to_string()),
            Token::AssignOp,
            Token::Number(2),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Fi),
        ]);
        let mut parser = Parser::debug(tokens).unwrap();
        parser.sym_table.insert_var(SymbolTable::DEBUG_SCOPE, "a".to_string()).unwrap();
        parser.sym_table.insert_var(SymbolTable::DEBUG_SCOPE, "b".to_string()).unwrap();

        assert_eq!(
            parser.parse_if_stmt(SymbolTable::DEBUG_SCOPE),
            Ok(IfStmt {
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
            })
        );
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
            Token::Keyword(Keyword::If),
            Token::Ident("a".to_string()),
            Token::RelOp(RelOp::Gt),
            Token::Number(0),
            Token::Keyword(Keyword::Then),
            Token::Keyword(Keyword::Let),
            Token::Ident("b".to_string()),
            Token::AssignOp,
            Token::Number(1),
            Token::Keyword(Keyword::Else),
            Token::Keyword(Keyword::Let),
            Token::Ident("b".to_string()),
            Token::AssignOp,
            Token::Number(2),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Let),
            Token::Ident("c".to_string()),
            Token::AssignOp,
            Token::Ident("a".to_string()),
            Token::Punctuation('+'),
            Token::Ident("b".to_string()),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Fi),
        ]);
        let mut parser = Parser::debug(tokens).unwrap();
        parser.sym_table.insert_var(SymbolTable::DEBUG_SCOPE, "a".to_string()).unwrap();
        parser.sym_table.insert_var(SymbolTable::DEBUG_SCOPE, "b".to_string()).unwrap();
        parser.sym_table.insert_var(SymbolTable::DEBUG_SCOPE, "c".to_string()).unwrap();

        assert_eq!(
            parser.parse_if_stmt(SymbolTable::DEBUG_SCOPE),
            Ok(IfStmt {
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
                                ops: vec![(
                                    TermOp::Add,
                                    Term {
                                        root: Factor::VarRef("b".to_string()),
                                        ops: vec![],
                                    }
                                )],
                            },
                        }),
                    ],
                }),
            })
        );
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
            Token::Keyword(Keyword::If),
            Token::Ident("a".to_string()),
            Token::RelOp(RelOp::Gt),
            Token::Number(0),
            Token::Keyword(Keyword::Then),
            Token::Keyword(Keyword::Let),
            Token::Ident("b".to_string()),
            Token::AssignOp,
            Token::Number(1),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Let),
            Token::Ident("c".to_string()),
            Token::AssignOp,
            Token::Ident("a".to_string()),
            Token::Punctuation('+'),
            Token::Ident("b".to_string()),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Fi),
        ]);
        let mut parser = Parser::debug(tokens).unwrap();
        parser.sym_table.insert_var(SymbolTable::DEBUG_SCOPE, "a".to_string()).unwrap();
        parser.sym_table.insert_var(SymbolTable::DEBUG_SCOPE, "b".to_string()).unwrap();
        parser.sym_table.insert_var(SymbolTable::DEBUG_SCOPE, "c".to_string()).unwrap();

        assert_eq!(
            parser.parse_if_stmt(SymbolTable::DEBUG_SCOPE),
            Ok(IfStmt {
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
                                ops: vec![(
                                    TermOp::Add,
                                    Term {
                                        root: Factor::VarRef("b".to_string()),
                                        ops: vec![],
                                    }
                                )],
                            },
                        }),
                    ],
                },

                else_block: None,
            })
        );
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
            Token::Keyword(Keyword::If),
            Token::Ident("a".to_string()),
            Token::RelOp(RelOp::Gt),
            Token::Number(0),
            Token::Keyword(Keyword::Then),
            Token::Keyword(Keyword::Let),
            Token::Ident("b".to_string()),
            Token::AssignOp,
            Token::Number(1),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Let),
            Token::Ident("c".to_string()),
            Token::AssignOp,
            Token::Ident("a".to_string()),
            Token::Punctuation('+'),
            Token::Ident("b".to_string()),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Else),
            Token::Keyword(Keyword::Let),
            Token::Ident("b".to_string()),
            Token::AssignOp,
            Token::Number(2),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Fi),
        ]);
        let mut parser = Parser::debug(tokens).unwrap();
        parser.sym_table.insert_var(SymbolTable::DEBUG_SCOPE, "a".to_string()).unwrap();
        parser.sym_table.insert_var(SymbolTable::DEBUG_SCOPE, "b".to_string()).unwrap();
        parser.sym_table.insert_var(SymbolTable::DEBUG_SCOPE, "c".to_string()).unwrap();

        assert_eq!(
            parser.parse_if_stmt(SymbolTable::DEBUG_SCOPE),
            Ok(IfStmt {
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
                                ops: vec![(
                                    TermOp::Add,
                                    Term {
                                        root: Factor::VarRef("b".to_string()),
                                        ops: vec![],
                                    }
                                )],
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
            })
        );
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
            Token::Keyword(Keyword::If),
            Token::Ident("a".to_string()),
            Token::RelOp(RelOp::Gt),
            Token::Number(0),
            Token::Keyword(Keyword::Then),
            Token::Keyword(Keyword::Let),
            Token::Ident("b".to_string()),
            Token::AssignOp,
            Token::Number(1),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Let),
            Token::Ident("c".to_string()),
            Token::AssignOp,
            Token::Ident("a".to_string()),
            Token::Punctuation('+'),
            Token::Ident("b".to_string()),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Else),
            Token::Keyword(Keyword::Let),
            Token::Ident("b".to_string()),
            Token::AssignOp,
            Token::Number(2),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Let),
            Token::Ident("c".to_string()),
            Token::AssignOp,
            Token::Ident("a".to_string()),
            Token::Punctuation('*'),
            Token::Number(2),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Fi),
        ]);
        let mut parser = Parser::debug(tokens).unwrap();
        parser.sym_table.insert_var(SymbolTable::DEBUG_SCOPE, "a".to_string()).unwrap();
        parser.sym_table.insert_var(SymbolTable::DEBUG_SCOPE, "b".to_string()).unwrap();
        parser.sym_table.insert_var(SymbolTable::DEBUG_SCOPE, "c".to_string()).unwrap();

        assert_eq!(
            parser.parse_if_stmt(SymbolTable::DEBUG_SCOPE),
            Ok(IfStmt {
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
                                ops: vec![(
                                    TermOp::Add,
                                    Term {
                                        root: Factor::VarRef("b".to_string()),
                                        ops: vec![],
                                    }
                                )],
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
            })
        );
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
            Token::Keyword(Keyword::If),
            Token::Ident("a".to_string()),
            Token::Punctuation('+'),
            Token::Number(1),
            Token::RelOp(RelOp::Gt),
            Token::Ident("b".to_string()),
            Token::Punctuation('*'),
            Token::Number(2),
            Token::Punctuation('+'),
            Token::Ident("c".to_string()),
            Token::Keyword(Keyword::Then),
            Token::Keyword(Keyword::Let),
            Token::Ident("result".to_string()),
            Token::AssignOp,
            Token::Number(0),
            Token::Keyword(Keyword::Fi),
        ]);
        let mut parser = Parser::debug(tokens).unwrap();
        parser.sym_table.insert_var(SymbolTable::DEBUG_SCOPE, "a".to_string()).unwrap();
        parser.sym_table.insert_var(SymbolTable::DEBUG_SCOPE, "b".to_string()).unwrap();
        parser.sym_table.insert_var(SymbolTable::DEBUG_SCOPE, "c".to_string()).unwrap();
        parser.sym_table.insert_var(SymbolTable::DEBUG_SCOPE, "result".to_string()).unwrap();

        assert_eq!(
            parser.parse_if_stmt(SymbolTable::DEBUG_SCOPE),
            Ok(IfStmt {
                condition: Relation {
                    lhs: Expr {
                        root: Term {
                            root: Factor::VarRef("a".to_string()),
                            ops: vec![],
                        },
                        ops: vec![(
                            TermOp::Add,
                            Term {
                                root: Factor::Number(1),
                                ops: vec![],
                            }
                        )],
                    },
                    rhs: Expr {
                        root: Term {
                            root: Factor::VarRef("b".to_string()),
                            ops: vec![(FactorOp::Mul, Factor::Number(2))],
                        },
                        ops: vec![(
                            TermOp::Add,
                            Term {
                                root: Factor::VarRef("c".to_string()),
                                ops: vec![],
                            }
                        )],
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
            })
        );
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
            Token::Keyword(Keyword::If),
            Token::Ident("a".to_string()),
            Token::RelOp(RelOp::Gt),
            Token::Number(0),
            Token::Keyword(Keyword::Then),
            Token::Keyword(Keyword::Let),
            Token::Ident("b".to_string()),
            Token::AssignOp,
            Token::Number(1),
            Token::Keyword(Keyword::Else),
            Token::Keyword(Keyword::Let),
            Token::Ident("b".to_string()),
            Token::AssignOp,
            Token::Number(2),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Fi),
        ]);
        let mut parser = Parser::debug(tokens).unwrap();
        parser.sym_table.insert_var(SymbolTable::DEBUG_SCOPE, "a".to_string()).unwrap();
        parser.sym_table.insert_var(SymbolTable::DEBUG_SCOPE, "b".to_string()).unwrap();

        assert_eq!(
            parser.parse_stmt(SymbolTable::DEBUG_SCOPE),
            Ok(Stmt::If(IfStmt {
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
            }))
        );
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
            Token::Keyword(Keyword::While),
            Token::Ident("a".to_string()),
            Token::RelOp(RelOp::Gt),
            Token::Number(0),
            Token::Keyword(Keyword::Do),
            Token::Keyword(Keyword::Let),
            Token::Ident("a".to_string()),
            Token::AssignOp,
            Token::Ident("a".to_string()),
            Token::Punctuation('-'),
            Token::Number(1),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Od),
        ]);
        let mut parser = Parser::debug(tokens).unwrap();
        parser.sym_table.insert_var(SymbolTable::DEBUG_SCOPE, "a".to_string()).unwrap();

        assert_eq!(
            parser.parse_loop(SymbolTable::DEBUG_SCOPE),
            Ok(Loop {
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
                    body: vec![Stmt::Assignment(Assignment {
                        place: "a".to_string(),
                        value: Expr {
                            root: Term {
                                root: Factor::VarRef("a".to_string()),
                                ops: vec![],
                            },
                            ops: vec![(
                                TermOp::Sub,
                                Term {
                                    root: Factor::Number(1),
                                    ops: vec![],
                                }
                            )],
                        },
                    }),],
                },
            })
        );
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
            Token::Keyword(Keyword::While),
            Token::Ident("a".to_string()),
            Token::RelOp(RelOp::Gt),
            Token::Number(0),
            Token::Keyword(Keyword::Do),
            Token::Keyword(Keyword::Let),
            Token::Ident("b".to_string()),
            Token::AssignOp,
            Token::Ident("b".to_string()),
            Token::Punctuation('+'),
            Token::Ident("a".to_string()),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Let),
            Token::Ident("a".to_string()),
            Token::AssignOp,
            Token::Ident("a".to_string()),
            Token::Punctuation('-'),
            Token::Number(1),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Od),
        ]);
        let mut parser = Parser::debug(tokens).unwrap();
        parser.sym_table.insert_var(SymbolTable::DEBUG_SCOPE, "a".to_string()).unwrap();
        parser.sym_table.insert_var(SymbolTable::DEBUG_SCOPE, "b".to_string()).unwrap();

        assert_eq!(
            parser.parse_loop(SymbolTable::DEBUG_SCOPE),
            Ok(Loop {
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
                                ops: vec![(
                                    TermOp::Add,
                                    Term {
                                        root: Factor::VarRef("a".to_string()),
                                        ops: vec![],
                                    }
                                )],
                            },
                        }),
                        Stmt::Assignment(Assignment {
                            place: "a".to_string(),
                            value: Expr {
                                root: Term {
                                    root: Factor::VarRef("a".to_string()),
                                    ops: vec![],
                                },
                                ops: vec![(
                                    TermOp::Sub,
                                    Term {
                                        root: Factor::Number(1),
                                        ops: vec![],
                                    }
                                )],
                            },
                        }),
                    ],
                },
            })
        );
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
            Token::Keyword(Keyword::While),
            Token::Ident("a".to_string()),
            Token::Punctuation('-'),
            Token::Ident("b".to_string()),
            Token::RelOp(RelOp::Gt),
            Token::Ident("c".to_string()),
            Token::Punctuation('*'),
            Token::Number(2),
            Token::Keyword(Keyword::Do),
            Token::Keyword(Keyword::Let),
            Token::Ident("a".to_string()),
            Token::AssignOp,
            Token::Ident("a".to_string()),
            Token::Punctuation('-'),
            Token::Ident("b".to_string()),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Od),
        ]);
        let mut parser = Parser::debug(tokens).unwrap();
        parser.sym_table.insert_var(SymbolTable::DEBUG_SCOPE, "a".to_string()).unwrap();
        parser.sym_table.insert_var(SymbolTable::DEBUG_SCOPE, "b".to_string()).unwrap();
        parser.sym_table.insert_var(SymbolTable::DEBUG_SCOPE, "c".to_string()).unwrap();

        assert_eq!(
            parser.parse_loop(SymbolTable::DEBUG_SCOPE),
            Ok(Loop {
                condition: Relation {
                    lhs: Expr {
                        root: Term {
                            root: Factor::VarRef("a".to_string()),
                            ops: vec![],
                        },
                        ops: vec![(
                            TermOp::Sub,
                            Term {
                                root: Factor::VarRef("b".to_string()),
                                ops: vec![],
                            }
                        )],
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
                    body: vec![Stmt::Assignment(Assignment {
                        place: "a".to_string(),
                        value: Expr {
                            root: Term {
                                root: Factor::VarRef("a".to_string()),
                                ops: vec![],
                            },
                            ops: vec![(
                                TermOp::Sub,
                                Term {
                                    root: Factor::VarRef("b".to_string()),
                                    ops: vec![],
                                }
                            )],
                        },
                    })],
                },
            })
        );
    }

    #[test]
    fn parse_loop_as_stmt() {
        /*
            while a > 0
            do
                let a <- a - 1;
            od
        */

        let tokens = stream_from_tokens(vec![
            Token::Keyword(Keyword::While),
            Token::Ident("a".to_string()),
            Token::RelOp(RelOp::Gt),
            Token::Number(0),
            Token::Keyword(Keyword::Do),
            Token::Keyword(Keyword::Let),
            Token::Ident("a".to_string()),
            Token::AssignOp,
            Token::Ident("a".to_string()),
            Token::Punctuation('-'),
            Token::Number(1),
            Token::Punctuation(';'),
            Token::Keyword(Keyword::Od),
        ]);
        let mut parser = Parser::debug(tokens).unwrap();
        parser.sym_table.insert_var(SymbolTable::DEBUG_SCOPE, "a".to_string()).unwrap();

        assert_eq!(
            parser.parse_stmt(SymbolTable::DEBUG_SCOPE),
            Ok(Stmt::Loop(Loop {
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
                    body: vec![Stmt::Assignment(Assignment {
                        place: "a".to_string(),
                        value: Expr {
                            root: Term {
                                root: Factor::VarRef("a".to_string()),
                                ops: vec![],
                            },
                            ops: vec![(
                                TermOp::Sub,
                                Term {
                                    root: Factor::Number(1),
                                    ops: vec![],
                                }
                            )],
                        },
                    }),],
                },
            }))
        );
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
        let mut parser = Parser::new(tokens).unwrap();

        assert_eq!(
            parser.parse_relation(),
            Ok(Relation {
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
            })
        );
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
        let mut parser = Parser::new(tokens).unwrap();

        assert_eq!(
            parser.parse_relation(),
            Ok(Relation {
                lhs: Expr {
                    root: Term {
                        root: Factor::VarRef(var1),
                        ops: vec![],
                    },
                    ops: vec![(
                        TermOp::Sub,
                        Term {
                            root: Factor::Number(1),
                            ops: vec![],
                        }
                    )],
                },
                rhs: Expr {
                    root: Term {
                        root: Factor::SubExpr(Box::new(Expr {
                            root: Term {
                                root: Factor::VarRef(var2),
                                ops: vec![],
                            },
                            ops: vec![(
                                TermOp::Add,
                                Term {
                                    root: Factor::Number(1),
                                    ops: vec![],
                                }
                            )],
                        })),
                        ops: vec![(FactorOp::Mul, Factor::Number(2))],
                    },
                    ops: vec![],
                },
                op,
            })
        );
    }

    #[test]
    fn parse_return_no_val() {
        let tokens = stream_from_tokens(vec![Token::Keyword(Keyword::Return)]);
        let mut parser = Parser::new(tokens).unwrap();

        assert_eq!(parser.parse_return(), Ok(Return { value: None }));
    }

    #[test]
    fn parse_return_with_val() {
        // return a + 1
        let tokens = stream_from_tokens(vec![
            Token::Keyword(Keyword::Return),
            Token::Ident("a".to_string()),
            Token::Punctuation('+'),
            Token::Number(1),
        ]);
        let mut parser = Parser::new(tokens).unwrap();

        assert_eq!(
            parser.parse_return(),
            Ok(Return {
                value: Some(Expr {
                    root: Term {
                        root: Factor::VarRef("a".to_string()),
                        ops: vec![],
                    },
                    ops: vec![(
                        TermOp::Add,
                        Term {
                            root: Factor::Number(1),
                            ops: vec![],
                        }
                    )],
                }),
            })
        );
    }

    #[test]
    fn parse_return_as_stmt() {
        // return a + 1
        let tokens = stream_from_tokens(vec![
            Token::Keyword(Keyword::Return),
            Token::Ident("a".to_string()),
            Token::Punctuation('+'),
            Token::Number(1),
        ]);
        let mut parser = Parser::debug(tokens).unwrap();

        assert_eq!(
            parser.parse_stmt(SymbolTable::DEBUG_SCOPE),
            Ok(Stmt::Return(Return {
                value: Some(Expr {
                    root: Term {
                        root: Factor::VarRef("a".to_string()),
                        ops: vec![],
                    },
                    ops: vec![(
                        TermOp::Add,
                        Term {
                            root: Factor::Number(1),
                            ops: vec![],
                        }
                    )],
                }),
            }))
        );
    }

    #[test]
    fn parse_term_single_factor() {
        let n = 5;
        let tokens = stream_from_tokens(vec![Token::Number(n)]);
        let mut parser = Parser::new(tokens).unwrap();

        assert_eq!(
            parser.parse_term(),
            Ok(Term {
                root: Factor::Number(n),
                ops: vec![],
            })
        );
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
        let mut parser = Parser::new(tokens).unwrap();

        assert_eq!(
            parser.parse_term(),
            Ok(Term {
                root: Factor::Number(n),
                ops: vec![(op, Factor::Number(m))],
            })
        );
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
        let mut parser = Parser::new(tokens).unwrap();

        assert_eq!(
            parser.parse_term(),
            Ok(Term {
                root: Factor::Number(x),
                ops: vec![(op1, Factor::Number(y)), (op2, Factor::Number(z)),],
            })
        );
    }

    #[test]
    fn scanner_error_propagated() {
        let tokens = vec![Ok(Token::Keyword(Keyword::Main)), Err(InvalidCharError('!'))];
        let mut parser = Parser::new(tokens.into_iter()).unwrap();

        assert_eq!(parser.parse_computation(), Err(ParseError::InvalidChar(InvalidCharError('!'))));
    }
}
