use std::{
    error::Error,
    fmt::{self, Display, Formatter},
};
use crate::{
    ast::{
        Assignment, Block, Computation, Expr, Factor, FactorOp, FuncCall, FuncDecl, IfStmt, Loop,
        Relation, Return, Stmt, Term, TermOp, VarDecl,
    },
    scanner::{InvalidCharError, TokenResult},
    semcheck,
    sym::{SymbolContext, SymbolTable, UndefinedSymbolError},
    tok::Token,
    utils::{Keyword, RelOp},
};

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, PartialEq)]
pub enum ParseError {
    InvalidChar(InvalidCharError), // still TODO: scanner error propagation
    ExpectedKeyword(Keyword),
    ExpectedIdentifier,
    ExpectedStatement,
    ExpectedPunctuation(char),
    ExpectedAssignOp,
    ExpectedRelOp,
    UndefinedSymbol(UndefinedSymbolError),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            ParseError::InvalidChar(e) => write!(f, "{}", e),
            ParseError::ExpectedKeyword(kw) => write!(f, "expected keyword '{}'", kw),
            ParseError::ExpectedIdentifier => write!(f, "expected identifier"),
            ParseError::ExpectedStatement => write!(f, "expected statement"),
            ParseError::ExpectedPunctuation(c) => write!(f, "expected '{}'", c),
            ParseError::ExpectedAssignOp => write!(f, "expected '<-'"),
            ParseError::ExpectedRelOp => write!(f, "expected relational operator: one of {}", RelOp::all_as_str()),
            ParseError::UndefinedSymbol(e) => write!(f, "{}", e),
        }
    }
}

impl Error for ParseError { }

impl From<InvalidCharError> for ParseError {
    fn from(e: InvalidCharError) -> Self {
        ParseError::InvalidChar(e)
    }
}

impl From<UndefinedSymbolError> for ParseError {
    fn from(e: UndefinedSymbolError) -> Self {
        ParseError::UndefinedSymbol(e)
    }
}


pub struct Parser<T: Iterator<Item = TokenResult>> {
    current: Option<TokenResult>,
    stream: T,
    sym_context: Option<SymbolContext>,
}

impl<T: Iterator<Item = TokenResult>> Parser<T> {
    pub fn new(mut stream: T) -> Self {
        Parser {
            current: stream.next(),
            stream,
            sym_context: None,
        }
    }

    pub fn peek(&self) -> Option<&TokenResult> {
        self.current.as_ref()
    }

    pub fn parse_assignment(&mut self) -> ParseResult<Assignment> {
        self.expect_keyword_or_err(Keyword::Let)?;
        let place = self.consume_ident_if_exists()?;

        if self.expect_assign_op() {
            let value = self.parse_expr()?;

            match self.sym_context {
                Some(ref sym_context) => semcheck::check_var_is_declared(sym_context, &place)?,
                None => eprintln!("no symbol context detected when checking assignment"),
            }

            Ok(Assignment { place, value })
        } else {
            Err(ParseError::ExpectedAssignOp)
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
        self.expect_keyword_or_err(Keyword::Main)?;

        let scope_name = Keyword::Main.to_string();
        let mut sym_table = SymbolTable::new();
        sym_table.insert_func(scope_name.clone());

        let sym_context = SymbolContext::new(sym_table, scope_name);
        self.sym_context = Some(sym_context);

        let mut vars = vec![];
        while let Some(Keyword::Var) = self.peek_keyword_if_exists() {
            vars.push(self.parse_var_decl()?);
        }

        let mut funcs = vec![];
        while !self.expect_punctuation_matching('{') {
            funcs.push(self.parse_func_decl()?);
        }

        let body = self.parse_block()?;

        self.expect_punctuation_or_err('}')?;
        self.expect_punctuation_or_err('.')?;

        Ok(Computation { vars, funcs, body })
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

            self.expect_punctuation_or_err(')')
                .map(|_| Factor::SubExpr(subexpr))
        } else if let Some(n) = self.consume_number_if_exists() {
            Ok(Factor::Number(n))
        } else {
            match self.peek_keyword_if_exists() {
                Some(Keyword::Call) => Ok(Factor::Call(self.parse_func_call()?)),
                _ => Ok(Factor::VarRef(self.consume_ident_if_exists()?)),
            }
        }
    }

    pub fn parse_func_call(&mut self) -> ParseResult<FuncCall> {
        self.expect_keyword_or_err(Keyword::Call)?;
        let name = self.consume_ident_if_exists()?;
        let mut args = vec![];

        if self.expect_punctuation_matching('(') && !self.expect_punctuation_matching(')') {
            args.push(self.parse_expr()?);

            while !self.expect_punctuation_matching(')') {
                self.expect_punctuation_matching(',');
                args.push(self.parse_expr()?);
            }
        }

        match self.sym_context {
            Some(ref sym_context) => semcheck::check_func_is_defined(sym_context, &name)?,
            None => eprintln!("no symbol context detected when checking function call"),
        }

        Ok(FuncCall { name, args })
    }

    pub fn parse_func_decl(&mut self) -> ParseResult<FuncDecl> {
        let returns_void = self.expect_keyword(Keyword::Void);
        self.expect_keyword_or_err(Keyword::Function)?;
        let name = self.consume_ident_if_exists()?;
        self.expect_punctuation_or_err('(')?;

        let params = if self.expect_punctuation_matching(')') {
            vec![]
        } else {
            let mut params = vec![self.consume_ident_if_exists()?];

            while !self.expect_punctuation_matching(')') {
                self.expect_punctuation_or_err(',')?;

                params.push(self.consume_ident_if_exists()?);
            }

            params
        };

        self.expect_punctuation_or_err(';')?;

        let prev_scope = match self.sym_context {
            Some(ref mut sym_context) => {
                sym_context.sym_table_mut().insert_func(name.clone());
                Some(sym_context.enter_scope(name.clone()))
            },

            None => {
                let mut sym_table = SymbolTable::new();
                sym_table.insert_func(name.clone());

                let sym_context = SymbolContext::new(sym_table, name.clone());
                self.sym_context = Some(sym_context);

                None
            },
        };

        let mut vars = vec![];

        while !self.expect_punctuation_matching('{') {
            vars.push(self.parse_var_decl()?);
        }

        let body = if self.expect_punctuation_matching('}') {
            Block::empty()
        } else {
            let body = self.parse_block()?;
            self.expect_punctuation_or_err('}')?;

            body
        };

        if let Some(prev_scope) = prev_scope {
            if let Some(ref mut sym_context) = self.sym_context {
                sym_context.enter_scope(prev_scope);
            }
        }

        self.expect_punctuation_or_err(';')
            .map(|_| FuncDecl { returns_void, name, params, vars, body })
    }

    pub fn parse_if_stmt(&mut self) -> ParseResult<IfStmt> {
        self.expect_keyword_or_err(Keyword::If)?;
        let condition = self.parse_relation()?;

        self.expect_keyword_or_err(Keyword::Then)?;
        let then_block = self.parse_block()?;

        let else_block = if self.expect_keyword(Keyword::Else) {
            Some(self.parse_block()?)
        } else {
            None
        };

        self.expect_keyword_or_err(Keyword::Fi)?;

        Ok(IfStmt {
            condition,
            then_block,
            else_block,
        })
    }

    pub fn parse_loop(&mut self) -> ParseResult<Loop> {
        self.expect_keyword_or_err(Keyword::While)?;
        let condition = self.parse_relation()?;

        self.expect_keyword_or_err(Keyword::Do)?;
        let body = self.parse_block()?;

        self.expect_keyword_or_err(Keyword::Od)?;
        Ok(Loop { condition, body })
    }

    pub fn parse_relation(&mut self) -> ParseResult<Relation> {
        let lhs = self.parse_expr()?;
        let op = self.expect_relop()?;
        let rhs = self.parse_expr()?;

        Ok(Relation { lhs, rhs, op })
    }

    pub fn parse_return(&mut self) -> ParseResult<Return> {
        self.expect_keyword_or_err(Keyword::Return)?;
        let value = self.parse_expr().ok(); // FIXME: this is probably very bad (lookahead?)

        Ok(Return { value })
    }

    pub fn parse_stmt(&mut self) -> ParseResult<Stmt> {
        match self.peek_keyword_if_exists() {
            Some(Keyword::Let) => self.parse_assignment().map(|a| a.into()),
            Some(Keyword::Call) => self.parse_func_call().map(|c| c.into()),
            Some(Keyword::If) => self.parse_if_stmt().map(|i| i.into()),
            Some(Keyword::While) => self.parse_loop().map(|l| l.into()),
            Some(Keyword::Return) => self.parse_return().map(|r| r.into()),
            _ => Err(ParseError::ExpectedStatement),
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

    pub fn parse_var_decl(&mut self) -> ParseResult<VarDecl> {
        self.expect_keyword_or_err(Keyword::Var)?;
        let var = self.consume_ident_if_exists()?;

        match self.sym_context {
            Some(ref mut sym_context) => sym_context.insert_var_in_scope(var.clone()),
            None => eprintln!("no symbol context detected when parsing variable declaration"),
        }

        let mut vars = vec![var];

        while !self.expect_punctuation_matching(';') {
            self.expect_punctuation_or_err(',')?;

            let var = self.consume_ident_if_exists()?;

            match self.sym_context {
                Some(ref mut sym_context) => sym_context.insert_var_in_scope(var.clone()),
                None => eprintln!("no symbol context detected when parsing variable declaration"),
            }

            vars.push(var);
        }

        Ok(VarDecl { vars })
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
            }
            _ => false,
        }
    }

    pub fn expect_keyword(&mut self, keyword: Keyword) -> bool {
        match self.peek_keyword_if_exists() {
            Some(kw) if kw == keyword => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    pub fn expect_keyword_or_err(&mut self, keyword: Keyword) -> ParseResult<()> {
        if self.expect_keyword(keyword) {
            Ok(())
        } else {
            Err(ParseError::ExpectedKeyword(keyword))
        }
    }

    pub fn expect_relop(&mut self) -> ParseResult<RelOp> {
        match self.current {
            Some(Ok(Token::RelOp(op))) => {
                self.advance();
                Ok(op)
            }

            _ => Err(ParseError::ExpectedRelOp),
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

    pub fn expect_punctuation_or_err(&mut self, c: char) -> ParseResult<()> {
        if self.expect_punctuation_matching(c) {
            Ok(())
        } else {
            Err(ParseError::ExpectedPunctuation(c))
        }
    }

    pub fn consume_number_if_exists(&mut self) -> Option<u32> {
        match self.current {
            Some(Ok(Token::Number(n))) => {
                self.advance();
                Some(n)
            }
            _ => None,
        }
    }

    pub fn consume_ident_if_exists(&mut self) -> ParseResult<String> {
        match self.current {
            Some(Ok(Token::Ident(_))) => match self.advance() {
                Some(Ok(Token::Ident(ident))) => Ok(ident),
                _ => unreachable!(),
            },
            _ => Err(ParseError::ExpectedIdentifier),
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

    pub fn peek_keyword_if_exists(&self) -> Option<Keyword> {
        match self.peek() {
            Some(Ok(Token::Keyword(kw))) => Some(*kw),
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
            Token::Keyword(Keyword::Let),
            Token::Ident(var.clone()),
            Token::AssignOp,
            Token::Number(val),
        ]);
        let mut parser = Parser::new(tokens);

        assert_eq!(
            parser.parse_assignment(),
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
        // result = a * 2 + b
        let tokens = stream_from_tokens(vec![
            Token::Keyword(Keyword::Let),
            Token::Ident("result".to_string()),
            Token::AssignOp,
            Token::Ident("a".to_string()),
            Token::Punctuation('*'),
            Token::Number(2),
            Token::Punctuation('+'),
            Token::Ident("b".to_string()),
        ]);
        let mut parser = Parser::new(tokens);

        assert_eq!(
            parser.parse_assignment(),
            Ok(Assignment {
                place: "result".to_string(),
                value: Expr {
                    root: Term {
                        root: Factor::VarRef("a".to_string()),
                        ops: vec![(FactorOp::Mul, Factor::Number(2))],
                    },
                    ops: vec![(
                        TermOp::Add,
                        Term {
                            root: Factor::VarRef("b".to_string()),
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
        let mut parser = Parser::new(tokens);

        assert_eq!(
            parser.parse_stmt(),
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
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_computation(), Ok(Computation {
            vars: vec![VarDecl { vars: vec!["x".to_string()] }],
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
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_computation(), Ok(Computation {
            vars: vec![
                VarDecl { vars: vec!["a".to_string(), "b".to_string()] },
                VarDecl { vars: vec!["c".to_string()] },
            ],
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
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_computation(), Ok(Computation {
            vars: vec![],
            funcs: vec![
                FuncDecl {
                    returns_void: false,
                    name: "add".to_string(),
                    params: vec!["x".to_string(), "y".to_string()],
                    vars: vec![],
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
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_computation(), Ok(Computation {
            vars: vec![
                VarDecl { vars: vec!["x".to_string(), "y".to_string()] },
            ],
            funcs: vec![
                FuncDecl {
                    returns_void: false,
                    name: "add".to_string(),
                    params: vec!["x".to_string(), "y".to_string()],
                    vars: vec![],
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
                    vars: vec![],
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
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_computation(), Ok(Computation {
            vars: vec![
                VarDecl { vars: vec!["in".to_string(), "cmp".to_string(), "result".to_string()] },
            ],
            funcs: vec![
                FuncDecl {
                    returns_void: false,
                    name: "square".to_string(),
                    params: vec!["n".to_string()],
                    vars: vec![],
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
                    vars: vec![],
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
        let mut parser = Parser::new(tokens);

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
        let mut parser = Parser::new(tokens);

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
        let mut parser = Parser::new(tokens);

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
        let func = "double".to_string();
        let arg = 5;
        let tokens = stream_from_tokens(vec![
            Token::Keyword(Keyword::Call),
            Token::Ident(func.clone()),
            Token::Punctuation('('),
            Token::Number(arg),
            Token::Punctuation(')'),
        ]);
        let mut parser = Parser::new(tokens);

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
        // call run
        let func = "run".to_string();
        let tokens = stream_from_tokens(vec![
            Token::Keyword(Keyword::Call),
            Token::Ident(func.clone()),
        ]);
        let mut parser = Parser::new(tokens);

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
        // call run()
        let func = "run".to_string();
        let tokens = stream_from_tokens(vec![
            Token::Keyword(Keyword::Call),
            Token::Ident(func.clone()),
            Token::Punctuation('('),
            Token::Punctuation(')'),
        ]);
        let mut parser = Parser::new(tokens);

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
        // call square(3)
        let func = "square".to_string();
        let arg = 3;
        let tokens = stream_from_tokens(vec![
            Token::Keyword(Keyword::Call),
            Token::Ident(func.clone()),
            Token::Punctuation('('),
            Token::Number(arg),
            Token::Punctuation(')'),
        ]);
        let mut parser = Parser::new(tokens);

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
        let mut parser = Parser::new(tokens);

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
        let mut parser = Parser::new(tokens);

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
        // call square(3)
        let func = "square".to_string();
        let arg = 3;
        let tokens = stream_from_tokens(vec![
            Token::Keyword(Keyword::Call),
            Token::Ident(func.clone()),
            Token::Punctuation('('),
            Token::Number(arg),
            Token::Punctuation(')'),
        ]);
        let mut parser = Parser::new(tokens);

        assert_eq!(
            parser.parse_stmt(),
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
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_func_decl(), Ok(FuncDecl {
            returns_void: true,
            name: "empty".to_string(),
            params: vec![],
            vars: vec![],
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
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_func_decl(), Ok(FuncDecl {
            returns_void: false,
            name: "const".to_string(),
            params: vec![],
            vars: vec![],
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
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_func_decl(), Ok(FuncDecl {
            returns_void: false,
            name: "square".to_string(),
            params: vec!["n".to_string()],
            vars: vec![],
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
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_func_decl(), Ok(FuncDecl {
            returns_void: false,
            name: "add".to_string(),
            params: vec!["x".to_string(), "y".to_string(), "z".to_string()],
            vars: vec![],
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
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_func_decl(), Ok(FuncDecl {
            returns_void: true,
            name: "assign".to_string(),
            params: vec![],
            vars: vec![VarDecl { vars: vec!["x".to_string()] }],
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
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_func_decl(), Ok(FuncDecl {
            returns_void: true,
            name: "add".to_string(),
            params: vec![],
            vars: vec![
                VarDecl { vars: vec!["x".to_string(), "y".to_string()] },
                VarDecl { vars: vec!["result".to_string()] },
            ],
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
        let mut parser = Parser::new(tokens);

        assert_eq!(parser.parse_func_decl(), Ok(FuncDecl {
            returns_void: false,
            name: "gtz".to_string(),
            params: vec!["n".to_string()],
            vars: vec![VarDecl { vars: vec!["result".to_string()] }],
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
        let mut parser = Parser::new(tokens);

        assert_eq!(
            parser.parse_if_stmt(),
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
        let mut parser = Parser::new(tokens);

        assert_eq!(
            parser.parse_if_stmt(),
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
        let mut parser = Parser::new(tokens);

        assert_eq!(
            parser.parse_if_stmt(),
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
        let mut parser = Parser::new(tokens);

        assert_eq!(
            parser.parse_if_stmt(),
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
        let mut parser = Parser::new(tokens);

        assert_eq!(
            parser.parse_if_stmt(),
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
        let mut parser = Parser::new(tokens);

        assert_eq!(
            parser.parse_if_stmt(),
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
        let mut parser = Parser::new(tokens);

        assert_eq!(
            parser.parse_if_stmt(),
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
        let mut parser = Parser::new(tokens);

        assert_eq!(
            parser.parse_stmt(),
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
        let mut parser = Parser::new(tokens);

        assert_eq!(
            parser.parse_loop(),
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
        let mut parser = Parser::new(tokens);

        assert_eq!(
            parser.parse_loop(),
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
        let mut parser = Parser::new(tokens);

        assert_eq!(
            parser.parse_loop(),
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
                    }),],
                },
            })
        );
    }

    #[test]
    fn parse_loop_as_stmt() {
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
        let mut parser = Parser::new(tokens);

        assert_eq!(
            parser.parse_stmt(),
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
        let mut parser = Parser::new(tokens);

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
        let mut parser = Parser::new(tokens);

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
        let mut parser = Parser::new(tokens);

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
        let mut parser = Parser::new(tokens);

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
        let mut parser = Parser::new(tokens);

        assert_eq!(
            parser.parse_stmt(),
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
        let mut parser = Parser::new(tokens);

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
        let mut parser = Parser::new(tokens);

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
        let mut parser = Parser::new(tokens);

        assert_eq!(
            parser.parse_term(),
            Ok(Term {
                root: Factor::Number(x),
                ops: vec![(op1, Factor::Number(y)), (op2, Factor::Number(z)),],
            })
        );
    }

    #[test]
    fn parse_var_decl_single_var() {
        let tokens = stream_from_tokens(vec![
            Token::Keyword(Keyword::Var),
            Token::Ident("asg".to_string()),
            Token::Punctuation(';'),
        ]);
        let mut parser = Parser::new(tokens);

        assert_eq!(
            parser.parse_var_decl(),
            Ok(VarDecl {
                vars: vec!["asg".to_string()],
            })
        );
    }

    #[test]
    fn parse_var_decl_many_vars() {
        let tokens = stream_from_tokens(vec![
            Token::Keyword(Keyword::Var),
            Token::Ident("x".to_string()),
            Token::Punctuation(','),
            Token::Ident("y".to_string()),
            Token::Punctuation(','),
            Token::Ident("z".to_string()),
            Token::Punctuation(';'),
        ]);
        let mut parser = Parser::new(tokens);

        assert_eq!(
            parser.parse_var_decl(),
            Ok(VarDecl {
                vars: vec!["x".to_string(), "y".to_string(), "z".to_string()],
            })
        );
    }
}
