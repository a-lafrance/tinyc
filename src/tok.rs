use std::fmt::{self, Display, Formatter};

#[derive(Debug, PartialEq)]
pub enum Token {
    Number(u32),
    Ident(String),
    RelOp(RelOp),
    Punctuation(char),
    AssignOp,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Token::Number(n) => write!(f, "{}", n),
            Token::Ident(ident) => write!(f, "{}", ident),
            Token::RelOp(op) => write!(f, "{}", op),
            Token::Punctuation(c) => write!(f, "{}", c),
            Token::AssignOp => write!(f, "<-"),
        }
    }
}

// TODO: move to different place?
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum RelOp {
    Eq,
    Ne,
    Gt,
    Ge,
    Lt,
    Le,
}

impl RelOp {
    pub fn all_as_str() -> String {
        format!(
            "{}, {}, {}, {}, {}, {}",
            RelOp::Eq, RelOp::Ne, RelOp::Gt, RelOp::Ge, RelOp::Lt, RelOp::Le,
        )
    }
}

impl Display for RelOp {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                RelOp::Eq => "==",
                RelOp::Ne => "!=",
                RelOp::Gt => ">",
                RelOp::Ge => ">=",
                RelOp::Lt => "<",
                RelOp::Le => "<=",
            }
        )
    }
}
