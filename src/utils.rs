use std::fmt::{self, Display, Formatter};

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
