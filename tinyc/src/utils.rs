use std::{
    fmt::{self, Display, Formatter},
    mem,
    str::FromStr,
};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum RelOp {
    Eq, Ne, Gt, Ge, Lt, Le,
}

impl RelOp {
    pub fn all_as_str() -> String {
        format!(
            "{}, {}, {}, {}, {}, {}",
            RelOp::Eq, RelOp::Ne, RelOp::Gt, RelOp::Ge, RelOp::Lt, RelOp::Le,
        )
    }

    pub fn negated(&self) -> RelOp {
        match self {
            RelOp::Eq => RelOp::Ne,
            RelOp::Ne => RelOp::Eq,
            RelOp::Gt => RelOp::Le,
            RelOp::Ge => RelOp::Lt,
            RelOp::Lt => RelOp::Ge,
            RelOp::Le => RelOp::Gt,
        }
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


#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Keyword {
    Let, Call, If, Then, Else, Fi, While, Do, Od, Return, Void, Function, Main, Var
}

impl Display for Keyword {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let mut kw = format!("{:?}", self);
        kw.make_ascii_lowercase();

        write!(f, "{}", kw)
    }
}

impl FromStr for Keyword {
    type Err = ();

    fn from_str(s: &str) -> Result<Keyword, Self::Err> {
        match s {
            "let" => Ok(Keyword::Let),
            "call" => Ok(Keyword::Call),
            "if" => Ok(Keyword::If),
            "then" => Ok(Keyword::Then),
            "else" => Ok(Keyword::Else),
            "fi" => Ok(Keyword::Fi),
            "while" => Ok(Keyword::While),
            "do" => Ok(Keyword::Do),
            "od" => Ok(Keyword::Od),
            "return" => Ok(Keyword::Return),
            "void" => Ok(Keyword::Void),
            "function" => Ok(Keyword::Function),
            "main" => Ok(Keyword::Main),
            "var" => Ok(Keyword::Var),
            _ => Err(())
        }
    }
}


#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Builtin {
    InputNum, OutputNum, OutputNewLine,
}

impl Builtin {
    pub fn from(s: &str) -> Option<Builtin> {
        match s {
            "InputNum" => Some(Builtin::InputNum),
            "OutputNum" => Some(Builtin::OutputNum),
            "OutputNewLine" => Some(Builtin::OutputNewLine),
            _ => None,
        }
    }
}

impl Display for Builtin {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}


pub fn take_result<T: Default, E>(result: &mut Result<T, E>) -> Result<T, E> {
    mem::replace(result, Ok(T::default()))
}
