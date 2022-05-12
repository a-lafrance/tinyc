pub mod dlx;

use std::{
    error::Error,
    fmt::{self, Display, Formatter},
    str::FromStr,
};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SupportedArch {
    Dlx
}

impl FromStr for SupportedArch {
    type Err = UnsupportedArch;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "dlx" => Ok(SupportedArch::Dlx),
            other => Err(UnsupportedArch(other.to_string())),
        }
    }
}


#[derive(Clone, Debug, Eq, PartialEq)]
pub struct UnsupportedArch(String);

impl Display for UnsupportedArch {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "unsupported architecture '{}'", self.0)
    }
}

impl Error for UnsupportedArch { }
