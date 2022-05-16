use std::{
    error::Error,
    fmt::{self, Display, Formatter},
    str::FromStr,
};
use super::Config;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct OptConfig {
    pub cse: bool,
    pub const_prop: bool,
    pub dead_code_elim: bool,
    pub instr_select: bool,
    pub reg_alloc: RegAllocator,
}

impl From<&Config> for OptConfig {
    fn from(cfg: &Config) -> OptConfig {
        OptConfig {
            cse: cfg.enable_cse || cfg.opt_level.enable_cse(),
            const_prop: cfg.enable_const_prop || cfg.opt_level.enable_const_prop(),
            dead_code_elim: cfg.enable_dead_code_elim || cfg.opt_level.enable_dead_code_elim(),
            instr_select: cfg.enable_instr_select || cfg.opt_level.enable_instr_select(),
            reg_alloc: cfg.reg_alloc.unwrap_or_else(|| cfg.opt_level.reg_alloc()),
        }
    }
}

impl From<OptLevel> for OptConfig {
    fn from(lvl: OptLevel) -> OptConfig {
        OptConfig {
            cse: lvl.enable_cse(),
            const_prop: lvl.enable_const_prop(),
            dead_code_elim: lvl.enable_dead_code_elim(),
            instr_select: lvl.enable_instr_select(),
        }
    }
}


#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum OptLevel {
    Bare, // bare opt enables no optimizations
    Default, // default opt enables CSE only
    Full, // full opt enables all optimizations
}

impl OptLevel {
    pub fn enable_cse(&self) -> bool {
        matches!(self, OptLevel::Default | OptLevel::Full)
    }

    pub fn enable_const_prop(&self) -> bool {
        matches!(self, OptLevel::Full)
    }

    pub fn enable_dead_code_elim(&self) -> bool {
        matches!(self, OptLevel::Full)
    }

    pub fn enable_instr_select(&self) -> bool {
        matches!(self, OptLevel::Full)
    }

    pub fn reg_alloc(&self) -> RegAllocator {
        match self {
            OptLevel::Bare => RegAllocator::Simple,
            OptLevel::Default | OptLevel::Full => RegAllocator::Coloring,
        }
    }
}

impl Default for OptLevel {
    fn default() -> OptLevel {
        OptLevel::Default
    }
}

impl Display for OptLevel {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            OptLevel::Bare => write!(f, "bare"),
            OptLevel::Default => write!(f, "default"),
            OptLevel::Full => write!(f, "full"),
        }
    }
}

impl FromStr for OptLevel {
    type Err = InvalidOptLevel;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "bare" => Ok(OptLevel::Bare),
            "default" => Ok(OptLevel::Default),
            "full" => Ok(OptLevel::Full),
            other => Err(InvalidOptLevel(other.to_string())),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct InvalidOptLevel(String);

impl Display for InvalidOptLevel {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "invalid opt level '{}'", self.0)
    }
}

impl Error for InvalidOptLevel { }


#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RegAllocator {
    Simple,
    Coloring,
}

impl Display for RegAllocator {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            RegAllocator::Simple => write!(f, "simple"),
            RegAllocator::Coloring => write!(f, "coloring"),
        }
    }
}

impl FromStr for RegAllocator {
    type Err = InvalidRegAllocator;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "simple" => Ok(RegAllocator::Simple),
            "coloring" => Ok(RegAllocator::Coloring),
            other => Err(InvalidRegAllocator(other.to_string())),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct InvalidRegAllocator(String);

impl Display for InvalidRegAllocator {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "invalid register allocator '{}'", self.0)
    }
}

impl Error for InvalidRegAllocator { }
