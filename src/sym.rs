use std::{
    collections::{HashMap, HashSet},
    error::Error,
    fmt::{self, Display, Formatter},
};
use maplit::{hashmap, hashset};
use crate::utils::Builtin;

pub struct SymbolTable {
    scopes: HashMap<String, HashSet<String>>,
}

impl SymbolTable {
    #[cfg(test)]
    pub const DEBUG_SCOPE: &'static str = "!debug";

    pub fn new() -> SymbolTable {
        SymbolTable {
            scopes: hashmap!{
                Builtin::InputNum.to_string() => hashset!{},
                Builtin::OutputNum.to_string() => hashset!{},
                Builtin::OutputNewLine.to_string() => hashset!{},
            }
        }
    }

    #[cfg(test)]
    pub fn debug() -> SymbolTable {
        let mut sym_table = SymbolTable::new();
        sym_table.insert_scope(SymbolTable::DEBUG_SCOPE.to_string());

        sym_table
    }

    pub fn insert_var(&mut self, scope: &str, name: String) -> Result<(), UndefinedSymbolError> {
        self.scopes.get_mut(scope)
            .ok_or_else(|| UndefinedSymbolError::RefToUndefinedFunc(scope.to_string()))
            .map(|vars| {
                vars.insert(name);
            })
    }

    pub fn contains_var(&self, scope: &str, name: &str) -> bool {
        self.scopes.get(scope)
            .map(|vars| vars.contains(name))
            .unwrap_or(false)
    }

    pub fn insert_scope(&mut self, name: String) {
        self.scopes.insert(name, hashset!{});
    }

    pub fn contains_scope(&self, name: &str) -> bool {
        self.scopes.contains_key(name)
    }
}


#[derive(Debug, PartialEq)]
pub enum UndefinedSymbolError {
    RefToUndefinedVar(String),
    RefToUndefinedFunc(String),
}

impl Display for UndefinedSymbolError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            UndefinedSymbolError::RefToUndefinedVar(var) => write!(f, "reference to undeclared variable '{}'", var),
            UndefinedSymbolError::RefToUndefinedFunc(func) => write!(f, "reference to undefined function '{}'", func),
        }
    }
}

impl Error for UndefinedSymbolError { }
