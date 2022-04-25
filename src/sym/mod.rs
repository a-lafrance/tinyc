pub mod info;

use std::{
    collections::HashMap,
    error::Error,
    fmt::{self, Display, Formatter},
};
use maplit::hashmap;
use crate::utils::Builtin;
use self::info::{FuncInfo, ScopeInfo};

pub struct SymbolTable {
    scopes: HashMap<String, ScopeInfo>
}

impl SymbolTable {
    #[cfg(test)]
    pub const DEBUG_SCOPE: &'static str = "!debug";

    pub fn new() -> SymbolTable {
        SymbolTable {
            scopes: hashmap!{
                Builtin::InputNum.to_string() => ScopeInfo::new(Some(FuncInfo {
                    returns_void: false,
                    n_params: 0,
                })),

                Builtin::OutputNum.to_string() => ScopeInfo::new(Some(FuncInfo {
                    returns_void: true,
                    n_params: 1,
                })),

                Builtin::OutputNewLine.to_string() => ScopeInfo::new(Some(FuncInfo {
                    returns_void: true,
                    n_params: 0,
                })),
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
            .map(|scope| {
                scope.vars.insert(name);
            })
    }

    pub fn contains_var(&self, scope: &str, name: &str) -> bool {
        self.scopes.get(scope)
            .map(|scope| scope.vars.contains(name))
            .unwrap_or(false)
    }

    pub fn insert_scope(&mut self, name: String) {
        self.scopes.insert(name, ScopeInfo::empty());
    }

    pub fn func_info(&self, name: &str) -> Option<&FuncInfo> {
        self.scopes.get(name).and_then(|scope| scope.func_info.as_ref())
    }

    pub fn set_func_info(&mut self, name: &str, info: FuncInfo) {
        if let Some(scope) = self.scopes.get_mut(name) {
            scope.func_info = Some(info);
        }
    }
}


#[derive(Clone, Debug, PartialEq)]
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
