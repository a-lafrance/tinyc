use std::{
    collections::{HashMap, HashSet},
    error::Error,
    fmt::{self, Display, Formatter},
};
use maplit::{hashmap, hashset};

pub struct SymbolTable {
    scopes: HashMap<String, HashSet<String>>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            scopes: hashmap!{
                "InputNum".to_string() => hashset!{},
                "OutputNum".to_string() => hashset!{},
                "OutputNewLine".to_string() => hashset!{},
            }
        }
    }

    pub fn insert_var(&mut self, func: &str, name: String) -> Result<(), UndefinedSymbolError> {
        self.scopes.get_mut(func)
            .ok_or_else(|| UndefinedSymbolError::RefToUndefinedFunc(func.to_string()))
            .map(|vars| {
                vars.insert(name);
            })
    }

    pub fn contains_var(&self, func: &str, name: &str) -> bool {
        self.scopes.get(func)
            .map(|vars| vars.contains(name))
            .unwrap_or(false)
    }

    pub fn insert_func(&mut self, name: String) {
        self.scopes.insert(name, hashset!{});
    }

    pub fn contains_func(&self, name: &str) -> bool {
        self.scopes.contains_key(name)
    }
}


pub struct SymbolContext {
    sym_table: SymbolTable,
    current_scope: String,
}

impl SymbolContext {
    pub fn new(sym_table: SymbolTable, current_scope: String) -> SymbolContext {
        SymbolContext { sym_table, current_scope }
    }

    pub fn sym_table(&self) -> &SymbolTable {
        &self.sym_table
    }

    pub fn sym_table_mut(&mut self) -> &mut SymbolTable {
        &mut self.sym_table
    }

    pub fn enter_scope(&mut self, scope: String) -> String {
        let prev_scope = self.current_scope.clone(); // FIXME: slight inefficiency
        self.current_scope = scope;

        prev_scope
    }

    pub fn contains_var_in_scope(&self, name: &str) -> bool {
        self.sym_table.contains_var(&self.current_scope, name)
    }

    pub fn insert_var_in_scope(&mut self, name: String) {
        self.sym_table.insert_var(&self.current_scope, name).ok(); // this will always be true
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
