// what does a symbol table need to do?
// track 2 things:
    // in each scope, which variables have been defined
    // in the program, which functions have been defined
// this way, you can identify two kinds of errors:
    // 1: reference to undefined variable or function
    // 2: redefinition of variable or function, but this one's arguable because it wasn't specified in the lang doc

// when to use the symbol table and how?
    // assignment: ensure that variable was declared
    // func call: ensure that the function exists
    // var decl: add variables to symbol table
    // func decl: add function to symbol table
    // func decl: enter func scope
        // MAKE SURE to save and restore previous scope
    // computation: enter main scope




use std::{
    collections::{HashMap, HashSet},
    error::Error,
    fmt::{self, Display, Formatter},
};
use maplit::{hashmap, hashset};
use crate::utils::Keyword;

pub struct SymbolTable {
    scopes: HashMap<String, HashSet<String>>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            scopes: hashmap!{
                Keyword::Main.to_string() => hashset!{},
                "InputNum".to_string() => hashset!{},
                "OutputNum".to_string() => hashset!{},
                "OutputNewLine".to_string() => hashset!{},
            }
        }
    }

    pub fn insert_var(&mut self, func: &str, name: String) -> Result<(), UndefinedSymbolError> {
        self.scopes.get_mut(func)
            .ok_or(UndefinedSymbolError::RefToUndefinedFunc(func.to_string()))
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

    pub fn enter_scope(&mut self, scope: String) {
        self.current_scope = scope;
    }

    pub fn contains_var_in_scope(&self, name: &str) -> bool {
        self.sym_table.contains_var(&self.current_scope, name)
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
