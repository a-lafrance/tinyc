// what does a symbol table need to do?
// track 2 things:
    // in each scope, which variables have been defined
    // in the program, which functions have been defined
// this way, you can identify two kinds of errors:
    // 1: reference to undefined variable or function
    // 2: redefinition of variable or function, but this one's arguable because it wasn't specified in the lang doc
use std::collections::HashSet;
use maplit::hashset;

pub struct SymbolTable {
    funcs: HashSet<String>,
    scopes: Vec<HashSet<String>>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            funcs: hashset!{"InputNum".to_string(), "OutputNum".to_string(), "OutputNewLine".to_string()},
            scopes: vec![hashset!{}], // create empty global scope
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashSet::new());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn insert_var(&mut self, name: String) {
        self.scopes.last_mut().unwrap().insert(name);
    }

    pub fn contains_var(&self, name: &str) -> bool {
        self.scopes.iter().any(|s| s.contains(name))
    }

    pub fn insert_func(&mut self, name: String) {
        self.funcs.insert(name);
    }

    pub fn contains_func(&self, name: &str) -> bool {
        self.funcs.contains(name)
    }
}
