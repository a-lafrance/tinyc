use crate::sym::{SymbolTable, UndefinedSymbolError};

pub fn check_var_is_declared(sym_table: &SymbolTable, scope: &str, var: &str) -> Result<(), UndefinedSymbolError> {
    if sym_table.contains_var(scope, var) {
        Ok(())
    } else {
        Err(UndefinedSymbolError::RefToUndefinedVar(var.to_string()))
    }
}

pub fn check_func_is_defined(sym_table: &SymbolTable, func: &str) -> Result<(), UndefinedSymbolError> {
    if sym_table.contains_scope(func) {
        Ok(())
    } else {
        Err(UndefinedSymbolError::RefToUndefinedFunc(func.to_string()))
    }
}
