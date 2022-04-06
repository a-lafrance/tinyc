use crate::sym::{SymbolContext, UndefinedSymbolError};

pub fn check_var_is_declared(sym_context: &SymbolContext, var: &str) -> Result<(), UndefinedSymbolError> {
    if sym_context.contains_var_in_scope(var) {
        Ok(())
    } else {
        Err(UndefinedSymbolError::RefToUndefinedVar(var.to_string()))
    }
}

pub fn check_func_is_defined(sym_context: &SymbolContext, func: &str) -> Result<(), UndefinedSymbolError> {
    if sym_context.sym_table().contains_func(func) {
        Ok(())
    } else {
        Err(UndefinedSymbolError::RefToUndefinedFunc(func.to_string()))
    }
}
