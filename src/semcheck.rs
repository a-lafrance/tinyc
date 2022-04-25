use crate::{
    parser::{
        err::ParseError::*,
        FuncCallContext, ParseResult,
    },
    sym::{SymbolTable, UndefinedSymbolError::*},
};

pub fn var_is_declared(sym_table: &SymbolTable, scope: &str, var: &str) -> ParseResult<()> {
    if sym_table.contains_var(scope, var) {
        Ok(())
    } else {
        Err(UndefinedSymbol(RefToUndefinedVar(var.to_string())))
    }
}

pub fn is_valid_func_call(sym_table: &SymbolTable, func: &str, context: FuncCallContext) -> ParseResult<()> {
    match sym_table.func_info(func) {
        Some(func_info) => {
            let valid = match context {
                FuncCallContext::Expr => !func_info.returns_void,
                FuncCallContext::Stmt => func_info.returns_void,
            };

            if valid {
                Ok(())
            } else {
                Err(InvalidFuncCall(func.to_string(), context))
            }
        },

        None => Err(UndefinedSymbol(RefToUndefinedFunc(func.to_string()))),
    }
}
