use crate::{
    ast::FuncCall,
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

pub fn is_valid_func_call(sym_table: &SymbolTable, func: &FuncCall, context: FuncCallContext) -> ParseResult<()> {
    match sym_table.func_info(&func.name) {
        Some(func_info) => {
            let valid_return_type = match context {
                FuncCallContext::Expr => !func_info.returns_void,
                FuncCallContext::Stmt => func_info.returns_void,
            };

            if valid_return_type {
                if func.args.len() == func_info.n_params {
                    Ok(())
                } else {
                    Err(FuncCallWrongArgs(func.args.len(), func_info.n_params))
                }
            } else {
                Err(InvalidFuncCall(func.name.clone(), context))
            }
        },

        None => Err(UndefinedSymbol(RefToUndefinedFunc(func.name.clone()))),
    }
}
