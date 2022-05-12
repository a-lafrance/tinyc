use std::collections::HashSet;
use maplit::hashset;
use crate::ast::FuncDecl;

#[derive(Clone, Debug)]
pub struct ScopeInfo {
    pub vars: HashSet<String>,
    pub func_info: Option<FuncInfo>,
}

impl ScopeInfo {
    pub fn new(func_info: Option<FuncInfo>) -> Self {
        ScopeInfo {
            vars: hashset!{},
            func_info,
        }
    }

    pub fn empty() -> Self {
        ScopeInfo {
            vars: hashset!{},
            func_info: None,
        }
    }
}


#[derive(Clone, Debug)]
pub struct FuncInfo {
    pub returns_void: bool,
    pub n_params: usize,
}

impl From<&FuncDecl> for FuncInfo {
    fn from(decl: &FuncDecl) -> FuncInfo {
        FuncInfo {
            returns_void: decl.returns_void,
            n_params: decl.params.len(),
        }
    }
}
