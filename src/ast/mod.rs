#[derive(Clone, Debug, PartialEq)]
pub struct Computation {
    vars: Vec<VarDecl>,
    funcs: Vec<FuncDecl>,
    body: Block,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarDecl;

#[derive(Clone, Debug, PartialEq)]
pub struct FuncDecl;

#[derive(Clone, Debug, PartialEq)]
pub struct Block;
