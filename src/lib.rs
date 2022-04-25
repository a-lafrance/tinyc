#![allow(clippy::needless_collect)] // because of the false positives

pub(crate) mod ast;
pub(crate) mod ir;
pub(crate) mod parser;
pub(crate) mod regalloc;
pub(crate) mod scanner;
pub(crate) mod semcheck;
pub(crate) mod sym;
pub(crate) mod tok;
pub(crate) mod utils;

pub mod driver;
