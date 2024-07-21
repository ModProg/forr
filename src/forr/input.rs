use std::ops::Range;

use proc_macro2::{Span, TokenStream, TokenTree};
use proc_macro_utils::TokenTreePunct;
use syn::Ident;

#[allow(clippy::module_name_repetitions)]
pub struct MacroInput {
    pub vars: Vars,
    pub values: Values,
    pub prefix: Prefix,
    pub loop_type: LoopType,
    pub body: TokenStream,
}

pub enum Vars {
    Single(Var),
    Plain(Vec<Vars>),
    Tuple(Box<Vars>),
    Function {
        name: Ident,
        function: VarsFunction,
        optional: bool,
    },
}

#[derive(Clone)]
pub struct Var {
    pub name: Ident,
    pub typ: Typ,
    pub optional: bool,
    pub span: Range<Span>,
}

#[derive(Clone, Copy, PartialEq)]
pub enum Typ {
    Expr,
    Ty,
    Tt,
    Inner,
    Idx,
    Ident,
    Block,
    Path,
    Lifetime,
    Literal,
}

pub enum VarsFunction {
    Casing(Vec<(Ident, Casing)>),
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Casing {
    s,
    S,
    c,
    C,
}

pub enum Values {
    List(TokenStream),
    Function(ValuesFunction),
}

#[derive(Clone, PartialEq)]
pub enum ValuesFunction {
    Ident(Ident, usize),
}

#[derive(Debug)]
pub enum Prefix {
    Pound,
    Dollar,
}
impl Prefix {
    pub fn test(&self) -> fn(&TokenTree) -> bool {
        match self {
            Prefix::Pound => TokenTree::is_pound,
            Prefix::Dollar => TokenTree::is_dollar,
        }
    }
}

pub enum LoopType {
    Star,
    Colon,
}
