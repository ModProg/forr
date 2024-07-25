use std::ops::{Bound, Range};

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

#[derive(Clone, Debug)]
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

impl Vars {
    #[allow(clippy::type_complexity)]
    pub fn split_tuple(&self) -> Option<((Var, (Bound<usize>, Bound<usize>)), Vars)> {
        let mut tuple = None;
        let mut meta = Vec::new();

        let Self::Plain(vars) = &self else { unreachable!() };

        for var in vars {
            match var {
                Vars::Function {
                    function: VarsFunction::Tuples(var, bounds),
                    ..
                } => tuple = Some((var.clone(), *bounds)),
                vars @ Vars::Single(var) if var.typ.is_meta() => meta.push(vars.clone()),
                _ => return None,
            }
        }

        tuple.map(|tuple| (tuple, Vars::Plain(meta)))
    }
}

#[derive(Clone, Debug)]
pub struct Var {
    pub name: Ident,
    pub typ: Typ,
    pub optional: bool,
    pub span: Range<Span>,
}

#[derive(Clone, Copy, PartialEq, Debug)]
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

#[derive(Clone, Debug)]
pub enum VarsFunction {
    Casing(Vec<(Ident, Casing)>),
    Tuples(Var, (Bound<usize>, Bound<usize>)),
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

#[derive(Debug, Clone, Copy)]
pub enum Prefix {
    Pound,
    Dollar,
}
impl Prefix {
    pub fn test(self) -> fn(&TokenTree) -> bool {
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
