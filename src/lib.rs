#![warn(clippy::pedantic, missing_docs)]
#![allow(clippy::wildcard_imports)]
#![cfg_attr(docsrs, feature(doc_auto_cfg))]
//! A for loop in compile time
//!
//! Aims to replace single use [`macro_rules!`](https://doc.rust-lang.org/reference/macros-by-example.html) for the purpose to repeating code.
//!
//! For example it can reduce an implementation for multiple tuples:
//! ```
//! # use forr::forr;
//! # trait Like { fn like (&self, other: &Self) -> bool; }
//! # impl Like for i32 { fn like (&self, other: &Self) -> bool { self == other } }
//! forr! { $gens:inner in [(), (A,), (A, B), (A, B, C),
//!                         (A, B, C, D), (A, B, C, D, E)] $*
//!     // $idx is special this would be like (idx, gen) = [...].enumerate()
//!     forr! { $idx:idx, $gen:expr in [$gens] $:
//!         impl<$($gen: Like,)*> Like for ($gens) {
//!             fn like(&self, other: &Self) -> bool {
//!                 $(self.$idx.like(&other.$idx)&&)* true
//!             }
//!         }
//!     }
//! }
//!
//! assert!((1, 3).like(&(1, 3)));
//! assert!((1,).like(&(1,)));
//! assert!(().like(&()));
//! ```
//! With macro-rules this would be:
//! ```
//! # trait Like { fn like (&self, other: &Self) -> bool; }
//! # impl Like for i32 { fn like (&self, other: &Self) -> bool { self == other } }
//! macro_rules! impl_like_for_tuples {
//!     [$(($gen:ident, $idx:tt), $(($gens:ident, $idxs:tt)),*)?$(,)?] => {
//!         impl$(<$gen: Like, $($gens: Like),*>)? Like for ($($gen, $($gens),*)?) {
//!             fn like(&self, other: &Self) -> bool {
//!                 $(self.$idx.like(&other.$idx) &&)?
//!                 $($(self.$idxs.like(&other.$idxs) &&)*)?
//!                 true
//!             }
//!         }
//!         $(impl_like_for_tuples![$(($gens, $idxs)),*,];)?
//!     }
//! }
//! impl_like_for_tuples![(E, 4), (D, 3), (C, 2), (B, 1), (A, 0)];
//!
//! assert!((1, 3).like(&(1, 3)));
//! assert!((1,).like(&(1,)));
//! assert!(().like(&()));
//! ```
//!
//! Granted in this example it is not a lot more complicated, and adding more
//! tuple variants actually requires less code. But it took me quite a few more
//! trys getting it to work correctly. (If you don't count me implementing this
//! whole library for the first one.)
//!
//! # Usage
//!
//! The first part of the invocation is the pattern, similar to a normal `for`
//! loop in rust. Here you can use either a [single
//! variable](#single-variable-binding) i.e. `$name:type` or a [tuple
//! binding](#tuple-binding) `($name:type, $nbme:type, ...)`. There can
//! optionally be [non consuming patterns](#non-consuming-patterns) specified
//! before or after, currently that includes only [`:idx`](#idx).
//!
//! This is followed by the keyword `in`, an array literal `[...]` containing
//! the tokens to iterate and the [body](#body) marked with either `$*` or `$:`.
//!
//! ## Single variable binding
//! `$` [`name`](#names) `:` [`type`](#types)
//! ```
//! # use forr::forr;
//! forr! { $val:expr in [1, 2 + 4, 20]
//! # $* assert_eq!($val, $val); }
//! ```
//! `$val` will be `1`, `2 + 4` and `20`.
//!
//! ## Tuple binding
//! `(` [`$name:type`](#single-variable-binding), ... `)`
//! ```
//! # use forr::forr;
//! forr! { ($val:expr, $vbl:ty) in [(1, i32), (Ok(2 + 4), Result<u8, ()>), (20.0, f32)]
//! # $* let a: $vbl = $val; assert_eq!(a, $val); }
//! ```
//! `$val` will be `1`, `Ok(2 + 4)` and `20.0`.
//!
//! `$vbl` will be `i32`, `Result<u8, ()>` and `f32`.
//!
//! ## Non consuming patterns
//!
//! Non consuming patterns can be specified before or after the consuming
//! patterns or inside if using a tuple binding.
//!
//! Currently, only [`:idx`](#idx) is supported
//! ```
//! # use forr::forr;
//! forr! { $val:expr, $i:idx in [1, 2]
//! # $* assert_eq!($i + 1, $val); }
//! forr! { $i:idx, $val:expr in [1, 2]
//! # $* assert_eq!($i + 1, $val); }
//! forr! { $i:idx, ($val:expr, $vbl:ty) in [(1, i32), (2, i32)]
//! # $* assert_eq!($i + 1, $val); }
//! forr! { ($val:expr, $vbl:ty), $i:idx in [(1, i32), (2, i32)]
//! # $* assert_eq!($i + 1, $val); }
//! forr! { ($val:expr, $i:idx, $vbl:ty) in [(1, i32), (2, i32)]
//! # $* assert_eq!($i + 1, $val); }
//! ```
//! `$val` will be `1` and `2`
//!
//! `$i` will be `0` and `1`
//!
//! `$vbl` will be `i32`
//!
//! ## Body
//!
//! The body can be in two different modes. When it is initialized with `$*` the
//! whole body is repeated similar to a normal for loop. Is it started with
//! `$:`, the body will behave more like macro expansion using `$()*` for
//! repetition. In both cases there is special handling for [optional
//! values](#optional values) when placed inside `$()?` the innermost such group
//! is only added if the value is present.
//!
//! ### `$*` outer repetition
//!
//! In the tokens following the `$*` every occurrence of a `$ident` where the
//! ident matches one of the declared variables is replaced with the
//! corresponding value.
//! ```
//! # use forr::forr;
//! forr! {$val:expr in [(1, "a", true)] $*
//!     assert_eq!($val, $val);
//! }
//! ```
//! will expand to
//! ```
//! assert_eq!(1, 1);
//! assert_eq!("a", "a");
//! assert_eq!(true, true);
//! ```
//!
//! ### `$:` inner repetition
//!
//! `$:` allows to have non repeated code surrounding the expansion, mainly
//! useful for cases where a macro would not be allowed.
//!
//! ```
//! # use forr::forr;
//! # assert!(!
//! forr! {($pat:expr, $res:expr) in [(0, true), (1, false), (2.., true)] $:
//!     match 1u8 {
//!         $($pat => $res,)*
//!     }
//! }
//! # );
//! ```
//! Without the inner repetition this would not be possible, as macros are not
//! allowed as the body of a `match`.
//! ```compile_fail
//! # use forr::forr;
//! # assert!(!
//! match 1 {
//!    forr! {($pat:expr, $res:expr) in [(0, true), (1, false), (2.., true)] $*
//!        $pat => $res
//!    }
//! }
//! # );
//! ```
//!
//! # Names
//! Any valid rust idents including keywords can be used to name variables. Note
//! that shadowing does not work, i.e. an inner `forr!` needs to use different
//! names.
//!
//! # Types
//! ## `:expr`
//! As this uses [`TokenParser::next_expression()`] this will allow anything
//! that matches the `,` rules for expressions i.e. it cannot contain `;` and no
//! `,` outside turbofishes for Types/Generics (`HashMap::<A, B>`).
//!
//! ## `:ty`
//! As this uses [`TokenParser::next_type()`] this will allow anything
//! that matches the `,` rules for types i.e. it cannot contain `;` and no
//! `,` outside `<>` for Generics (`HashMap<A, B>`) and unopened closing `>`.
//!
//! ## `:tt`
//! Currently matches exactly one [`proc_macro::TokenTree`], but the plan is to extend this to what [`macro_rule!`'s `:tt` matches](https://doc.rust-lang.org/reference/macros-by-example.html#metavariables).
//!
//! ## `:inner`
//! The most versatile type, allowing arbitrary tokens wrapped by any bracket
//! `[`, `{` or `(` ... `)}]`.
//!
//! ## `:idx`
//! [Non consuming pattern](#non-consuming-patterns) that will contain the
//! current index.

use std::iter;
use std::ops::BitAndAssign;

use manyhow::{
    bail, error_message, manyhow, Error, ErrorMessage, JoinToTokensError, Result, SpanRanged,
};
use proc_macro2::{token_stream, Group, Ident, Literal, Span, TokenStream, TokenTree};
use proc_macro_utils::{Delimited, TokenParser, TokenStream2Ext, TokenTreePunct};
use quote::{quote, ToTokens};

type Parser = TokenParser<token_stream::IntoIter>;

#[rustfmt::skip]
macro_rules! unwrap_or {
    ($ident:tt, $expr:expr, $parser:expr, $expected:literal) => {
        let Some($ident) = $expr else {
            if let Some(token) = $parser.next() {
                bail!(token, "unexpected token: expected {}", $expected)
            }
            bail!("unexpected end of macro invocation: expected to be followed by {}", $expected)
        };
    };
}

macro_rules! bail_optional_span {
    ($parser:expr, $($fmt:tt)*) => {
        if let Some(token) = $parser.next() {
            bail!(token, "unexpected token: {}", format_args!($($fmt)*));
        }
        bail!("unexpected end of macro invocation: {}", format_args!($($fmt)*));
    };
}

/// Iterates over specified list and expands input similar to macro_rules
#[manyhow(impl_fn)]
#[proc_macro]
pub fn forr(input: TokenStream) -> Result {
    let mut context = Context::default();
    let mut input = input.parser();
    // parse variables
    let vars = Vars::parse(&mut input)?;
    for var in vars.idents() {
        context.push_key(var)?;
    }
    // ... in
    unwrap_or!(_, input.next_keyword("in"), input, "`in`");
    // ... in [...]
    unwrap_or!(values, input.next_bracketed(), input, "`[...]`");
    let mut values = values.stream().parser();
    while !values.is_empty() {
        context.new_row();
        vars.parse_values(&mut values, &mut context, None)?;
    }

    // $*
    if input
        .next_if_each((TokenTree::is_dollar, TokenTree::is_asterix))
        .is_some()
    {
        Ok(replace_loop(input, &context))
    } else if input
        .next_if_each((TokenTree::is_dollar, TokenTree::is_colon))
        .is_some()
    {
        replace_outer(input, &context)
    } else if let Some(token) = input.next() {
        bail!(token, "expected `$*` or `$:`")
    } else {
        bail!("unexpected end of macro invocation: expected to be followed by `$+` or `$:`")
    }
    // for i in 0..len {
    //     output.extend(replace_inner(input.clone(), &context, i).0);
    // }
    // Ok(output)
    // Ok(TokenStream::new())
}

#[derive(Default, Debug)]
struct Context {
    keys: Vec<Ident>,
    data: Vec<Value>,
    len: usize,
}
impl Context {
    fn push_key(&mut self, key: Ident) -> Result<()> {
        assert_eq!(self.len, 0, "can only add keys before pushing values");

        if let Some(present) = self.keys.iter().find(|k| *k == &key) {
            Err(
                error_message!(key, "variable `{key}` specified twice").join(error_message!(
                    present,
                    "variable `{present}` specified twice"
                )),
            )
        } else {
            self.keys.push(key);
            Ok(())
        }
    }

    fn new_row(&mut self) {
        self.len += 1;
        self.data
            .reserve(self.len * self.keys.len() - self.data.len());
    }

    // fn finish_row(&mut self) {
    //     self.data.extend((self.data.len()..self.len * self.keys.len()).map(|_|
    // Value::Optional(None))); }
    fn push(&mut self, value: Value) {
        self.data.push(value);
    }

    fn get(&self, index: usize, name: &Ident) -> Option<&Value> {
        self.keys
            .iter()
            .position(|k| k == name)
            .map(|k| &self.data[k + index * self.keys.len()])
    }
}

fn replace_outer(input: impl Into<TokenParser>, context: &Context) -> Result {
    let mut input = input.into();
    let mut output = TokenStream::new();
    while !input.is_empty() {
        if let Some(group) = input.next_group() {
            output.push(TokenTree::Group(Group::new(
                group.delimiter(),
                replace_outer(group.stream(), context)?,
            )));
            continue;
        }
        let Some(dollar) = input.next_tt_dollar() else {
            output.extend(input.next());
            continue;
        };
        // doesn't use `peek_n_star` to also match on e.g. `*,` i.e. joint punct
        if input.peek_n_tt_star(1).is_some() {
            if let Some(inner) = input.next_parenthesized() {
                input.next(); // *
                output.extend(replace_loop(inner.stream(), context));
                continue;
            }
        }
        output.extend(dollar);
    }
    Ok(output)
}

fn replace_loop(tokens: impl Into<TokenParser>, context: &Context) -> TokenStream {
    let mut output = TokenStream::new();
    let tokens = tokens.into();
    for i in 0..context.len {
        output.extend(replace_inner(tokens.clone(), context, i).0);
    }
    output
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
enum Optionalness {
    NotOptional,
    Some,
    None,
}

impl BitAndAssign<&Value> for Optionalness {
    fn bitand_assign(&mut self, rhs: &Value) {
        *self = match (*self, rhs) {
            (_, Value::Required(_))
            | (Optionalness::None, _)
            | (Optionalness::Some, Value::Optional(Some(_))) => return,
            (_, Value::Optional(None)) => Optionalness::None,
            (Optionalness::NotOptional, Value::Optional(Some(_))) => Optionalness::Some,
        }
    }
}
impl BitAndAssign for Optionalness {
    fn bitand_assign(&mut self, rhs: Self) {
        *self = (*self).max(rhs);
    }
}

fn replace_inner(
    tokens: impl Into<TokenParser>,
    context: &Context,
    index: usize,
) -> (TokenStream, Optionalness) {
    let mut tl_some = Optionalness::NotOptional;
    let mut output = TokenStream::new();
    let mut tokens = tokens.into();
    while !tokens.is_empty() {
        if tokens.peek_tt_dollar().is_some() {
            if let Some(ident) = tokens.peek_n_ident(1) {
                if let Some(replacement) = context.get(index, ident) {
                    // $
                    tokens.next();
                    // ident
                    tokens.next();
                    tl_some &= replacement;
                    let replacement = replacement.tokens();
                    output.extend(replacement);
                } else {
                    // $
                    output.extend(tokens.next());
                    // ident
                    output.extend(tokens.next());
                }
                continue;
            }
            if let (Some(_), Some(group)) = (tokens.peek_n_tt_question(2), tokens.peek_n_group(1)) {
                if group.is_parenthesized() {
                    let (replaced, some) = replace_inner(group.stream(), context, index);
                    match some {
                        Optionalness::NotOptional => {
                            output.extend(tokens.next());
                            let group = tokens.next_group().expect("group was peeked");
                            let mut replaced = Group::new(group.delimiter(), replaced);
                            replaced.set_span(group.span());
                            output.push(replaced.into());
                            output.extend(tokens.next());
                            continue;
                        }
                        Optionalness::Some => output.extend(replaced),
                        Optionalness::None => {}
                    }
                    tokens.next(); // $
                    tokens.next(); // (...)
                    tokens.next(); // ?
                }
            }
        } else if let Some(group) = tokens.next_group() {
            // Ø{ .. ])
            let (replaced, some) = replace_inner(group.stream(), context, index);
            output.push(TokenTree::Group(Group::new(group.delimiter(), replaced))); // Ø{ .. ])
            tl_some &= some;
            continue;
        }
        // non group non replacement token
        output.extend(tokens.next());
    }
    (output, tl_some)
}

#[derive(Clone, Copy, PartialEq)]
enum Typ {
    Expr,
    Ty,
    Tt,
    Inner,
    Idx,
}

impl Typ {
    fn parse_value(
        self,
        input: &mut TokenParser<token_stream::IntoIter>,
        index: usize,
    ) -> Result<TokenStream, ErrorMessage> {
        match self {
            Typ::Expr => input
                .next_expression()
                .ok_or_else(|| error_message!(input.peek(), "expected non-empty expression")),
            Typ::Ty => input
                .next_type()
                .ok_or_else(|| error_message!(input.peek(), "expected non-empty type")),
            Typ::Tt => input
                .next()
                .map(ToTokens::into_token_stream)
                .ok_or_else(|| error_message!(input.peek(), "expected token tree")),
            Typ::Inner => input
                .next_group()
                .as_ref()
                .map(Group::stream)
                .ok_or_else(|| error_message!(input.peek(), "expected group")),
            Typ::Idx => {
                Ok(TokenTree::Literal(Literal::usize_unsuffixed(index)).into_token_stream())
            }
        }
    }

    fn is_meta(self) -> bool {
        matches!(self, Typ::Idx)
    }
}

impl TryFrom<Ident> for Typ {
    type Error = Error;

    fn try_from(value: Ident) -> Result<Self> {
        const ERROR: &str = "expected `expr`, `ty`, `tt` or `inner`";
        const NOTE: &str = "this crates patterns are fuzzy, i.e. `ty` and `expr` can parse any \
                            pattern matching the comma rules for type and expression";
        let help = match value.to_string().as_str() {
            "expr" => return Ok(Self::Expr),
            "ty" => return Ok(Self::Ty),
            "tt" => return Ok(Self::Tt),
            "inner" => return Ok(Self::Inner),
            "idx" => return Ok(Self::Idx),
            "block" => "`tt`",
            "ident" => "`tt`, `ty` or `expr`",
            "path" => "`ty`",
            "lifetime" | "literal" => "`ty` or `expr`",
            "vis" => "`ty?` or `expr?`",
            _ => bail!(value, "{ERROR}";
                note = "{NOTE}";
                help = "use `inner` instead and wrap the value in `(...)`"
            ),
        };
        bail!( value, "{ERROR}";
            note = "{NOTE}";
            help = "use {help} instead"
        );
    }
}

enum Vars {
    Single(Var),
    Plain(Vec<Vars>),
    Tuple(Vec<Vars>, Span),
}

impl SpanRanged for Vars {
    fn span_range(&self) -> std::ops::Range<proc_macro2::Span> {
        match self {
            Vars::Plain(vars) => {
                vars.first()
                    .expect("vars are nether empty")
                    .span_range()
                    .start
                    ..vars.last().expect("vars are nether empty").span_range().end
            }
            Vars::Tuple(_, span) => span.span_range(),
            Vars::Single(var) => var.span_range(),
        }
    }
}

impl Vars {
    fn optional(&self) -> bool {
        match self {
            Vars::Single(var) => var.optional,
            Vars::Plain(vars) => vars.iter().all(Vars::optional),
            Vars::Tuple(..) => false,
        }
    }

    fn idx(&self) -> bool {
        matches!(self, Self::Single(Var { typ: Typ::Idx, .. }))
    }

    fn parse_multiple(input: &mut Parser, top_level: bool) -> Result<Vec<Self>> {
        let mut vars = Vec::new();
        while !(input.is_empty() || (top_level && input.peek_keyword("in").is_some())) {
            // $
            if input.next_tt_dollar().is_some() {
                vars.push(Var::parse(input).map(Self::Single)?);
            } else {
                unwrap_or!(
                    input,
                    input.next_parenthesized(),
                    input,
                    "either `$ident` or (...)"
                );
                vars.push(Vars::Tuple(
                    Vars::parse_multiple(&mut input.stream().parser(), false)?,
                    input.span(),
                ));
            }
            if input.is_empty() || (top_level && input.peek_keyword("in").is_some()) {
                break;
            }
            if input.next_tt_comma().is_none() {
                bail!(input.into_token_stream(), "expected `,`");
            }
        }

        Ok(vars)
    }

    fn parse(input: &mut Parser) -> Result<Self> {
        let vars = Vars::parse_multiple(input, true)?;
        if vars.is_empty() {
            bail_optional_span!(input, "expected binding, either $ident:type or (...)");
        }
        let vars = Self::Plain(vars);
        if vars.optional() {
            bail!(vars, "binding cannot be optional");
        }
        if vars.idx() {
            bail!(
                vars,
                "at least one consuming binding must be specified i.e. of type `expr`, `ty`, `tt` \
                 or `inner`"
            );
        }
        Ok(vars)
    }

    fn parse_values(
        &self,
        input: &mut Parser,
        context: &mut Context,
        group_span: Option<Span>,
    ) -> Result<()> {
        match self {
            Vars::Plain(vars) => {
                for vars in vars {
                    vars.parse_values(input, context, group_span)?;
                }
            }
            Vars::Tuple(vars, _) => {
                let group_span = input.peek().map(TokenTree::span);
                unwrap_or!(inner, input.next_parenthesized(), input, "(...)");
                let mut inner = inner.stream().parser();
                for vars in vars {
                    vars.parse_values(&mut inner, context, group_span)?;
                }

                if !input.is_empty() {
                    unwrap_or!(_, input.next_tt_comma(), input, "`,`");
                }
            }
            Vars::Single(Var {
                name,
                typ,
                optional,
            }) => {
                if input.is_empty() {
                    if *optional {
                        context.push(Value::Optional(None));
                    } else if typ.is_meta() {
                        context.push(Value::Required(typ.parse_value(input, context.len - 1)?));
                    } else {
                        bail!(group_span, "missing mandatory value {name}");
                    }
                } else {
                    context.push(Value::some(
                        typ.parse_value(input, context.len - 1)?,
                        *optional,
                    ));
                    if !(typ.is_meta() || input.is_empty()) {
                        unwrap_or!(_, input.next_tt_comma(), input, "`,`");
                    }
                }
            }
        }
        Ok(())
    }

    fn idents(&self) -> impl Iterator<Item = Ident> + '_ {
        let box_: Box<dyn Iterator<Item = Ident>> = match self {
            Vars::Single(Var { name, .. }) => Box::new(iter::once(name.clone())),
            Vars::Tuple(vars, _) | Vars::Plain(vars) => {
                Box::new(vars.iter().flat_map(Vars::idents))
            }
        };
        box_
    }
}

#[derive(Clone)]
struct Var {
    name: Ident,
    typ: Typ,
    optional: bool,
}

impl SpanRanged for Var {
    fn span_range(&self) -> std::ops::Range<Span> {
        todo!()
    }
}

impl Var {
    fn parse(input: &mut Parser) -> Result<Self> {
        // $ident
        unwrap_or!(name, input.next_ident(), input, "ident");
        // $ident:
        unwrap_or!(_, input.next_tt_colon(), input, "`:`");
        // $ident:typ
        unwrap_or!(
            typ,
            input.next_ident(),
            input,
            "`expr`, `ty`, `tt` or `inner`"
        );
        let optional = if let Some(question) = input.next_tt_question() {
            if typ == "tt" {
                bail!(
                    quote!(#typ #question),
                    "`tt` cannot be optional due to parsing instructions use `inner` instead and \
                     wrap the value in `(...)`"
                )
            }
            true
        } else {
            false
        };
        let typ = Typ::try_from(typ)?;

        Ok(Self {
            name,
            typ,
            optional,
        })
    }
}

#[derive(Clone, Debug)]
enum Value {
    Optional(Option<TokenStream>),
    Required(TokenStream),
}

impl Value {
    fn some(value: TokenStream, optional: bool) -> Value {
        if optional {
            Value::Optional(Some(value))
        } else {
            Value::Required(value)
        }
    }

    fn tokens(&self) -> Option<TokenStream> {
        match self.clone() {
            Value::Optional(v) => v,
            Value::Required(v) => Some(v),
        }
    }
}

#[test]
fn expansions() {
    use proc_macro_utils::assert_expansion;
    assert_expansion!(forr_impl! {$a:expr in [1, 2, 3]$* $a}.unwrap(), {1 2 3});
    assert_expansion!(forr_impl! {$a:expr, $i:idx in [1, 2, 3]$* $i}.unwrap(), {0 1 2});
    assert_expansion!(forr_impl! {$a:expr, $i:idx in [1, 2, 3]$: ! $($i)* !}.unwrap(), {! 0 1 2 !});
    assert_expansion!(forr_impl! {$a:expr?, $i:idx in [1, 2, 3]$: ! $($i)* !}.unwrap(), {! 0 1 2 !});
    assert_expansion!(forr_impl! {($a:expr?), $i:idx in [(1), (), (3)]$: ! $($i$(:$a)?)* !}.unwrap(), {! 0:1 1 2:3 !});
    // This behaviour might not be the expected one, but it enables better support
    // for nested forr loops, as the groups of an inner forr loop are left
    // untouched.
    assert_expansion!(
        forr_impl! {$a:expr, $b:expr?, $i:idx in [1a, 1b, 2a]$: ! $($i:$($a)? $()? $(+ $b)? $()* )* !}.unwrap(),
        {! 0:$(1a)? $()? + 1b $()* 1:$(2a)? $()? $()* !}
    );
}
