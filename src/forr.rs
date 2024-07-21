use std::iter;
use std::ops::BitAndAssign;

use manyhow::{bail, error_message, ErrorMessage, JoinToTokensError, Result};
use proc_macro2::{token_stream, Group, Ident, Literal, Span, TokenStream, TokenTree};
use proc_macro_utils::{
    Delimited, TokenParser, TokenStream2Ext, TokenTree2Ext, TokenTreeLiteral, TokenTreePunct,
};
use quote::{format_ident, quote, ToTokens};

use crate::Parser;

mod input;
use input::*;

mod parse;

pub fn forr(
    MacroInput {
        vars,
        values,
        prefix,
        loop_type,
        body,
    }: MacroInput,
) -> Result {
    let mut context = Context::new(vars.idents(), prefix)?;

    let values = match values {
        Values::List(values) => values,
        Values::Function(function) => function.expand(),
    };

    let mut values = values.parser();
    while !values.is_empty() {
        context.new_row();
        vars.parse_values(&mut values, &mut context, None)?;
    }

    match loop_type {
        LoopType::Star => Ok(replace_loop(body, &context)),
        LoopType::Colon => replace_outer(body, &context),
    }
}

#[derive(Debug)]
struct Context {
    keys: Vec<Ident>,
    data: Vec<Value>,
    len: usize,
    prefix: Prefix,
}

impl Context {
    fn new(vars: impl IntoIterator<Item = Ident>, prefix: Prefix) -> Result<Self> {
        let mut keys = Vec::new();
        for key in vars {
            if keys.contains(&key) {
                bail!(key, "variable `{key}` already used twice");
            }
            keys.push(key);
        }
        Ok(Self {
            keys,
            data: Vec::new(),
            len: 0,
            prefix,
        })
    }
}

impl Context {
    fn new_row(&mut self) {
        self.len += 1;
        self.data
            .reserve(self.len * self.keys.len() - self.data.len());
    }

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
        let Some(prefix) = input.next_if(context.prefix.test()) else {
            output.extend(input.next());
            continue;
        };
        if input.peek_n_tt_star(1).is_some() {
            if let Some(inner) = input.next_parenthesized() {
                input.next(); // *
                output.extend(replace_loop(inner.stream(), context));
                continue;
            }
        }
        output.push(prefix);
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
        if tokens.peek_if_each(context.prefix.test()).is_some() {
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
            Typ::Ident => input
                .next_ident()
                .map(ToTokens::into_token_stream)
                .ok_or_else(|| error_message!(input.peek(), "expected ident")),
            Typ::Block => input
                .next_braced()
                .map(ToTokens::into_token_stream)
                .ok_or_else(|| error_message!(input.peek(), "expected block")),
            Typ::Path => input
                .next_type()
                .ok_or_else(|| error_message!(input.peek(), "expected path")),
            Typ::Lifetime => input
                .next_if_each((
                    |tt: &TokenTree| tt.is_joint() && tt.is_quote(),
                    TokenTree::is_ident,
                ))
                .ok_or_else(|| error_message!(input.peek(), "expected lifetime")),
            Typ::Literal => input
                .next_literal()
                .map(ToTokens::into_token_stream)
                .ok_or_else(|| error_message!(input.peek(), "expected literal")),
        }
    }

    fn is_meta(self) -> bool {
        matches!(self, Typ::Idx)
    }
}

impl Vars {
    fn non_consuming(&self) -> bool {
        match self {
            Vars::Single(v) => matches!(v.typ, Typ::Idx),
            Vars::Plain(v) => v.iter().all(Self::non_consuming),
            Vars::Tuple(..) | Vars::Function { .. } => false,
        }
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
            Vars::Tuple(vars) => {
                let group_span = input.peek().map(TokenTree::span);
                unwrap_or!(inner, input.next_parenthesized(), input, "(...)");
                let mut inner = inner.stream().parser();
                vars.parse_values(&mut inner, context, group_span)?;

                if !input.is_empty() {
                    unwrap_or!(_, input.next_tt_comma(), input, "`,`");
                }
            }
            Vars::Single(Var {
                name,
                typ,
                optional,
                ..
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
            Vars::Function {
                name,
                function,
                optional,
            } => {
                if input.is_empty() {
                    if *optional {
                        for _ in 0..function.len() {
                            context.push(Value::Optional(None));
                        }
                    } else {
                        bail!(
                            error_message!(group_span, "missing mandatory values for `{name}`")
                                .join(error_message!(name, "`{name}` defined mandatory here"))
                        );
                    }
                } else {
                    for value in function.parse_value(input)? {
                        context.push(Value::some(value, *optional));
                    }
                    if !input.is_empty() {
                        unwrap_or!(_, input.next_tt_comma(), input, "`,`");
                    }
                }
            }
        }
        Ok(())
    }

    fn idents(&self) -> Box<dyn Iterator<Item = Ident> + '_> {
        match self {
            Vars::Single(Var { name, .. }) => Box::new(iter::once(name.clone())),
            Vars::Tuple(vars) => Box::new(vars.idents()),
            Vars::Plain(vars) => Box::new(vars.iter().flat_map(Vars::idents)),
            Vars::Function { function, .. } => Box::new(function.idents()),
        }
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

impl VarsFunction {
    fn len(&self) -> usize {
        match self {
            Self::Casing(v) => v.len(),
        }
    }

    fn parse_value(&self, input: &mut TokenParser) -> Result<Vec<TokenStream>> {
        match self {
            Self::Casing(variables) => {
                let mut span = None;
                let parts: Vec<_> = input
                    .next_while(|token| token.is_ident() || token.is_literal())
                    .ok_or_else(|| {
                        error_message!(input.peek(), "expected at least one ident or literal")
                    })?
                    .into_iter()
                    .inspect(|t| span = Some(t.span()))
                    .map(|t| match t {
                        TokenTree::Ident(ident) => ident.to_string(),
                        TokenTree::Literal(lit) => lit.string().unwrap_or_else(|| lit.to_string()),
                        _ => unreachable!(),
                    })
                    .flat_map(|s| {
                        s.to_lowercase()
                            .split_whitespace()
                            .map(str::to_string)
                            .collect::<Vec<String>>()
                    })
                    .collect();
                let span = span.expect("next_while should ensure at least one value");
                Ok(variables
                    .iter()
                    .map(|(_, casing)| match *casing {
                        Casing::s => parts.join("_"),
                        Casing::S => parts.join("_").to_uppercase(),
                        casing => {
                            let mut parts = parts.iter();
                            (casing == Casing::c)
                                .then(|| parts.next().cloned().unwrap_or_default())
                                .unwrap_or_default()
                                + &parts
                                    .map(|s| {
                                        let mut s = s.chars();
                                        s.next()
                                            .map(|c| c.to_uppercase().to_string())
                                            .unwrap_or_default()
                                            + s.as_str()
                                    })
                                    .collect::<String>()
                        }
                    })
                    .map(|s| Ident::new(&s, span).to_token_stream())
                    .collect())
            }
        }
    }

    fn idents(&self) -> impl Iterator<Item = Ident> + '_ {
        match self {
            Self::Casing(c) => c.iter().map(|i| i.0.clone()),
        }
    }
}

impl ValuesFunction {
    fn expand(self) -> TokenStream {
        match self {
            ValuesFunction::Ident(prefix, count) => (0..count)
                .map(|idx| {
                    let ident = format_ident!("{prefix}{idx}");
                    quote!(#ident,)
                })
                .collect(),
        }
    }
}

#[cfg(test)]
macro_rules! assert_expansion {
    ($in:tt, $out:tt) => {
        proc_macro_utils::assert_tokens!(forr(syn::parse_quote!$in).unwrap(), $out);
    };
}

#[test]
fn expansions() {
    assert_expansion!({$a:expr in [1, 2, 3]$* $a}, {1 2 3});
    assert_expansion!({$a:expr, $i:idx in [1, 2, 3]$* $i}, {0 1 2});
    assert_expansion!({$a:expr, $i:idx in [1, 2, 3]$: ! $($i)* !}, {! 0 1 2 !});
    assert_expansion!({$a:expr, $i:idx in [1, 2, 3]$: ! $($i)* !}, {! 0 1 2 !});
    assert_expansion!({($a:expr?), $i:idx in [(1), (), (3)]$: ! $($i$(:$a)?)* !}, {! 0:1 1 2:3 !});
    // This behaviour might not be the expected one, but it enables better support
    // for nested forr loops, as the groups of an inner forr loop are left
    // untouched.
    assert_expansion!(
        {($a:expr, $b:expr?), $i:idx in [(1a, 1b), (2a)]$: ! $($i:$($a)? $()? $(+ $b)? $()* )* !},
        {! 0:$(1a)? $()? + 1b $()* 1:$(2a)? $()? $()* !}
    );
}

#[test]
fn casing() {
    assert_expansion!(
        {casing($s:s, $S:S, $c:c, $C:C) in [a b, "a b", a 1, a 0x23]$* $s $S $c $C },
        {
            a_b    A_B    aB    AB
            a_b    A_B    aB    AB
            a_1    A_1    a1    A1
            a_0x23 A_0X23 a0x23 A0x23
        }
    );
    assert_expansion!({casing($s:s,) in [a b] $* $s}, { a_b });
    assert_expansion!({($a:expr, casing($s:s)?) in [(a, b c), (d)] $* $a $s}, {a b_c d});
}

#[test]
fn idents() {
    assert_expansion!({$a:ident in idents(a, 2) $* $a}, {a0 a1});
}
