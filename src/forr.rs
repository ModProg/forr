use std::fmt::Display;
use std::iter;
use std::ops::{BitAndAssign, Range};

use manyhow::{bail, error_message, Error, ErrorMessage, JoinToTokensError, Result, SpanRanged};
use proc_macro2::{token_stream, Group, Ident, Literal, Span, TokenStream, TokenTree};
use proc_macro_utils::{
    Delimited, TokenParser, TokenStream2Ext, TokenTree2Ext, TokenTreeLiteral, TokenTreePunct,
};
use quote::{quote, ToTokens};

use crate::iff::step_colon;
use crate::Parser;

pub fn step_star(input: &mut Parser) -> Result<()> {
    if input
        .next_if_each((TokenTree::is_dollar, TokenTree::is_asterix))
        .or_else(|| input.next_if_each((TokenTree::is_pound, TokenTree::is_asterix)))
        .is_none()
    {
        bail_optional_span!(input, "expected `$*` or `#*`");
    } else {
        Ok(())
    }
}

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

    if input.peek_tt_pound().is_some() {
        context.prefix = TokenTree::is_pound;
    }

    // $*
    if step_star(&mut input).is_ok() {
        Ok(replace_loop(input, &context))
    } else if step_colon(&mut input).is_ok() {
        replace_outer(input, &context)
    } else if let Some(token) = input.next() {
        bail!(token, "expected `$*`, `$:`, `#*`, or `#:`")
    } else {
        bail!(
            "unexpected end of macro invocation: expected to be followed by `$*`, `$:`, `#*`, or \
             `#:`"
        )
    }
}

#[derive(Debug)]
struct Context {
    keys: Vec<Ident>,
    data: Vec<Value>,
    len: usize,
    prefix: fn(&TokenTree) -> bool,
}

impl Default for Context {
    fn default() -> Self {
        Self {
            keys: Vec::new(),
            data: Vec::new(),
            len: 0,
            prefix: TokenTree::is_dollar,
        }
    }
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
        let Some(prefix) = input.next_if(context.prefix) else {
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
        if tokens.peek_if_each(context.prefix).is_some() {
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
    Ident,
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
            "ident" => return Ok(Self::Ident),
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
    Function {
        function: Function,
        span: Range<Span>,
        optional: bool,
    },
}

impl SpanRanged for Vars {
    fn span_range(&self) -> Range<proc_macro2::Span> {
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
            Vars::Function { span, .. } => span.span_range(),
        }
    }
}

impl Vars {
    fn optional(&self) -> bool {
        match self {
            Vars::Single(var) => var.optional,
            Vars::Plain(vars) => vars.iter().all(Vars::optional),
            Vars::Tuple(..) => false,
            Vars::Function { optional, .. } => *optional,
        }
    }

    fn idx(&self) -> bool {
        matches!(self, Self::Single(Var { typ: Typ::Idx, .. }))
    }

    fn parse_multiple(input: &mut Parser, top_level: bool) -> Result<Vec<Self>> {
        let mut vars = Vec::new();
        while !(input.is_empty() || (top_level && input.peek_keyword("in").is_some())) {
            // $ or #
            if input.next_tt_dollar().is_some() || input.next_tt_pound().is_some() {
                vars.push(Var::parse(input).map(Self::Single)?);
            } else if let Some(input) = input.next_parenthesized() {
                vars.push(Vars::Tuple(
                    Vars::parse_multiple(&mut input.stream().parser(), false)?,
                    input.span(),
                ));
            } else {
                let span = input.span();
                unwrap_or!(
                    name,
                    input.next_ident(),
                    input,
                    "either `$ident`, `(...)` or `casing(...)`"
                );
                let mut span = span..input.span();
                unwrap_or!(args, input.next_parenthesized(), input, "`(...)`");

                let optional = input.next_if(TokenTree::is_question).map(|t| t.span());
                span.end = optional.unwrap_or(span.end);

                vars.push(Vars::Function {
                    function: Function::parse(&name, args.stream().parser(), optional)?,
                    optional: optional.is_some(),
                    span,
                });
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
            bail!(vars, "top level binding cannot be optional");
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
                function,
                span,
                optional,
            } => {
                if input.is_empty() {
                    if *optional {
                        for _ in 0..function.len() {
                            context.push(Value::Optional(None));
                        }
                    } else {
                        bail!(
                            error_message!(group_span, "missing mandatory values for `{function}`")
                                .join(error_message!(span, "`{function}` defined mandatory here"))
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

    fn idents(&self) -> impl Iterator<Item = Ident> + '_ {
        let box_: Box<dyn Iterator<Item = Ident>> = match self {
            Vars::Single(Var { name, .. }) => Box::new(iter::once(name.clone())),
            Vars::Tuple(vars, _) | Vars::Plain(vars) => {
                Box::new(vars.iter().flat_map(Vars::idents))
            }
            Vars::Function { function, .. } => Box::new(function.idents()),
        };
        box_
    }
}

#[derive(Clone)]
struct Var {
    name: Ident,
    typ: Typ,
    optional: bool,
    span: Range<Span>,
}

impl SpanRanged for Var {
    fn span_range(&self) -> Range<Span> {
        self.span.clone()
    }
}

impl Var {
    fn parse(input: &mut Parser) -> Result<Self> {
        let span = input.span();
        // $ident
        unwrap_or!(name, input.next_ident(), input, "ident");
        // $ident:
        unwrap_or!(_, input.next_tt_colon(), input, "`:`");
        let mut span = span..input.span();
        // $ident:typ
        unwrap_or!(
            typ,
            input.next_ident(),
            input,
            "`expr`, `ty`, `tt` or `inner`"
        );
        let q_span = input.span();
        let optional = if let Some(question) = input.next_tt_question() {
            span.end = q_span;
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
            span,
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

enum Function {
    Casing(Vec<(Ident, Casing)>),
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Function::Casing(v) => write!(
                f,
                "casing({})",
                v.iter()
                    .map(|(i, c)| format!("{i}:{c:?}"))
                    .collect::<Vec<_>>()
                    .join(",")
            ),
        }
    }
}

impl Function {
    fn len(&self) -> usize {
        match self {
            Function::Casing(v) => v.len(),
        }
    }

    fn parse_value(&self, input: &mut TokenParser) -> Result<Vec<TokenStream>> {
        match self {
            Function::Casing(variables) => {
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
            Function::Casing(c) => c.iter().map(|i| i.0.clone()),
        }
    }

    fn parse(name: &Ident, mut args: TokenParser, _optional: Option<Span>) -> Result<Self> {
        match name.to_string().as_str() {
            "casing" => {
                let mut casings = Vec::new();
                while !args.is_empty() {
                    unwrap_or!(
                        _,
                        args.next_tt_dollar().or_else(|| args.next_tt_pound()),
                        args,
                        "`$`"
                    );
                    unwrap_or!(name, args.next_ident(), args, "ident");
                    unwrap_or!(_, args.next_tt_colon(), args, ":");
                    unwrap_or!(
                        casing,
                        args.next_ident(),
                        args,
                        "either `c`, `C`, `s` or `S`"
                    );
                    let casing = match casing.to_string().as_str() {
                        "c" => Casing::c,
                        "C" => Casing::C,
                        "s" => Casing::s,
                        "S" => Casing::S,
                        other => bail!(
                            name,
                            "unknown case `{other}`, expected either `c`, `C`, `s` or `S`"
                        ),
                    };
                    casings.push((name, casing));
                    if !args.is_empty() {
                        unwrap_or!(_, args.next_tt_comma(), args, "`,`");
                    }
                }
                Ok(Self::Casing(casings))
            }
            other => bail!(name, "unknown function `{other}`, expected `casing`"),
        }
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Casing {
    s,
    S,
    c,
    C,
}

#[test]
fn expansions() {
    use proc_macro_utils::assert_expansion;
    assert_expansion!(forr! {$a:expr in [1, 2, 3]$* $a}.unwrap(), {1 2 3});
    assert_expansion!(forr! {$a:expr, $i:idx in [1, 2, 3]$* $i}.unwrap(), {0 1 2});
    assert_expansion!(forr! {$a:expr, $i:idx in [1, 2, 3]$: ! $($i)* !}.unwrap(), {! 0 1 2 !});
    assert_expansion!(forr! {$a:expr?, $i:idx in [1, 2, 3]$: ! $($i)* !}.unwrap(), {! 0 1 2 !});
    assert_expansion!(forr! {($a:expr?), $i:idx in [(1), (), (3)]$: ! $($i$(:$a)?)* !}.unwrap(), {! 0:1 1 2:3 !});
    // This behaviour might not be the expected one, but it enables better support
    // for nested forr loops, as the groups of an inner forr loop are left
    // untouched.
    assert_expansion!(
        forr! {$a:expr, $b:expr?, $i:idx in [1a, 1b, 2a]$: ! $($i:$($a)? $()? $(+ $b)? $()* )* !}.unwrap(),
        {! 0:$(1a)? $()? + 1b $()* 1:$(2a)? $()? $()* !}
    );
}

#[test]
fn casing() {
    use proc_macro_utils::assert_expansion;
    assert_expansion!(
        forr! {casing($s:s, $S:S, $c:c, $C:C) in [a b, "a b", a 1, a 0x23]$* $s $S $c $C }.unwrap(),
        {
            a_b    A_B    aB    AB
            a_b    A_B    aB    AB
            a_1    A_1    a1    A1
            a_0x23 A_0X23 a0x23 A0x23
        }
    );
    assert_expansion!(forr! {casing($s:s,) in [a b] $* $s}.unwrap(), { a_b });
    assert_expansion!(forr! {($a:expr, casing($s:s)?) in [(a, b c), (d)] $* $a $s}.unwrap(), {a b_c d});
}
