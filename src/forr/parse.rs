use std::ops::{Bound, Range};

use manyhow::{bail, SpanRanged};
use proc_macro2::{Span, TokenTree};
use syn::ext::IdentExt;
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::token::{Bracket, Paren};
use syn::{bracketed, custom_keyword, parenthesized, Ident, LitInt, Token};

use super::input::*;

impl Parse for MacroInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let vars = Vars::parse(input, true)?;
        if let Vars::Plain(vars) = &vars {
            if vars.is_empty() {
                return Err(input.error(
                    "expected binding, either $ident:type or
            (...)",
                ));
            }
            for var in vars {
                if let Vars::Single(var) = var {
                    if var.optional {
                        bail!(var, "top level binding cannot be optional");
                    }
                }
            }
            if vars.iter().all(Vars::non_consuming) {
                return Err(input.error(
                    "missing a consuming binding i.e. of type `ident`, `expr`, `ty`, `tt` or \
                     `inner`",
                ));
            }
        } else {
            unreachable!("Vars::parse always returns Vars::Plain")
        }

        input.parse::<Token![in]>()?;

        Ok(Self {
            vars,
            values: input.parse()?,
            prefix: input.parse()?,
            loop_type: input.parse()?,
            body: input.parse()?,
        })
    }
}

impl Parse for Prefix {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let la = input.lookahead1();
        if la.peek(Token![$]) {
            input.parse::<Token![$]>()?;
            Ok(Self::Dollar)
        } else if la.peek(Token![#]) {
            input.parse::<Token![#]>()?;
            Ok(Self::Pound)
        } else {
            Err(la.error())
        }
    }
}

impl Parse for LoopType {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let la = input.lookahead1();
        if la.peek(Token![:]) {
            input.parse::<Token![:]>()?;
            Ok(Self::Colon)
        } else if la.peek(Token![*]) {
            input.parse::<Token![*]>()?;
            Ok(Self::Star)
        } else {
            Err(la.error())
        }
    }
}

impl Vars {
    fn parse(input: ParseStream, first: bool) -> syn::Result<Self> {
        let mut vars = Vec::new();
        loop {
            let var;
            let la = input.lookahead1();
            if la.peek(Token![$]) || la.peek(Token![#]) {
                var = Vars::Single(input.parse()?);
            } else if la.peek(Paren) {
                let content;
                parenthesized!(content in input);
                var = Vars::Tuple(Box::new(Vars::parse(&content, false)?));
            } else if VarsFunction::peek(&la) {
                let name: Ident = input.fork().parse()?;
                let function = VarsFunction::parse(input)?;
                let optional = input.parse::<Token![?]>();

                if let (Ok(optional), VarsFunction::Tuples(..)) = (&optional, &function) {
                    bail!(
                        name.span()..optional.span(),
                        "`tuples(...)` cannot be optional"
                    )
                }

                if matches!(function, VarsFunction::Tuples(..)) && !first {
                    bail!(name, "tuples can only be used at the top level")
                }

                var = Vars::Function {
                    name,
                    function,
                    optional: optional.is_ok(),
                };
            } else {
                return Err(la.error());
            }

            vars.push(var);
            let la = input.lookahead1();

            if input.is_empty() || la.peek(Token![in]) {
                break;
            } else if la.peek(Token![,]) {
                <Token![,]>::parse(input)?;
                continue;
            }

            return Err(la.error());
        }
        Ok(Self::Plain(vars))
    }
}

impl SpanRanged for Var {
    fn span_range(&self) -> Range<Span> {
        self.span.clone()
    }
}

impl Parse for Var {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let span = input.span();
        assert!(input.parse::<Token![$]>().is_ok() || input.parse::<Token![#]>().is_ok());

        let name: Ident = input.parse()?;
        input.parse::<Token![:]>()?;
        let mut span = span..input.span();
        let typ: Typ = input.parse()?;

        let optional = if let Ok(q) = input.parse::<Token![?]>() {
            if matches!(typ, Typ::Tt) {
                bail!(
                    span.end..q.span,
                    "`tt` cannot be optional due to parsing
        restrictions use `inner` instead and wrap the value in `(...)`"
                )
            }
            span.end = q.span;
            true
        } else {
            false
        };

        Ok(Self {
            name,
            typ,
            optional,
            span,
        })
    }
}

impl Parse for Casing {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        custom_keyword!(s);
        custom_keyword!(S);
        custom_keyword!(c);
        custom_keyword!(C);
        let la = input.lookahead1();
        let it = if input.peek(s) {
            Self::s
        } else if input.peek(S) {
            Self::S
        } else if input.peek(c) {
            Self::c
        } else if input.peek(C) {
            Self::C
        } else {
            return Err(la.error());
        };
        input.parse::<Ident>()?;
        Ok(it)
    }
}

impl Parse for ValuesFunction {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name: Ident = input.parse()?;
        let args;
        parenthesized!(args in input);

        match name.to_string().as_str() {
            "idents" => Ok(Self::Ident(Ident::parse_any(&args)?, {
                args.parse::<Token![,]>()?;
                args.parse().and_then(|i: LitInt| i.base10_parse())?
            })),
            _ => unreachable!("should be peeked"),
        }
    }
}

impl ValuesFunction {
    fn peek(la: &syn::parse::Lookahead1) -> bool {
        custom_keyword!(idents);
        la.peek(idents)
    }
}

impl Parse for VarsFunction {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name: Ident = input.parse()?;
        let args;
        parenthesized!(args in input);
        match name.to_string().as_str() {
            "casing" => {
                let mut casings = Vec::new();
                while !args.is_empty() {
                    let la = args.lookahead1();
                    if !(la.peek(Token![$]) || la.peek(Token![#])) {
                        return Err(la.error());
                    }
                    args.parse::<TokenTree>()?;
                    let name = args.parse()?;
                    <Token![:]>::parse(&args)?;
                    let casing = args.parse()?;
                    casings.push((name, casing));
                    if !args.is_empty() {
                        <Token![,]>::parse(&args)?;
                    }
                }
                Ok(Self::Casing(casings))
            }
            "tuples" => Ok(Self::Tuples(args.parse()?, {
                if args.parse::<Token![,]>().is_ok() {
                    let start = args
                        .parse::<Option<LitInt>>()?
                        .as_ref()
                        .map(LitInt::base10_parse)
                        .transpose()?;

                    let la = args.lookahead1();
                    let inclusive = if la.peek(Token![..=]) {
                        args.parse::<Token![..=]>()?;
                        true
                    } else if la.peek(Token![..]) {
                        args.parse::<Token![..]>()?;
                        false
                    } else if !args.is_empty() || start.is_some() {
                        return Err(la.error());
                    } else {
                        false
                    };

                    let end = args
                        .parse::<Option<LitInt>>()?
                        .as_ref()
                        .map(LitInt::base10_parse)
                        .transpose()?;
                    (
                        start.map_or(Bound::Unbounded, Bound::Included),
                        end.map_or(Bound::Unbounded, |end| {
                            if inclusive {
                                Bound::Included(end)
                            } else {
                                Bound::Excluded(end)
                            }
                        }),
                    )
                } else {
                    (Bound::Unbounded, Bound::Unbounded)
                }
            })),
            name => unreachable!("{name} should not be peeked"),
        }
    }
}

impl VarsFunction {
    fn peek(la: &syn::parse::Lookahead1) -> bool {
        custom_keyword!(casing);
        custom_keyword!(tuples);
        la.peek(casing) || la.peek(tuples)
    }
}

impl Parse for Typ {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        custom_keyword!(expr);
        custom_keyword!(ty);
        custom_keyword!(tt);
        custom_keyword!(inner);
        custom_keyword!(idx);
        custom_keyword!(ident);
        custom_keyword!(block);
        custom_keyword!(path);
        custom_keyword!(lifetime);
        custom_keyword!(literal);
        custom_keyword!(vis);

        let la = input.lookahead1();

        let it = if la.peek(expr) {
            Self::Expr
        } else if la.peek(ty) {
            Self::Ty
        } else if la.peek(tt) {
            Self::Tt
        } else if la.peek(inner) {
            Self::Inner
        } else if la.peek(idx) {
            Self::Idx
        } else if la.peek(ident) {
            Self::Ident
        } else if la.peek(block) {
            Self::Block
        } else if la.peek(path) {
            Self::Path
        } else if la.peek(lifetime) {
            Self::Lifetime
        } else if la.peek(literal) {
            Self::Literal
        } else {
            return Err(la.error());
        };
        input.parse::<Ident>()?;
        Ok(it)
    }
}

impl Parse for Values {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let la = input.lookahead1();
        if la.peek(Bracket) {
            let args;
            bracketed!(args in input);
            Ok(Self::List(args.parse()?))
        } else if ValuesFunction::peek(&la) {
            Ok(Self::Function(input.parse()?))
        } else {
            Err(la.error())
        }
    }
}
