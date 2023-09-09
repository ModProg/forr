use manyhow::Result;
use proc_macro2::{TokenStream, TokenTree};
use proc_macro_utils::{TokenStream2Ext, TokenTreePunct};
use quote::ToTokens;

use crate::Parser;

struct Expression;

#[allow(clippy::type_complexity)]
const DOLLAR_COLON: (fn(&TokenTree) -> bool, fn(&TokenTree) -> bool) =
    (TokenTree::is_dollar, TokenTree::is_colon);

pub fn iff(input: TokenStream) -> Result {
    let mut input = input.parser();

    let expression = Expression::parse(true, &mut input)?;

    if input.next_if_each(DOLLAR_COLON).is_none() {
        bail_optional_span!(input, "expected $:");
    }

    Ok(expression.then_some(input).into_token_stream())
}

impl Expression {
    fn parse(llhs: bool, input: &mut Parser) -> Result<bool> {
        let lhs = Expression::parse_lazy(input)?;
        Ok(if input.next_tt_or_or().is_some() {
            let rhs = Expression::parse(true, input)?;
            llhs && lhs || rhs
        } else if input.next_tt_and_and().is_some() {
            let rhs = Expression::parse(lhs, input)?;
            llhs && rhs
        } else if input.is_empty() || input.peek_if_each(DOLLAR_COLON).is_some() {
            llhs && lhs
        } else {
            bail_optional_span!(input, "expected `&&` or `||`");
        })
    }

    fn parse_all(input: TokenStream) -> Result<bool> {
        let mut input = input.parser();
        let ret = Expression::parse(true, &mut input)?;
        if input.is_empty() {
            Ok(ret)
        } else {
            bail_optional_span!(input, "expected `&&` or `||`");
        }
    }

    fn parse_lazy(input: &mut Parser) -> Result<bool> {
        Ok(if let Some(parenthesized) = input.next_parenthesized() {
            Expression::parse_all(parenthesized.stream())?
        } else if input.next_tt_not().is_some() {
            !Expression::parse_lazy(input)?
        } else if input.next_keyword("true").is_some() {
            true
        } else if input.next_keyword("false").is_some() {
            false
        } else if input.next_keyword("empty").is_some() {
            let Some(parenthesized) = input.next_parenthesized() else {
                bail_optional_span!(input, "expected `(...)`");
            };

            parenthesized.stream().is_empty()
        } else if input.next_keyword("equals").is_some() {
            let Some(lhs) = input.next_parenthesized() else {
                bail_optional_span!(input, "expected `(...)`");
            };
            let Some(rhs) = input.next_parenthesized() else {
                bail_optional_span!(input, "expected `(...)`");
            };

            lhs.stream().to_string() == rhs.stream().to_string()
        } else {
            bail_optional_span!(
                input,
                "expected `(...)`, `!...`, `true`, `false`, or `empty`"
            );
        })
    }
}

#[test]
fn expression() {
    use quote::quote;
    assert!(Expression::parse_all(quote!(true && true || false)).unwrap());
    assert!(Expression::parse_all(quote!(true && (true || false))).unwrap());
    assert!(!Expression::parse_all(quote!(false || true && false)).unwrap());
    assert!(Expression::parse_all(quote!(false || empty() && true)).unwrap());
    assert!(!Expression::parse_all(quote!(false || empty(()) && true)).unwrap());
}