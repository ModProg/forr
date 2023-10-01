#![warn(clippy::pedantic, missing_docs)]
#![allow(clippy::wildcard_imports)]
#![cfg_attr(docsrs, feature(doc_auto_cfg))]
//! [Control flow](#iff) and [loops](#forr) in compile time.
//!
//! # `forr!`
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
//! The first part of the invocation is the pattern, similar to a normal `for`
//! loop in rust. Here you can use either a [single
//! variable](forr!#single-variable-binding) i.e. `$name:type` or a [tuple
//! binding](forr!#tuple-binding) `($name:type, $nbme:type, ...)`. There can
//! optionally be [non consuming patterns](forr!#non-consuming-patterns)
//! specified, currently that includes only [`:idx`](forr!#idx).
//!
//! This is followed by the keyword `in`, an array literal `[...]` containing
//! the tokens to iterate and the [body](forr!#body) marked with either `$*` or
//! `$:`.
//!
//! For more details see [`forr!`].
//!
//! # `iff!`
//!
//! Aims to be an alternative version of
//! [`#[cfg(...)]`](https://doc.rust-lang.org/reference/conditional-compilation.html#the-cfg-attribute)
//! able to conditionally enable any rust tokens and being able to, e.g. compare
//! tokens, but it is not able to be conditional other actual `cfg` or features.
//!
//! ```
//! # forr::
//! iff! { true && false $:
//!     compile_error!("This is not expanded")
//! }
//! ```
//! ```compile_fail
//! # forr::
//! iff! { true || false $:
//!     compile_error!("This is expanded")
//! }
//! ```
//!
//! On top of the basic boolean operations (`!`, `&&`, `||`) there are some
//! functions:
//!
//! - `empty(<tokens>)` tests if `<tokens>` is empty.
//! - `equals(<lhs>)(<rhs>)` tests if `<lhs>` is equal to `<rhs>`.
//! - `equals_any(<lhs>)[(<rhs>), (<rhs>), ...]` tests if `<lhs>` is equal to
//!   any `<rhs>`.
//!
//! For more details see [`iff!`].

use manyhow::manyhow;
use proc_macro2::token_stream;
use proc_macro_utils::TokenParser;

type Parser = TokenParser<token_stream::IntoIter>;

#[rustfmt::skip]
macro_rules! unwrap_or {
    ($ident:tt, $expr:expr, $parser:expr, $expected:literal) => {
        let Some($ident) = $expr else {
            if let Some(token) = $parser.next() {
                manyhow::bail!(token, "unexpected token: expected {}", $expected)
            }
            manyhow::bail!("unexpected end of macro invocation: expected to be followed by {}", $expected)
        };
    };
}

macro_rules! bail_optional_span {
    ($parser:expr, $($fmt:tt)*) => {
        if let Some(token) = $parser.next() {
            manyhow::bail!(token, "unexpected token: {}", format_args!($($fmt)*));
        }
        manyhow::bail!("unexpected end of macro invocation: {}", format_args!($($fmt)*));
    };
}

mod forr;

/// Iterates over specified list and expands input similar to macro_rules.
///
/// # Usage
///
/// The first part of the invocation is the pattern, similar to a normal `for`
/// loop in rust. Here you can use either a [single
/// variable](#single-variable-binding) i.e. `$name:type` or a [tuple
/// binding](#tuple-binding) `($name:type, $nbme:type, ...)`. There can
/// optionally be [non consuming patterns](#non-consuming-patterns) specified
/// before or after, currently that includes only [`:idx`](#idx).
///
/// This is followed by the keyword `in`, an array literal `[...]` containing
/// the tokens to iterate and the [body](#body) marked with either `$*` or `$:`.
///
/// ## Single variable binding
/// `$` [`name`](#names) `:` [`type`](#types)
/// ```
/// # use forr::forr;
/// forr! { $val:expr in [1, 2 + 4, 20]
/// # $* assert_eq!($val, $val); }
/// ```
/// `$val` will be `1`, `2 + 4` and `20`.
///
/// ## Tuple binding
/// `(` [`$name:type`](#single-variable-binding), ... `)`
/// ```
/// # use forr::forr;
/// forr! { ($val:expr, $vbl:ty) in [(1, i32), (Ok(2 + 4), Result<u8, ()>), (20.0, f32)]
/// # $* let a: $vbl = $val; assert_eq!(a, $val); }
/// ```
/// `$val` will be `1`, `Ok(2 + 4)` and `20.0`.
///
/// `$vbl` will be `i32`, `Result<u8, ()>` and `f32`.
///
/// ## Non consuming patterns
///
/// Non consuming patterns can be specified before or after the consuming
/// patterns or inside if using a tuple binding.
///
/// Currently, only [`:idx`](#idx) is supported
/// ```
/// # use forr::forr;
/// forr! { $val:expr, $i:idx in [1, 2]
/// # $* assert_eq!($i + 1, $val); }
/// forr! { $i:idx, $val:expr in [1, 2]
/// # $* assert_eq!($i + 1, $val); }
/// forr! { $i:idx, ($val:expr, $vbl:ty) in [(1, i32), (2, i32)]
/// # $* assert_eq!($i + 1, $val); }
/// forr! { ($val:expr, $vbl:ty), $i:idx in [(1, i32), (2, i32)]
/// # $* assert_eq!($i + 1, $val); }
/// forr! { ($val:expr, $i:idx, $vbl:ty) in [(1, i32), (2, i32)]
/// # $* assert_eq!($i + 1, $val); }
/// ```
/// `$val` will be `1` and `2`
///
/// `$i` will be `0` and `1`
///
/// `$vbl` will be `i32`
///
/// ## Body
///
/// The body can be in two different modes. When it is initialized with `$*` the
/// whole body is repeated similar to a normal for loop. Is it started with
/// `$:`, the body will behave more like macro expansion using `$()*` for
/// repetition. In both cases there is special handling for [optional
/// values](#optional values) when placed inside `$()?` the innermost such group
/// is only added if the value is present.
///
/// ### `$*` outer repetition
///
/// In the tokens following the `$*` every occurrence of a `$ident` where the
/// ident matches one of the declared variables is replaced with the
/// corresponding value.
/// ```
/// # use forr::forr;
/// forr! {$val:expr in [(1, "a", true)] $*
///     assert_eq!($val, $val);
/// }
/// ```
/// will expand to
/// ```
/// assert_eq!(1, 1);
/// assert_eq!("a", "a");
/// assert_eq!(true, true);
/// ```
///
/// ### `$:` inner repetition
///
/// `$:` allows to have non repeated code surrounding the expansion, mainly
/// useful for cases where a macro would not be allowed.
///
/// ```
/// # use forr::forr;
/// # assert!(!
/// forr! {($pat:expr, $res:expr) in [(0, true), (1, false), (2.., true)] $:
///     match 1u8 {
///         $($pat => $res,)*
///     }
/// }
/// # );
/// ```
/// Without the inner repetition this would not be possible, as macros are not
/// allowed as the body of a `match`.
/// ```compile_fail
/// # use forr::forr;
/// # assert!(!
/// match 1 {
///    forr! {($pat:expr, $res:expr) in [(0, true), (1, false), (2.., true)] $*
///        $pat => $res
///    }
/// }
/// # );
/// ```
///
/// # Names
/// Any valid rust idents including keywords can be used to name variables. Note
/// that shadowing does not work, i.e. an inner `forr!` needs to use different
/// names.
///
/// # Types
/// ## `:expr`
/// As this uses [`TokenParser::next_expression()`] this will allow anything
/// that matches the `,` rules for expressions i.e. it cannot contain `;` and no
/// `,` outside turbofishes for Types/Generics (`HashMap::<A, B>`).
///
/// ## `:ty`
/// As this uses [`TokenParser::next_type()`] this will allow anything
/// that matches the `,` rules for types i.e. it cannot contain `;` and no
/// `,` outside `<>` for Generics (`HashMap<A, B>`) and unopened closing `>`.
///
/// ## `:tt`
/// Currently matches exactly one [`proc_macro::TokenTree`], but the plan is to extend this to what [`macro_rule!`'s `:tt` matches](https://doc.rust-lang.org/reference/macros-by-example.html#metavariables).
///
/// ## `:inner`
/// The most versatile type, allowing arbitrary tokens wrapped by any bracket
/// `[`, `{` or `(` ... `)}]`.
///
/// ## `:idx`
/// [Non consuming pattern](#non-consuming-patterns) that will contain the
/// current index.
#[manyhow(proc_macro)]
pub use forr::forr;

mod iff;

/// `if` for macro expansions.
///
/// Aims to be an alternative version of
/// [`#[cfg(...)]`](https://doc.rust-lang.org/reference/conditional-compilation.html#the-cfg-attribute)
/// able to conditionally enable any rust tokens and being able to, e.g. compare
/// tokens, but it is not able to be conditional other actual `cfg` or features.
///
/// ```
/// # forr::
/// iff! { true && false $:
///     compile_error!("This is not expanded")
/// }
/// ```
/// ```compile_fail
/// # forr::
/// iff! { true || false $:
///     compile_error!("This is expanded")
/// }
/// ```
///
/// On top of the basic boolean operations (`!`, `&&`, `||`) there are some
/// functions:
///
/// - `empty(<tokens>)` tests if `<tokens>` is empty.
/// - `equals(<lhs>)(<rhs>)` tests if `<lhs>` is equal to `<rhs>`.
/// - `equals_any(<lhs>)[(<rhs>), (<rhs>), ...]` tests if `<lhs>` is equal to
///   any `<rhs>`.
///
/// ```compile_fail
/// # use forr::
/// iff! { empty() $:
///     compile_error!("This is expanded")
/// }
/// ```
/// ```
/// # use forr::iff;
/// iff! { empty(something) $:
///     compile_error!("This is not expanded")
/// }
/// iff! { equals(something)(another thing) $:
///     compile_error!("Neither is this")
/// }
/// ```
#[manyhow(proc_macro)]
pub use iff::iff;
