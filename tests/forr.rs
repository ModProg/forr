use std::fmt::Display;
use std::str::FromStr;

use forr::forr;

forr! { ($name:tt, $str:expr, $alt:expr?) in [(A, "a", "ä"), (B, "b"), (C, "c")] $:
    #[derive(PartialEq, Debug)]
    enum Enum { $($name,)* }

    impl Display for Enum {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                $(Self::$name => write!(f, $str),)*
            }
        }
    }
    impl FromStr for Enum {
        type Err = String;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            match s {
                $(
                    $str => Ok(Self::$name),
                    $($alt => Ok(Self::$name),)?
                )*
                other => Err(format!("Unknown variant {other}")),
            }
        }
    }
}

#[test]
fn enum_test() {
    let a = Enum::A.to_string();
    assert_eq!(a, "a");
    let a: Enum = a.parse().unwrap();
    assert_eq!(a, Enum::A);
    let a: Enum = "ä".parse().unwrap();
    assert_eq!(a, Enum::A);
    let b = Enum::B.to_string();
    assert_eq!(b, "b");
    let b: Enum = b.parse().unwrap();
    assert_eq!(b, Enum::B);
    let c = Enum::C.to_string();
    assert_eq!(c, "c");
    let c: Enum = c.parse().unwrap();
    assert_eq!(c, Enum::C);
}
