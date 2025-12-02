/// Build a representation of a JSON document that can parse a document into an abstract syntax
/// tree and then recreate the source exactly.
use pc::trait_based::{character, Parser};

struct Json(Element);

enum Value {
    Object(Object),
    Array(Array),
    String(JString),
    Number(Number),
    /// Covers true/false in the value McKeeman Form
    Boolean(bool),
    Null,
}

enum Object {
    Empty,
    WithMembers(Members),
}

struct Members(Vec<Member>);

struct Member(Vec<Whitespace>, JString, Vec<Whitespace>, Element);

enum Array {
    EmptyArray(Vec<Whitespace>),
    WithElements(Elements),
}

struct Elements(Vec<Element>);

struct Element(Vec<Whitespace>, Value, Vec<Whitespace>);

struct JString(Characters);

struct Characters(Vec<Character>);

enum Character {
    Value(char),
    Escape(Escape),
}

enum Escape {
    Quote,
    BackSlash,
    ForwardSlash,
    B,
    F,
    N,
    R,
    T,
    Hex(Hex),
}

enum Hex {
    Digit(Digit),
    UppercaseAF(u8),
    LowercaseAF(u8),
}

struct Number(Integer, Option<Fraction>, Option<Exponent>);

enum Integer {
    Digit(Digit),
    OneNineDigits(OneNine, Digits),
    NegativeDigit(Digit),
    NegativeOneNineDigits(OneNine, Digits),
}

struct Digits(Vec<Digit>);

/// Distinct from Digit since some places cannot have a 0
struct OneNine(u8);

/// OneNine and 0
struct Digit(u8);

struct Fraction(Digits);

enum Exponent {
    Uppercase(Option<Sign>, Digits),
    Lowercase(Option<Sign>, Digits),
}

enum Sign {
    Plus,
    Minus,
}

impl TryFrom<char> for Sign {
    type Error = ();

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            '+' => Ok(Sign::Plus),
            '-' => Ok(Sign::Minus),
            _ => Err(()),
        }
    }
}

enum Whitespace {
    Space,
    LineFeed,
    CarriageReturn,
    HorizontalTab,
}

impl TryFrom<char> for Whitespace {
    type Error = ();

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            '\u{0020}' => Ok(Whitespace::Space),
            '\u{000A}' => Ok(Whitespace::LineFeed),
            '\u{000D}' => Ok(Whitespace::CarriageReturn),
            '\u{0009}' => Ok(Whitespace::HorizontalTab),
            _ => Err(()),
        }
    }
}

fn main() {
    println!("Hello, World\n");
}
