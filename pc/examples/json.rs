/// Build a representation of a JSON document that can parse a document into an abstract syntax
/// tree and then recreate the source exactly.
use pc::trait_based::{
    any, character, lazy, map, one_or_more, optional, predicate, run_parser, symbol, zero_or_more,
    Parser,
};

struct Json(Element);

fn json() -> impl Parser<Output = Element> {
    element()
}

#[derive(Debug)]
enum Value {
    Object(Object),
    Array(Array),
    String(JString),
    Number(Number),
    /// Covers true/false in the value McKeeman Form
    Boolean(bool),
    Null,
}

fn value() -> impl Parser<Output = Value> {
    let object_p = object().map(|o| Value::Object(o));
    let array_p = array().map(|a| Value::Array(a));
    let string_p = j_string().map(|s| Value::String(s));
    let number_p = number().map(|n| Value::Number(n));
    let boolean_p = symbol("true")
        .or_else(symbol("false"))
        .map(|s: String| match s.as_str() {
            "true" => Value::Boolean(true),
            "false" => Value::Boolean(false),
            _ => unreachable!(),
        });

    let null_p = symbol("null").map(|_: String| Value::Null);

    object_p
        .or_else(array_p)
        .or_else(string_p)
        .or_else(number_p)
        .or_else(boolean_p)
        .or_else(null_p)
}

#[derive(Debug)]
enum Object {
    EmptyObject(Vec<Whitespace>),
    WithMembers(Members),
}

fn object() -> Box<dyn Parser<Output = Object>> {
    let whitespace_only = character('{')
        .and_then(whitespace())
        .right()
        .and_then(character('}'))
        .left()
        .map(|v| Object::EmptyObject(v));

    let with_members = lazy(|| {
        character('{')
            .and_then(members())
            .right()
            .and_then(character('}'))
            .left()
            .map(|v| Object::WithMembers(v))
    });

    Box::new(whitespace_only.or_else(with_members))
}

#[derive(Debug)]
struct Members(Vec<Member>);

fn members() -> impl Parser<Output = Members> {
    // TODO: figure out how to not accept trailing commas
    one_or_more(member().and_then(optional(character(','))).left()).map(|v| Members(v))
}

#[derive(Debug)]
struct Member(Vec<Whitespace>, JString, Vec<Whitespace>, Box<Element>);

fn member() -> impl Parser<Output = Member> {
    whitespace()
        .and_then(j_string())
        .and_then(whitespace())
        .and_then(character(':'))
        .left()
        .and_then(element())
        .map(|(((w1, s), w2), e)| Member(w1, s, w2, Box::new(e)))
}

#[derive(Debug)]
enum Array {
    EmptyArray(Vec<Whitespace>),
    WithElements(Elements),
}

fn array() -> Box<dyn Parser<Output = Array>> {
    let whitespace_only = character('[')
        .and_then(whitespace())
        .right()
        .and_then(character(']'))
        .left()
        .map(|v| Array::EmptyArray(v));

    let with_elements = lazy(|| {
        character('[')
            .and_then(elements())
            .right()
            .and_then(character(']'))
            .left()
            .map(|v| Array::WithElements(v))
    });

    Box::new(whitespace_only.or_else(with_elements))
}

#[derive(Debug)]
struct Elements(Vec<Element>);

fn elements() -> impl Parser<Output = Elements> {
    // TODO: figure out how to not accept trailing commas
    one_or_more(element().and_then(optional(character(','))).left()).map(|v| Elements(v))
}

#[derive(Debug)]
struct Element(Vec<Whitespace>, Value, Vec<Whitespace>);

fn element() -> impl Parser<Output = Element> {
    whitespace()
        .and_then(value())
        .and_then(whitespace())
        .map(|((w1, v), w2)| Element(w1, v, w2))
}

#[derive(Debug)]
struct JString(JCharacters);

fn j_string() -> impl Parser<Output = JString> {
    character('"')
        .and_then(j_characters())
        .right()
        .and_then(character('"'))
        .left()
        .map(|j_characters| JString(j_characters))
}

#[derive(Debug)]
struct JCharacters(Vec<JCharacter>);

fn j_characters() -> impl Parser<Output = JCharacters> {
    zero_or_more(j_character()).map(|v| JCharacters(v))
}

#[derive(Debug)]
enum JCharacter {
    Value(char),
    Escape(Escape),
}

fn j_character() -> impl Parser<Output = JCharacter> {
    predicate(|ch| ch != '"' && ch != '\\')
        .map(|ch| JCharacter::Value(ch))
        .or_else(
            character('\\')
                .and_then(escape().map(|e| JCharacter::Escape(e)))
                .right(),
        )
}

#[derive(Debug)]
enum Escape {
    Quote,
    BackSlash,
    ForwardSlash,
    B,
    F,
    N,
    R,
    T,
    Hex([Hex; 4]),
}

fn escape() -> impl Parser<Output = Escape> {
    let simple_chars_escape = any().map(|ch| match ch {
        '"' => Escape::Quote,
        '\\' => Escape::BackSlash,
        '/' => Escape::ForwardSlash,
        'b' => Escape::B,
        'f' => Escape::F,
        'n' => Escape::N,
        'r' => Escape::R,
        't' => Escape::T,
        _ => unreachable!(),
    });

    let hex_escape = character('u')
        .and_then(hex())
        .right()
        .and_then(hex())
        .and_then(hex())
        .and_then(hex())
        .map(|(((a, b), c), d)| Escape::Hex([a, b, c, d]));

    simple_chars_escape.or_else(hex_escape)
}

#[derive(Debug)]
enum Hex {
    Digit(Digit),
    UppercaseAF(u8),
    LowercaseAF(u8),
}

fn hex() -> impl Parser<Output = Hex> {
    let positive_digit = digit().map(|digit| Hex::Digit(digit));

    let uppercase_af = predicate(|ch| ch >= 'A' && ch <= 'F').map(|ch: char| {
        let ch_u8 = ch.to_digit(16).unwrap() as u8;

        Hex::UppercaseAF(ch_u8)
    });

    let lowercase_af = predicate(|ch| ch >= 'a' && ch <= 'f').map(|ch: char| {
        let ch_u8 = ch.to_digit(16).unwrap() as u8;

        Hex::LowercaseAF(ch_u8)
    });

    positive_digit.or_else(uppercase_af).or_else(lowercase_af)
}

#[derive(Debug)]
struct Number(Integer, Option<Fraction>, Option<Exponent>);

fn number() -> impl Parser<Output = Number> {
    integer()
        .and_then(optional(fraction()))
        .and_then(optional(exponent()))
        .map(|((integer, fraction), exponent)| Number(integer, fraction, exponent))
}

#[derive(Debug)]
enum Integer {
    Digit(Digit),
    OneNineDigits(OneNine, Digits),
    NegativeDigit(Digit),
    NegativeOneNineDigits(OneNine, Digits),
}

fn integer() -> impl Parser<Output = Integer> {
    let positive_digit = digit().map(|digit| Integer::Digit(digit));

    let positive_one_nine_digits = one_nine()
        .and_then(digits())
        .map(|(one_nine, digits)| Integer::OneNineDigits(one_nine, digits));

    let negative_digit = character('-')
        .and_then(digit())
        .right()
        .map(|digit| Integer::NegativeDigit(digit));

    let negative_one_nine_digits = character('-')
        .and_then(one_nine())
        .right()
        .and_then(digits())
        .map(|(one_nine, digits)| Integer::NegativeOneNineDigits(one_nine, digits));

    positive_one_nine_digits
        .or_else(negative_one_nine_digits)
        .or_else(positive_digit)
        .or_else(negative_digit)
}

#[derive(Debug)]
struct Digits(Vec<Digit>);

fn digits() -> impl Parser<Output = Digits> {
    one_or_more(digit()).map(|digits| Digits(digits))
}

/// Distinct from Digit since some places cannot have a 0
#[derive(Debug)]
struct OneNine(u8);

fn one_nine() -> impl Parser<Output = OneNine> {
    predicate(|ch| ch.is_digit(10) && ch != '0')
        .map(|ch: char| OneNine(ch.to_digit(10).unwrap() as u8))
}

/// OneNine and 0
#[derive(Debug)]
struct Digit(u8);

fn digit() -> impl Parser<Output = Digit> {
    predicate(|ch| ch.is_digit(10)).map(|ch: char| Digit(ch.to_digit(10).unwrap() as u8))
}

#[derive(Debug)]
struct Fraction(Digits);

fn fraction() -> impl Parser<Output = Fraction> {
    character('.')
        .and_then(digits())
        .right()
        .map(|digits| Fraction(digits))
}

#[derive(Debug)]
enum Exponent {
    Uppercase(Option<Sign>, Digits),
    Lowercase(Option<Sign>, Digits),
}

fn exponent() -> impl Parser<Output = Exponent> {
    let letter = character('E').or_else(character('e'));

    letter
        .and_then(optional(sign()))
        .and_then(digits())
        .map(|((letter, sign), digits)| match ((letter, sign), digits) {
            (('E', sign), digits) => Exponent::Uppercase(sign, digits),
            (('e', sign), digits) => Exponent::Lowercase(sign, digits),
            _ => unreachable!(),
        })
}

#[derive(Debug)]
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

fn sign() -> impl Parser<Output = Sign> {
    character('+')
        .or_else(character('-'))
        .map(|ch| Sign::try_from(ch).unwrap())
}

#[derive(Debug)]
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

fn whitespace() -> impl Parser<Output = Vec<Whitespace>> {
    zero_or_more(
        character('\u{0020}')
            .or_else(character('\u{000A}'))
            .or_else(character('\u{000D}'))
            .or_else(character('\u{0009}'))
            .map(|ch| Whitespace::try_from(ch).unwrap()),
    )
}

fn main() {
    let example = r#"
    {
  "bookTitle": "The JSON Handbook",
  "author": "Jane Doe",
  "yearPublished": 2023,
  "isAvailable": true,
  "chapters": [
    "Introduction to JSON",
    "Data Types & Syntax",
    "Working with Arrays",
    "Real-World Examples"
  ],
  "publisher": {
    "name": "Tech Publishers Inc.",
    "location": "San Francisco"
  },
  "ISBN": null
}
    "#;

    println!("{}", example);

    let (parsed, rest) = run_parser(json(), example).unwrap();

    println!("{:#?}", parsed);
}
