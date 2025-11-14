use std::fmt::Display;

#[derive(Debug)]
pub struct ParseError {
    message: String,
}

impl ParseError {
    fn new(message: &str) -> Self {
        ParseError {
            message: message.to_owned(),
        }
    }
}

fn expect_specific<T: Display>(expected: &T, actual: &T) -> ParseError {
    ParseError::new(&format!("Expected '{}', got '{}'", expected, actual))
}

fn expect<T: Display>(expected: &T, actual: &str) -> ParseError {
    ParseError::new(&format!("Expected '{}', got {}", expected, actual))
}

/// A parser takes an input (e.g a string), and turns it into a parsed token or error and
/// remaining input. We implement it as a subtrait as function aliases would require a type alias
/// for impl Trait's, an unstable feature. Creating this subtrait and then implementing it for all
/// blanket types effectively accomplishes the same thing.
pub trait Parser<T>: Fn(&str) -> (Result<T, ParseError>, &str) {}

impl<T, F> Parser<T> for F where F: Fn(&str) -> (Result<T, ParseError>, &str) {}

pub fn char_parser(to_match: char) -> impl Parser<char> {
    move |input| match input.chars().next() {
        Some(ch) if ch == to_match => (Ok(ch), &input[to_match.len_utf8()..]),
        Some(ch) => (Err(expect_specific(&to_match, &ch)), input),
        None => (Err(expect(&to_match, "nothing")), input),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn char_parser_should_return_a_parser() {
        let a_parser = char_parser('a');

        assert!(matches!(a_parser("abc"), (Ok('a'), "bc")));
    }

    #[test]
    fn parse_char_can_be_called_multiple_times() {
        let a_parser = char_parser('a');

        let input = "aac";

        let (token, rest) = a_parser(input);

        assert!(matches!(token, Ok('a')));

        let (token, rest) = a_parser(rest);

        assert!(matches!(token, Ok('a')));

        assert_eq!(rest, "c");
    }

    #[test]
    fn parse_non_matching_should_return_error() {
        let a_parser = char_parser('a');

        let (token, rest) = a_parser("zyx");

        assert!(token.is_err());
        assert_eq!(rest, "zyx");
    }

    #[test]
    fn parse_empty_string_should_return_error() {
        let a_parser = char_parser('a');

        let (token, rest) = a_parser("");

        println!("{:?}", token);

        assert!(token.is_err());
        assert_eq!(rest, "");
    }
}
