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

/// Combine two parsers in succession, and return a tuple of their combined results. If either
/// parser fails, then the corresponding input is not consumed and the error is returned.
pub fn and_then<T, U>(parser1: impl Parser<T>, parser2: impl Parser<U>) -> impl Parser<(T, U)> {
    move |input| {
        let (result1, remaining1) = parser1(input);

        match result1 {
            Ok(token1) => {
                let (result2, remaining2) = parser2(remaining1);

                match result2 {
                    Ok(token2) => (Ok((token1, token2)), remaining2),
                    // Don't consume second input if second parser errors
                    Err(err2) => (Err(err2), remaining1),
                }
            }
            // Don't consume input if first parser errors
            Err(err1) => (Err(err1), input),
        }
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

    #[test]
    fn and_then_should_parse_two_in_succession() {
        let parser_a = char_parser('a');
        let parser_b = char_parser('b');

        let a_and_then_b = and_then(parser_a, parser_b);

        let (result, rest) = a_and_then_b("abc");

        assert!(matches!(result, Ok(('a', 'b'))));
        assert_eq!(rest, "c");
    }

    #[test]
    fn if_one_parser_fails_then_and_then_should_error() {
        let parser_a = char_parser('a');
        let parser_b = char_parser('b');

        let a_and_then_b = and_then(parser_a, parser_b);

        // First parser fails
        let (result, rest) = a_and_then_b("zbc");

        assert!(result.is_err());
        assert_eq!(rest, "zbc");

        // Second parser fails
        let (result, rest) = a_and_then_b("azc");

        assert!(result.is_err());
        assert_eq!(rest, "zc");
    }
}
