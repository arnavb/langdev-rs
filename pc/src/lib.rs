use std::fmt::{Debug, Display};

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

fn nothing() -> ParseError {
    ParseError::new("Expected anything, got nothing")
}

/// A parser takes an input (e.g a string), and turns it into a parsed token or error and
/// remaining input. We implement it as a subtrait as function aliases would require a type alias
/// for impl Trait's, an unstable feature. Creating this subtrait and then implementing it for all
/// blanket types effectively accomplishes the same thing.
pub trait Parser<T>: Fn(&str) -> (Result<T, ParseError>, &str) {}

impl<T, F> Parser<T> for F where F: Fn(&str) -> (Result<T, ParseError>, &str) {}

/// Match any character
pub fn any_char(input: &str) -> (Result<char, ParseError>, &str) {
    match input.chars().next() {
        Some(ch) => (Ok(ch), &input[ch.len_utf8()..]),
        None => (Err(nothing()), input),
    }
}

/// Match a specific character
/// (could be implemented in terms of any_char(), but that doesn't add much
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

/// Run either of two parsers of the same type. If the first succeeds, return its result.
/// Otherwise return the result of the second parser. If any parser fails, input should not be
/// consumed with respect to that parser.
pub fn or_else<T>(parser1: impl Parser<T>, parser2: impl Parser<T>) -> impl Parser<T> {
    move |input| {
        let (result1, remaining1) = parser1(input);

        match result1 {
            Ok(token1) => (Ok(token1), remaining1),
            Err(_) => parser2(remaining1),
        }
    }
}

/// Implement choice, which is essentially equivalent to folding over or_else: keep trying parsers
/// in a collection until one works, and if none do, return the last result. Not actually
/// implemented using or_else, because this is difficult to statically do with Rust and how traits
/// work (they end up being different opaque types, which can't be reduced).
pub fn choice<T>(parsers: &[impl Parser<T>]) -> impl Parser<T> {
    move |input| {
        let [first, rest_parsers @ ..] = parsers else {
            return (Err(nothing()), input);
        };

        // Will store successive error, and return the last one if no parser
        // succeeds
        let (mut result, remaining) = first(input);

        if result.is_ok() {
            return (result, remaining);
        }

        for parser in rest_parsers {
            result = match parser(input) {
                (Ok(token), remaining) => return (Ok(token), remaining),
                (Err(err), _) => Err(err),
            }
        }

        // If we reached here, no parser succeeded -> No input was consumed
        (result, input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn any_char_should_parse_any_character() {
        let (token, rest) = any_char("a");

        assert!(matches!(token, Ok('a')));
        assert_eq!(rest, "");
    }

    #[test]
    fn when_passed_nothing_any_char_should_error() {
        let (token, rest) = any_char("");

        assert!(token.is_err());
        assert_eq!(rest, "");
    }

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

        assert!(token.is_err());
        assert_eq!(rest, "");
    }

    #[test]
    fn and_then_should_parse_two_in_succession() {
        let parser_a = char_parser('a');
        let parser_b = char_parser('b');

        let a_and_then_b = and_then(&parser_a, &parser_b);

        let (result, rest) = a_and_then_b("abc");

        assert!(matches!(result, Ok(('a', 'b'))));
        assert_eq!(rest, "c");
    }

    #[test]
    fn if_one_parser_fails_then_and_then_should_error() {
        let parser_a = char_parser('a');
        let parser_b = char_parser('b');

        let a_and_then_b = and_then(&parser_a, &parser_b);

        // First parser fails
        let (result, rest) = a_and_then_b("zbc");

        assert!(result.is_err());
        assert_eq!(rest, "zbc");

        // Second parser fails
        let (result, rest) = a_and_then_b("azc");

        assert!(result.is_err());
        assert_eq!(rest, "zc");
    }

    #[test]
    fn or_else_should_parse_either_of_two_parsers() {
        let parser_a = char_parser('a');
        let parser_b = char_parser('b');

        let a_or_else_b = or_else(&parser_a, &parser_b);

        let (result, rest) = a_or_else_b("a");

        assert!(matches!(result, Ok('a')));
        assert_eq!(rest, "");

        let (result, rest) = a_or_else_b("b");

        assert!(matches!(result, Ok('b')));
        assert_eq!(rest, "");
    }

    #[test]
    fn if_both_parsers_fail_then_or_else_should_error() {
        let parser_a = char_parser('a');
        let parser_b = char_parser('b');

        let a_or_else_b = or_else(&parser_a, &parser_b);

        let (result, rest) = a_or_else_b("c");

        assert!(result.is_err());
        assert_eq!(rest, "c");
    }

    #[test]
    fn should_be_able_to_combine_and_then_and_or_else() {
        let parser_a = char_parser('a');
        let parser_b = char_parser('b');
        let parser_c = char_parser('c');

        let b_or_else_c = or_else(&parser_b, &parser_c);
        let a_and_then_b_or_else_c = and_then(&parser_a, &b_or_else_c);

        let (result, rest) = a_and_then_b_or_else_c("abz");

        assert!(matches!(result, Ok(('a', 'b'))));
        assert_eq!(rest, "z");

        let (result, rest) = a_and_then_b_or_else_c("acz");

        assert!(matches!(result, Ok(('a', 'c'))));
        assert_eq!(rest, "z");

        let (result, rest) = a_and_then_b_or_else_c("zbz");

        assert!(result.is_err());
        assert_eq!(rest, "zbz");

        println!("{:?}", result);

        let (result, rest) = a_and_then_b_or_else_c("azz");

        assert!(result.is_err());
        assert_eq!(rest, "zz");

        // TODO: Better error messages (e.g show that it was B OR C that was expected, not just C
        println!("{:?}", result);
    }

    #[test]
    fn if_given_list_of_parser_choice_should_try_each_one() {
        let lowercase_parsers = ('a'..='z').map(char_parser).collect::<Vec<_>>();
        let any_lowercase = choice(&lowercase_parsers);

        for letter in 'a'..='z' {
            let input = &format!("{letter}random");
            let (result, rest) = any_lowercase(input);

            assert!(matches!(result, Ok(l) if letter == l));
            assert_eq!(rest, "random");
        }
    }

    #[test]
    fn if_given_list_parser_choice_and_none_match_should_return_error() {
        // Exclude z
        let lowercase_parsers = ('a'..='y').map(char_parser).collect::<Vec<_>>();
        let any_lowercase = choice(&lowercase_parsers);

        let input = &format!("zrandom");
        let (result, rest) = any_lowercase(input);

        assert!(result.is_err());
        assert_eq!(rest, "zrandom");
    }

    #[test]
    fn if_given_heterogenous_parsers_with_same_return_should_be_able_to_choice_over_them() {
        // Exclude z
        let lowercase_parsers = ('a'..='y').map(char_parser).collect::<Vec<_>>();

        // It's not really possible to represent this as a closure directly, because of
        // the lack of higher ranked trait bounds in order to represent that the returned
        // string slice must live as long as the input. We can write a function to
        // constrain, e.g like https://stackoverflow.com/a/46198877/6525260
        let other_parser: fn(&str) -> (Result<char, ParseError>, &str) = |input| (Ok('z'), input);

        let mut combined_list = lowercase_parsers
            .iter()
            .map(|p| p as &dyn Parser<char>)
            .collect::<Vec<_>>();

        combined_list.push(&other_parser);

        // This should parse any character outside a..=y as z
        let combined_parser = choice(&combined_list);

        let (token, rest) = combined_parser("$");

        assert!(matches!(token, Ok('z')));
        assert_eq!(rest, "$");
    }
}
