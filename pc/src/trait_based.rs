/// Model after Iterator as a design, similar to how map, fold, and all the various other
/// iterator methods use intermediary structs to model their desired functionality.
/// Design idea:
///
/// Parser has an associated Output type
/// Parser has a parse method --> Returns a ParseResult
///
/// Also has various chaining methods. These methods all return other structs that implement
/// Parser. The final parse method call is the one that will actually evaluate the parser.
pub trait Parser {
    type Output;

    /// Generic parse method from input to the output type specified
    fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Output>;

    fn or_else<T>(self, other: T) -> OrElse<Self, T>
    where
        Self: Sized,
        T: Parser<Output = Self::Output>,
    {
        OrElse {
            parser_a: self,
            parser_b: other,
        }
    }

    fn and_then<U>(self, other: U) -> AndThen<Self, U>
    where
        Self: Sized,
        U: Parser,
    {
        AndThen {
            parser_a: self,
            parser_b: other,
        }
    }
}

/// Blanket implementation for all parser references
impl<P: Parser> Parser for &P {
    type Output = P::Output;

    fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Output> {
        (**self).parse(input)
    }
}

/// Is either the parsed Output type along with remaining input, or an error with the unconsumed
/// input. Even though the input is present on both sides, modeling it in this way allows for
/// easier chaining using Result methods.
pub type ParseResult<'a, Output> = Result<(Output, &'a str), (ParseError, &'a str)>;

/// Different parsing errors
#[derive(Debug)]
pub enum ParseError {
    /// Expected end of input, but input was still present
    ExpectedEofGotCharacter(char),

    /// Expected a specific character, got end of input
    ExpectedSpecificGotEof(char),

    /// Expected a specific character, got something else specific
    ExpectedSpecificGotSpecific(char, char),

    /// Expected a general concept (e.g an identifier), but got end of input
    ExpectedConceptGotEof(String),

    /// Expected a general concept (e.g an identifier), but got something specific
    ExpectedConceptGotSpecific(String, char),

    /// Unexpected EOF
    UnexpectedEof,
}

pub struct Any;

impl Parser for Any {
    type Output = char;

    fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Output> {
        match input.chars().next() {
            Some(ch) => Ok((ch, &input[ch.len_utf8()..])),
            None => Err((
                ParseError::ExpectedConceptGotEof("a character".to_owned()),
                input,
            )),
        }
    }
}

pub struct Character(char);

impl Parser for Character {
    type Output = char;

    fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Output> {
        match input.chars().next() {
            Some(ch) if ch == self.0 => Ok((ch, &input[ch.len_utf8()..])),
            Some(ch) => Err((ParseError::ExpectedSpecificGotSpecific(self.0, ch), input)),
            None => Err((ParseError::ExpectedSpecificGotEof(self.0), input)),
        }
    }
}

pub struct Symbol(String);

impl Parser for Symbol {
    type Output = String;

    fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Output> {
        if input.starts_with(&self.0) {
            Ok((self.0.clone(), &input[self.0.len()..]))
        } else {
            Err(match input.chars().next() {
                Some(ch) => (
                    ParseError::ExpectedConceptGotSpecific(format!("symbol {}", self.0), ch),
                    input,
                ),
                None => (
                    ParseError::ExpectedConceptGotEof(format!("symbol {}", self.0)),
                    input,
                ),
            })
        }
    }
}

pub struct Until<P: Parser>(P);

impl<P: Parser> Parser for Until<P> {
    type Output = String;

    fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Output> {
        let mut result = "".to_owned();
        let mut ended_at = None;

        for (i, ch) in input.char_indices() {
            let current_string = &input[i..];

            match self.0.parse(current_string) {
                Ok(_) => {
                    // Since the current string parsed successfully, we do NOT consume this
                    // character onward and end here
                    ended_at = Some(i);
                    break;
                }
                Err(_) => {
                    result.push(ch);
                }
            }
        }

        match ended_at {
            Some(pos) => Ok((result, &input[pos..])),
            None => Err((ParseError::UnexpectedEof, input)),
        }
    }
}

pub struct Map<P, F>(P, F);

impl<P, F, A, B> Parser for Map<P, F>
where
    P: Parser<Output = A>,
    F: Fn(A) -> B,
{
    type Output = B;

    fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Output> {
        self.0
            .parse(input)
            .map(|(parsed, rest)| ((self.1)(parsed), rest))
    }
}

pub struct Eof;

impl Parser for Eof {
    type Output = ();

    fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Output> {
        match input.chars().next() {
            Some(ch) => Err((ParseError::ExpectedEofGotCharacter(ch), input)),
            None => Ok(((), input)),
        }
    }
}

pub struct OrElse<A, B> {
    parser_a: A,
    parser_b: B,
}

impl<A, B> Parser for OrElse<A, B>
where
    A: Parser,
    B: Parser<Output = A::Output>,
{
    type Output = A::Output;

    fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Output> {
        self.parser_a
            .parse(input)
            .or_else(|_| self.parser_b.parse(input))
    }
}

pub struct AndThen<A, B> {
    parser_a: A,
    parser_b: B,
}

impl<A, B> Parser for AndThen<A, B>
where
    A: Parser,
    B: Parser,
{
    type Output = (A::Output, B::Output);

    fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Output> {
        self.parser_a.parse(input).and_then(|(token, rest)| {
            let (token2, rest2) = self.parser_b.parse(rest)?;

            Ok(((token, token2), rest2))
        })
    }
}

/// Functional interface to the above structs/traits

/// Evaluates a parser
pub fn run_parser<'a, T: Parser>(parser: T, input: &'a str) -> ParseResult<'a, T::Output> {
    parser.parse(input)
}

/// Parses any character
pub fn any() -> Any {
    Any
}

/// Parses a specific character
pub fn character(ch: char) -> Character {
    Character(ch)
}

/// Parses end of input
pub fn eof() -> Eof {
    Eof
}

/// Run one parser, if that results in an error, then return the result of the second parser
pub fn or_else<A, B>(parser_a: A, parser_b: B) -> OrElse<A, B>
where
    A: Parser,
    B: Parser<Output = A::Output>,
{
    OrElse { parser_a, parser_b }
}

/// Combine the results of two parsers run in sequence. If one errors, its error is returned
/// instead
pub fn and_then<A, B>(parser_a: A, parser_b: B) -> AndThen<A, B>
where
    A: Parser,
    B: Parser,
{
    AndThen { parser_a, parser_b }
}

/// Parse a symbol, e.g a specific sequence of characters
pub fn symbol(string: &str) -> Symbol {
    Symbol(string.to_owned())
}

/// Consumes input until the passed parser successfully returns a result. Upon doing so, returns
/// all the input prior to that consumed input. If the parser never succeeds, returns an error.
pub fn until<P: Parser>(parser: P) -> Until<P> {
    Until(parser)
}

/// Map's the successful result of a parser to a passed function
pub fn map<A, B, P, F>(parser: P, func: F) -> Map<P, F>
where
    P: Parser<Output = A>,
    F: Fn(A) -> B,
{
    Map(parser, func)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_any() {
        let (parsed, rest) = run_parser(any(), "input").unwrap();
        assert_eq!(parsed, 'i');
        assert_eq!(rest, "nput");

        let (error, rest) = run_parser(any(), "").unwrap_err();
        assert!(matches!(error, ParseError::ExpectedConceptGotEof(..)));
        assert_eq!(rest, "");
    }

    #[test]
    fn test_eof() {
        let (parsed, rest) = run_parser(eof(), "").unwrap();
        assert_eq!(parsed, ());
        assert_eq!(rest, "");

        let (error, rest) = run_parser(eof(), "a").unwrap_err();
        assert!(matches!(error, ParseError::ExpectedEofGotCharacter('a')));
        assert_eq!(rest, "a");
    }

    #[test]
    fn test_character() {
        let (parsed, rest) = run_parser(character('i'), "input").unwrap();
        assert_eq!(parsed, 'i');
        assert_eq!(rest, "nput");

        let (error, rest) = run_parser(character('i'), "anput").unwrap_err();
        assert!(matches!(
            error,
            ParseError::ExpectedSpecificGotSpecific('i', 'a')
        ));
        assert_eq!(rest, "anput");

        let (error, rest) = run_parser(character('i'), "").unwrap_err();
        assert!(matches!(error, ParseError::ExpectedSpecificGotEof(..)));
        assert_eq!(rest, "");
    }

    #[test]
    fn test_or_else() {
        let parser = or_else(character('i'), character('a'));

        let (parsed, rest) = run_parser(&parser, "input").unwrap();
        assert_eq!(parsed, 'i');
        assert_eq!(rest, "nput");

        let (parsed, rest) = run_parser(&parser, "anput").unwrap();
        assert_eq!(parsed, 'a');
        assert_eq!(rest, "nput");

        let (error, rest) = run_parser(&parser, "bnput").unwrap_err();
        assert!(matches!(
            error,
            ParseError::ExpectedSpecificGotSpecific('a', 'b')
        ));
        assert_eq!(rest, "bnput");
    }

    #[test]
    fn test_and_then() {
        let parser = and_then(character('i'), character('n'));

        let (parsed, rest) = run_parser(&parser, "input").unwrap();
        assert_eq!(parsed, ('i', 'n'));
        assert_eq!(rest, "put");

        let (error, rest) = run_parser(&parser, "anput").unwrap_err();
        assert!(matches!(
            error,
            ParseError::ExpectedSpecificGotSpecific('i', 'a')
        ));
        assert_eq!(rest, "anput");

        let (error, rest) = run_parser(&parser, "iaput").unwrap_err();
        assert!(matches!(
            error,
            ParseError::ExpectedSpecificGotSpecific('n', 'a')
        ));
        assert_eq!(rest, "aput");
    }

    #[test]
    fn test_combining_or_else_and_then() {
        let in_or_ou = or_else(
            and_then(character('i'), character('n')),
            and_then(character('o'), character('u')),
        );

        let (parsed, rest) = run_parser(&in_or_ou, "input").unwrap();
        assert_eq!(parsed, ('i', 'n'));
        assert_eq!(rest, "put");

        let (parsed, rest) = run_parser(&in_or_ou, "output").unwrap();
        assert_eq!(parsed, ('o', 'u'));
        assert_eq!(rest, "tput");

        let (error, rest) = run_parser(&in_or_ou, "random").unwrap_err();

        assert!(matches!(
            error,
            ParseError::ExpectedSpecificGotSpecific('o', 'r')
        ));
        assert_eq!(rest, "random");
    }

    #[test]
    fn test_chaining_or_else() {
        let digit_parser = character('0')
            .or_else(character('1'))
            .or_else(character('2'))
            .or_else(character('3'))
            .or_else(character('4'))
            .or_else(character('5'))
            .or_else(character('5'))
            .or_else(character('7'))
            .or_else(character('8'))
            .or_else(character('9'));

        let (parsed, rest) = run_parser(&digit_parser, "943").unwrap();

        assert_eq!(parsed, '9');
        assert_eq!(rest, "43");
    }

    #[test]
    fn test_chaining_and_then() {
        let digit_parser = character('0')
            .and_then(character('1'))
            .and_then(character('2'))
            .and_then(character('3'))
            .and_then(character('4'));

        let (parsed, rest) = run_parser(&digit_parser, "01234").unwrap();

        assert_eq!(parsed, (((('0', '1'), '2'), '3'), '4'));
        assert_eq!(rest, "");
    }

    #[test]
    fn test_symbol() {
        let keyword_parser = symbol("if");

        let (parsed, rest) = run_parser(&keyword_parser, "if (a == b) {}").unwrap();

        assert_eq!(parsed, "if");
        assert_eq!(rest, " (a == b) {}");

        let (error, rest) = run_parser(&keyword_parser, "(a == b) {}").unwrap_err();

        assert!(matches!(error, ParseError::ExpectedConceptGotSpecific(..)));
        assert_eq!(rest, "(a == b) {}");

        let (error, rest) = run_parser(&keyword_parser, "").unwrap_err();

        assert!(matches!(error, ParseError::ExpectedConceptGotEof(..)));
        assert_eq!(rest, "");
    }

    #[test]
    fn test_until() {
        let quote = character('"');
        let string_parser = and_then(and_then(&quote, until(&quote)), &quote);

        let (parsed, rest) = run_parser(&string_parser, "\"a string\" everything else").unwrap();

        assert_eq!(parsed, (('"', "a string".to_string()), '"'));
        assert_eq!(rest, " everything else");

        let (error, rest) = run_parser(&string_parser, "\"a string").unwrap_err();

        assert!(matches!(error, ParseError::UnexpectedEof));
        assert_eq!(rest, "a string");
    }

    #[test]
    fn test_map() {
        let digit_char_parser = character('0')
            .or_else(character('1'))
            .or_else(character('2'))
            .or_else(character('3'))
            .or_else(character('4'));

        let digit_parser = map(&digit_char_parser, |ch| {
            ch.to_digit(10).expect("ideally not possible")
        });

        let (parsed, rest) = run_parser(&digit_parser, "4").unwrap();

        assert_eq!(parsed, 4);
        assert_eq!(rest, "");

        let (error, rest) = run_parser(&digit_parser, "a").unwrap_err();

        assert!(matches!(error, ParseError::ExpectedSpecificGotSpecific(..)));
        assert_eq!(rest, "a");
    }
}
