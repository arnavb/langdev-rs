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
}
