use std::{iter::Peekable, str::FromStr};

use crate::term::Term;

impl FromStr for Term {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let tokens = tokens(s)?;
        let term = new_parser(tokens).term()?;
        Ok(term)
    }
}

#[derive(Debug, PartialEq, Eq)]
enum TermToken {
    Variable { name: char },
    Whitespace,
    LeftParenthesis,
    RightParenthesis,
    Zero,
    Successor,
    Plus,
    Star,
}

#[derive(Debug, PartialEq, Eq)]
pub struct InvalidCharacter(char);

fn tokens(input: &str) -> Result<impl Iterator<Item = TermToken> + '_, InvalidCharacter> {
    let mut tokens = Vec::with_capacity(input.len());
    for c in input.chars() {
        tokens.push(match c {
            'a'..='z' => TermToken::Variable { name: c },
            '(' => TermToken::LeftParenthesis,
            ')' => TermToken::RightParenthesis,
            '0' => TermToken::Zero,
            'S' => TermToken::Successor,
            '+' => TermToken::Plus,
            '*' => TermToken::Star,
            c if c.is_whitespace() => TermToken::Whitespace,
            c => return Err(InvalidCharacter(c)),
        });
    }
    Ok(tokens.into_iter())
}

struct TermParser<I>
where
    I: Iterator<Item = TermToken>,
{
    tokens: Peekable<I>,
}

fn new_parser<J: Iterator<Item = TermToken>>(
    iter: J,
) -> TermParser<impl Iterator<Item = TermToken>> {
    TermParser {
        tokens: iter.filter(|t| t != &TermToken::Whitespace).peekable(),
    }
}

impl<I> TermParser<I>
where
    I: Iterator<Item = TermToken>,
{
    pub fn term(&mut self) -> Result<Term, ParseError> {
        let term = self.add()?;
        if self.tokens.next().is_none() {
            Ok(term)
        } else {
            Err(ParseError)
        }
    }

    fn add(&mut self) -> Result<Term, ParseError> {
        let mut left = self.mul()?;

        while let Some(TermToken::Plus) = self.tokens.next_if_eq(&TermToken::Plus) {
            let right = self.mul()?;
            left = Term::Add(Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    fn mul(&mut self) -> Result<Term, ParseError> {
        let mut left = self.successor()?;

        while let Some(TermToken::Star) = self.tokens.next_if_eq(&TermToken::Star) {
            let right = self.successor()?;
            left = Term::Mul(Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    fn successor(&mut self) -> Result<Term, ParseError> {
        match self.tokens.next_if_eq(&TermToken::Successor) {
            Some(TermToken::Successor) => Ok(Term::S(Box::new(self.successor()?))),
            _ => self.zero_or_variable(),
        }
    }

    fn zero_or_variable(&mut self) -> Result<Term, ParseError> {
        match self.tokens.next() {
            Some(TermToken::Zero) => Ok(Term::Zero),
            Some(TermToken::Variable { name }) => Ok(Term::Variable(name.into())),
            Some(TermToken::LeftParenthesis) => {
                let term = self.add()?;
                if let Some(TermToken::RightParenthesis) = self.tokens.next() {
                    Ok(term)
                } else {
                    Err(ParseError)
                }
            }
            _ => Err(ParseError),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    TokenizerError(InvalidCharacter),
    ParserError(ParseError),
}

impl From<InvalidCharacter> for Error {
    fn from(i: InvalidCharacter) -> Self {
        Self::TokenizerError(i)
    }
}

impl From<ParseError> for Error {
    fn from(p: ParseError) -> Self {
        Self::ParserError(p)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParseError;

#[cfg(test)]
mod test {
    use crate::term::Term;

    #[test]
    fn parse_test() {
        assert_eq!(
            "SS0*y+S0".parse(),
            Ok(Term::Add(
                Term::Mul(
                    Term::S(Term::S(Term::Zero.into()).into()).into(),
                    Term::Variable('y'.into()).into()
                )
                .into(),
                Term::S(Term::Zero.into()).into()
            ))
        )
    }

    #[test]
    fn pares_test_with_whitespace() {
        assert_eq!(
            "S0 * x".parse(),
            Ok(Term::Mul(
                Term::S(Term::Zero.into()).into(),
                Term::Variable('x'.into()).into()
            ))
        )
    }

    #[test]
    fn parse_test_with_parenthesis() {
        assert_eq!(
            "SS(0*y+S0)".parse(),
            Ok(Term::S(
                Term::S(
                    Term::Add(
                        Term::Mul(Term::Zero.into(), Term::Variable('y'.into()).into()).into(),
                        Term::S(Term::Zero.into()).into()
                    )
                    .into()
                )
                .into()
            ))
        )
    }
}
