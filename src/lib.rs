mod disequality;
mod multiset;
mod polynomial;
mod proof;
pub mod proof_search;
mod substitution;
mod term;

use wasm_bindgen::prelude::wasm_bindgen;
use web_sys::console;

mod ui {

    use disequality::TermDisequality;
    use proof_search::search_proof;
    use term::Term;
    use wasm_bindgen::prelude::*;
    use web_sys::{console, Document, Element, HtmlElement, HtmlInputElement, InputEvent, Node};

    use crate::{disequality, proof_search, term};

    fn unchecked_document() -> Document {
        let window = web_sys::window().expect("window must exist");
        window.document().expect("document must exist")
    }

    fn unchecked_input_by_id(id: &str) -> HtmlInputElement {
        unchecked_element_by_id(id).unchecked_into()
    }

    fn unchecked_element_by_id(id: &str) -> web_sys::Element {
        unchecked_document()
            .get_element_by_id(id)
            .unwrap_or_else(|| {
                panic!(
                    "{}",
                    format_args!("element with id '{id}' must exist")
                        .as_str()
                        .unwrap()
                        .to_string()
                )
            })
    }

    fn left_input() -> HtmlInputElement {
        unchecked_input_by_id("left-term")
    }

    fn right_input() -> HtmlInputElement {
        unchecked_input_by_id("right-term")
    }

    fn left_term_display() -> HtmlElement {
        unchecked_element_by_id("left-term-display").unchecked_into()
    }

    fn right_term_display() -> HtmlElement {
        unchecked_element_by_id("right-term-display").unchecked_into()
    }

    pub(crate) fn setup() {
        let left_input = left_input();
        let left_input_on_change: Closure<dyn Fn(InputEvent)> = Closure::wrap(Box::new(oninput));
        left_input.set_oninput(Some(left_input_on_change.as_ref().unchecked_ref()));
        left_input_on_change.forget();

        let right_input = right_input();
        let right_input_on_change: Closure<dyn Fn(InputEvent)> = Closure::wrap(Box::new(oninput));
        right_input.set_oninput(Some(right_input_on_change.as_ref().unchecked_ref()));
        right_input_on_change.forget();

        display(left_input.value(), right_input.value());
    }

    fn oninput(_event: InputEvent) {
        let left_value = left_input().value();
        let right_value = right_input().value();

        display(left_value, right_value);
    }

    fn display(left_value: String, right_value: String) {
        console::log_4(
            &"as strings: ".into(),
            &left_value.as_str().into(),
            &"≠".into(),
            &right_value.as_str().into(),
        );

        let left: Result<Term, _> = left_value.parse();
        let right: Result<Term, _> = right_value.parse();

        let left = left.expect("left term must be valid");
        let right = right.expect("right term must be valid");

        console::log_4(
            &"as terms: ".into(),
            &serde_wasm_bindgen::to_value(&left).unwrap(),
            &"≠".into(),
            &serde_wasm_bindgen::to_value(&right).unwrap(),
        );

        let left_display: Element = left_term_display().into();
        left_display.set_text_content(None);
        left_display
            .append_child(&Node::from(left.clone()))
            .expect("append child must work");
        let right_display = right_term_display();
        right_display.set_text_content(None);
        right_display
            .append_child(&Node::from(right.clone()))
            .expect("append child must work");

        let disequality = TermDisequality::from_terms(left, right);

        match search_proof(&disequality) {
            Ok(proof) => {
                let proof = serde_wasm_bindgen::to_value(&proof).expect("serialize must succeed");
                console::log_2(&"found proof: ".into(), &proof)
            }
            Err(proof_attempt) => {
                let proof_attempt =
                    serde_wasm_bindgen::to_value(&proof_attempt).expect("serialize must succeed");
                console::log_2(&"proof attempt: ".into(), &proof_attempt)
            }
        }
    }

    impl From<Term> for Node {
        fn from(term: Term) -> Self {
            match term {
                Term::Variable(x) => unchecked_document()
                    .create_text_node(&format!(
                        "{}",
                        char::try_from(x).expect("must be a valid char value")
                    ))
                    .into(),
                Term::Zero => unchecked_document().create_text_node("0").into(),
                Term::S(inner) => {
                    let inner_node = Node::from(*inner);
                    let node = unchecked_document()
                        .create_element("span")
                        .expect("create element must work");

                    node.append_child(&unchecked_document().create_text_node("S").into())
                        .expect("append child must work");
                    node.append_child(&inner_node)
                        .expect("append child must work");

                    node.into()
                }
                Term::Add(left, right) => {
                    let left_node = Node::from(*left);
                    let right_node = Node::from(*right);
                    let node = unchecked_document()
                        .create_element("span")
                        .expect("create element must work");

                    node.append_child(&left_node)
                        .expect("append child must work");
                    node.append_child(&unchecked_document().create_text_node("+").into())
                        .expect("append child must work");
                    node.append_child(&right_node)
                        .expect("append child must work");

                    node.into()
                }
                Term::Mul(left, right) => {
                    let left_node = Node::from(*left);
                    let right_node = Node::from(*right);
                    let node = unchecked_document()
                        .create_element("span")
                        .expect("create element must work");

                    node.append_child(&left_node)
                        .expect("append child must work");
                    node.append_child(&unchecked_document().create_text_node("*").into())
                        .expect("append child must work");
                    node.append_child(&right_node)
                        .expect("append child must work");

                    node.into()
                }
            }
        }
    }
}

mod parse {
    use std::{iter::Peekable, str::FromStr};

    use crate::term::Term;

    impl FromStr for Term {
        type Err = Error;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let tokens = tokens(s)?;
            let term = TermParser::new(tokens).term()?;
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

    impl<I> TermParser<I>
    where
        I: Iterator<Item = TermToken>,
    {
        fn new(iter: I) -> Self {
            Self {
                tokens: iter.peekable(),
            }
        }

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
}

#[wasm_bindgen(start)]
pub fn main() {
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));

    ui::setup();

    console::log_1(&"main ended".into());
}
