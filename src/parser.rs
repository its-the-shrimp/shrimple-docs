use std::{convert::Infallible, fmt::{Debug, Display, Formatter}, iter::from_fn, mem::replace, path::Path};
use crate::utils::{is_in, Result};
use shrimple_parser::{any, from_tuple, parse_char, parse_exact, parse_until, parse_until_ex, parse_until_exact, parse_while, tuple::second, utils::eq, Parser, ParsingError, ParsingResult};

#[derive(Debug, Clone, Copy)]
pub enum DocParserErrorKind {
    UnclosedClosingTag,
    Any,
    UnclosedStrLiteral,
    NoAttrName,
    UnclosedComment,
}

impl Display for DocParserErrorKind {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::UnclosedClosingTag => write!(f, "expected `/>` or `>`"),
            Self::Any => unreachable!(),
            Self::UnclosedStrLiteral => write!(f, "expected a string terminated by `\"`"),
            Self::NoAttrName => write!(f, "expected an attribute name"),
            Self::UnclosedComment => write!(f, "expected a comment terminated by `-->`"),
        }
    }
}

impl std::error::Error for DocParserErrorKind {}

impl From<Infallible> for DocParserErrorKind {
    fn from(value: Infallible) -> Self {
        match value {}
    }
}

pub type DocParserError<'src> = ParsingError<'src, DocParserErrorKind>;
pub type DocParserResult<'src, T> = ParsingResult<'src, T, DocParserErrorKind>;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Attr<'src> {
    pub name: &'src str,
    pub value: Option<&'src str>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
// Safety invariant: `!self.0.is_empty()`
pub struct OpeningTagEnd<'src>(&'src str);

impl OpeningTagEnd<'_> {
    pub fn is_self_closing(&self) -> bool {
        self.0.ends_with("/>")
    }
}

#[derive(Debug)]
pub enum HtmlFragment<'src> {
    /// <tagname
    OpeningTagStart(&'src str),
    /// `key=value` or key
    Attr(Attr<'src>),
    /// `/>` or `>`
    OpeningTagEnd(OpeningTagEnd<'src>),
    /// </tagname>
    ClosingTag(&'src str),
    /// any other text
    Text(&'src str),
}

/// Never returns an error
fn parse_word(input: &str) -> DocParserResult<&str> {
    parse_until(is_in([' ', '\n', '\t', '/', '>'])).expect(DocParserErrorKind::Any)(input)
}

fn parse_whitespace(input: &str) -> ParsingResult<&str> {
    parse_while(char::is_ascii_whitespace)(input)
}

/// Recoverable if no initial double quote found
fn parse_string_literal(input: &str) -> DocParserResult<&str> {
    parse_char('"')
        .then(parse_until_ex(eq('"')).expect(DocParserErrorKind::UnclosedStrLiteral))
        .parse(input)
}

fn parse_attr_value(input: &str) -> DocParserResult<&str> {
    parse_string_literal.or(parse_word)(input)
}

fn parse_attr(input: &str) -> DocParserResult<Attr> {
    parse_whitespace
        .then(parse_until(is_in([' ', '\n', '\t', '/', '>', '=']))
              .expect(DocParserErrorKind::NoAttrName))
        .and(parse_char('=').then(parse_attr_value).maybe())
        .map(from_tuple!(Attr { name, value }))
        .parse(input)
}

/// The output is the element name, without the `<`.
/// Recoverable if no initial `<` is found.
fn parse_opening_tag_start(input: &str) -> DocParserResult<&str> {
    parse_char('<').then(parse_word)(input)
}

/// Any returned error is recoverable.
fn parse_opening_tag_end(input: &str) -> ParsingResult<OpeningTagEnd> {
    parse_whitespace
        .maybe_skip(parse_char('/'))
        .skip(parse_char('>'))
        .get_span()
        .map(second)
        .map(OpeningTagEnd)
        .parse(input)
}

/// Recoverable if no initial `<!--` is found.
fn parse_comment(input: &str) -> DocParserResult<&str> {
    parse_exact("<!--").then(parse_until_exact("-->").expect(DocParserErrorKind::UnclosedComment))(input)
}

fn parse_closing_tag(input: &str) -> DocParserResult<&str> {
    parse_exact("</")
        .then(parse_word)
        .maybe_skip(parse_whitespace)
        .skip(parse_char('>').expect(DocParserErrorKind::UnclosedClosingTag))
        .parse(input)
}

fn parse_html_fragment_elsewhere(input: &str) -> DocParserResult<HtmlFragment> {
    parse_comment.repeat()
        .then(any! {
            parse_closing_tag.map(HtmlFragment::ClosingTag),
            parse_opening_tag_start.map(HtmlFragment::OpeningTagStart),
            parse_until(eq('<'))
                .filter(|s| !s.is_empty())
                .map(HtmlFragment::Text)
                .narrow_reason(DocParserErrorKind::Any),
        })
        .parse(input)
}

fn parse_html_fragment_in_opening_tag(input: &str) -> DocParserResult<HtmlFragment> {
    parse_opening_tag_end.map(HtmlFragment::OpeningTagEnd)
        .or(parse_attr.map(HtmlFragment::Attr))
        .parse(input)
}

fn with_state<'input, T, Reason, S, F>(mut state: S, mut f: F) -> impl Parser<'input, T, Reason>
where
    F: FnMut(&mut S, &'input str) -> ParsingResult<'input, T, Reason>,
{
    move |input| f(&mut state, input)
}

fn parse_html(input: &str) -> impl Iterator<Item = Result<HtmlFragment, DocParserError>> {
    with_state(false, |parsing_attrs, input| if *parsing_attrs {
        parse_html_fragment_in_opening_tag(input)
            .inspect(|(_, out)| *parsing_attrs = matches!(out, HtmlFragment::Attr(_)))
    } else {
        parse_html_fragment_elsewhere(input)
            .inspect(|(_, out)| *parsing_attrs = matches!(out, HtmlFragment::OpeningTagStart(_)))
    })
    .iter(input)
}

/// Returns an iterator over files to parse for concrete defintions
pub fn parse_index(src: &str) -> impl Iterator<Item = Result<&Path, DocParserError>> {
    let mut frags = parse_html(src);
    let mut in_main = false;
    from_fn(move || {
        while !in_main {
            match frags.next()? {
                Err(err) => return Some(Err(err)),
                Ok(HtmlFragment::OpeningTagStart("main")) => in_main = true,
                _ => {}
            }
        }
        loop {
            match frags.next()? {
                Err(err) => return Some(Err(err)),
                Ok(HtmlFragment::OpeningTagStart("a")) => match frags.next()? {
                    Err(err) => return Some(Err(err)),
                    Ok(HtmlFragment::Attr(Attr { value: Some(v), .. })) => {
                        return Some(Ok(Path::new(v)))
                    }
                    _ => {}
                }
                _ => {}
            }
        }
    })
}
