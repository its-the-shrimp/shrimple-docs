use std::{collections::HashMap, convert::Infallible, error::Error, fmt::{Display, Formatter}, ops::{Deref, Index}, slice::from_raw_parts, str::from_utf8_unchecked};
use shrimple_parser::{
    any,
    pattern::{parse, parse_until_ex, parse_while, NotEscaped},
    tuple::map_first,
    FullParsingError,
    Parser,
    ParsingError,
    ParsingResult,
};

#[derive(Debug, Clone)]
pub enum JsonParserErrorKind {
    UnclosedStrLiteral,
    IntTooLong,
    NoValue,
    NoComma,
    ExtraComma,
    UnmatchedSqBracket,
    UnmatchedBracket,
    NoKey,
}

impl Display for JsonParserErrorKind {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::UnclosedStrLiteral => write!(f, "unclosed string literal found"),
            Self::IntTooLong => write!(f, "integer literal too long"),
            Self::NoValue => write!(f, "expected a JSON value"),
            Self::NoComma => write!(f, "expected a `,`"),
            Self::ExtraComma => write!(f, "extraneous comma after a key"),
            Self::UnmatchedSqBracket => write!(f, "extraneous `]`"),
            Self::UnmatchedBracket => write!(f, "extraneous `}}`"),
            Self::NoKey => write!(f, "expected a key"),
        }
    }
}

impl Error for JsonParserErrorKind {}

impl From<Infallible> for JsonParserErrorKind {
    fn from(value: Infallible) -> Self {
        match value {}
    }
}

/// A string that points after the end of any existing string in the program. 
const EMPTY_STR: &str = unsafe {
    from_utf8_unchecked(from_raw_parts(usize::MAX as *const u8, 0))
};

pub type JsonParserError<'src> = ParsingError<'src, JsonParserErrorKind>;
pub type FullJsonParserError<'src> = FullParsingError<'src, JsonParserErrorKind>;
pub type JsonParserResult<'src, T = ()> = ParsingResult<'src, T, JsonParserErrorKind>;

#[derive(Debug, Clone, Copy)]
pub enum Token<'src> {
    Null(*const u8),
    Bool(bool, *const u8),
    String(&'src str),
    Int(i64, *const u8),
    ArrayStart(*const u8),
    ArrayEnd(*const u8),
    ObjectStart(*const u8),
    Key(&'src str),
    ObjectEnd(*const u8),
}

#[derive(Debug, Clone)]
pub enum Value {
    Null,
    Bool(bool),
    String(Box<str>),
    Int(i64),
    Array(Box<[Value]>),
    Object(HashMap<Box<str>, Value>),
}

impl Index<&str> for Value {
    type Output = Self;

    fn index(&self, index: &str) -> &Self::Output {
        static NULL: Value = Value::Null;
        match self {
            Self::Object(obj) => obj.get(index).unwrap_or(&NULL),
            _ => &NULL,
        }
    }
}

impl Index<usize> for Value {
    type Output = Self;

    fn index(&self, index: usize) -> &Self::Output {
        static NULL: Value = Value::Null;
        match self {
            Self::Array(array) => array.get(index).unwrap_or(&NULL),
            _ => &NULL,
        }
    }
}

impl Index<&Self> for Value {
    type Output = Self;

    fn index(&self, index: &Self) -> &Self::Output {
        static NULL: Value = Value::Null;
        match (self, index) {
            (Self::Array(array), Self::Int(i)) => usize::try_from(*i).ok()
                .and_then(|i| array.get(i))
                .unwrap_or(&NULL),
            (Self::Object(obj), Self::String(s)) => obj.get(s).unwrap_or(&NULL),
            _ => &NULL,
        }
    }
}

impl Value {
    pub fn try_from_iter<'src, T>(iter: T) -> Result<Self, JsonParserError<'src>>
    where
        T: IntoIterator<Item = Result<Token<'src>, JsonParserError<'src>>>
    {
        Self::_try_from_iter(&mut iter.into_iter())
    }

    fn _try_from_iter<'src, T>(iter: &mut T) -> Result<Self, JsonParserError<'src>>
    where
        T: Iterator<Item = Result<Token<'src>, JsonParserError<'src>>>
    {
        Ok(match iter.next().ok_or(ParsingError::new(EMPTY_STR, JsonParserErrorKind::NoValue))?? {
            Token::Null(_) => Self::Null,
            Token::Bool(v, _) => Self::Bool(v),
            Token::String(v) => Self::String(v.into()),
            Token::Int(v, _) => Self::Int(v),
            Token::ArrayStart(_) => {
                let mut elements = vec![];
                loop {
                    match Self::_try_from_iter(iter) {
                        Ok(element) => elements.push(element),
                        Err(err) if err.is_recoverable() => break,
                        Err(err) => return Err(err),
                    }
                }
                Self::Array(elements.into_boxed_slice())
            }
            Token::ObjectStart(_) => {
                let mut pairs = HashMap::new();
                loop {
                    let k = match iter.next() {
                        Some(Err(err)) => return Err(err),
                        Some(Ok(Token::Key(k))) => k,
                        Some(Ok(Token::ObjectEnd(_))) => break,
                        _ => return Err(ParsingError::new(EMPTY_STR, JsonParserErrorKind::NoKey)),
                    };
                    match Self::_try_from_iter(iter) {
                        Ok(v) => pairs.insert(k.into(), v),
                        Err(err) => return Err(err.or_reason(JsonParserErrorKind::NoValue)),
                    };
                }
                Self::Object(pairs)
            }
            t @Token::Key(_) => return Err(ParsingError::new(t.src(), JsonParserErrorKind::NoValue)),
            t @(Token::ObjectEnd(_) | Token::ArrayEnd(_)) => {
                return Err(ParsingError::new_recoverable(t.src()))
            }
        })
    }

    pub fn keys(&self) -> impl Iterator<Item = &str> {
        match self {
            Self::Object(obj) => Some(obj.keys().map(Deref::deref)),
            _ => None,
        }.into_iter().flatten()
    }
}

impl<'src> Token<'src> {
    const fn comma_terminated(&self) -> bool {
        !matches!(self, Self::Key(_) | Self::ObjectStart(_) | Self::ArrayStart(_))
    }

    /// The returned string may have whatever but will always point to the start of the token.
    const fn src(&self) -> &'src str {
        match self {
            | Token::String(res)
            | Token::Key(res)
            => res,
            | Token::Null(ptr)
            | Token::Bool(_, ptr)
            | Token::Int(_, ptr)
            | Token::ArrayStart(ptr)
            | Token::ArrayEnd(ptr)
            | Token::ObjectStart(ptr)
            | Token::ObjectEnd(ptr)
            => unsafe {
                from_utf8_unchecked(from_raw_parts(*ptr, 0))
            }
        }
    }
}

fn parse_whitespace(src: &str) -> JsonParserResult<&str> {
    parse_while(char::is_ascii_whitespace)(src)
}

fn parse_null(src: &str) -> JsonParserResult<*const u8> {
    parse("null").map(str::as_ptr)(src)
}

fn parse_bool(src: &str) -> JsonParserResult<(bool, *const u8)> {
    parse("true").map(|s| (true, s.as_ptr()))
        .or(parse("false").map(|s| (false, s.as_ptr())))
        .parse(src)
}

/// Recoverable if no initial double quote found
/// The boolean in the output denotes whether the string literal was followed by a colon.
fn parse_string_or_key(src: &str) -> JsonParserResult<(&str, bool)> {
    parse('"')
        .then(parse_until_ex(NotEscaped('\\', '"'))
            .or_reason(JsonParserErrorKind::UnclosedStrLiteral))
        .skip(parse_whitespace)
        .and(parse(':').ok())
        .parse(src)
}

#[allow(clippy::cast_possible_truncation, /* reason = "the int literal will never exceed 20 chars" */)] 
fn parse_int(src: &str) -> JsonParserResult<(i64, *const u8)> {
    parse('-').maybe()
        .and(parse_while(char::is_ascii_digit)
            .filter_fatal(JsonParserErrorKind::IntTooLong,
                |&s| s.len() < 19 || s <= "9223372036854775807"))
        .filter(|(_, s)| !s.is_empty())
        .map(|(negation, s)| ((negation, s), negation.map_or(s.as_ptr(), str::as_ptr))) 
        .map(map_first(|(negation, s): (Option<&str>, &str)| s.bytes()
            .rev()
            .enumerate()
            .fold(0i64, |res, (i, byte)| res + (byte - b'0') as i64 * 10i64.pow(i as u32))
            * negation.map_or(1, |_| -1)))
        .parse(src)
}

/// The boolean in the input denotes whether there was a comma after the token.
fn parse_json_token(src: &str) -> JsonParserResult<(Token, bool)> {
    any! {
        parse_null.map(Token::Null),
        parse_bool.map(|(v, ptr)| Token::Bool(v, ptr)),
        parse_string_or_key.map(|(s, colon)| if colon {Token::Key(s)} else {Token::String(s)}),
        parse_int.map(|(v, ptr)| Token::Int(v, ptr)),
        parse('[').map(str::as_ptr).map(Token::ArrayStart),
        parse('{').map(str::as_ptr).map(Token::ObjectStart),
        parse(']').map(str::as_ptr).map(Token::ArrayEnd),
        parse('}').map(str::as_ptr).map(Token::ObjectEnd),
    }
    .or_reason_if_nonempty(JsonParserErrorKind::NoValue)
    .skip(parse_whitespace)
    .and(parse(',').skip(parse_whitespace).ok())
    .parse(src)
}

pub fn parse_json(src: &str) -> impl Iterator<Item = Result<Token, JsonParserError>> {
    #[derive(Clone, Copy, PartialEq, Eq)]
    enum State { Array, Object }

    parse_json_token
        .add_rest()
        .filter_fatal(JsonParserErrorKind::NoComma, |(token, comma, rest)| {
            *comma || rest.is_empty() || rest.starts_with([']', '}']) || !token.comma_terminated()
        })
        .filter_fatal(JsonParserErrorKind::ExtraComma, |(token, comma, _)| {
            token.comma_terminated() || !*comma
        })
        .map(|(token, _, rest)| (rest, token))
        .iter(src.trim_start())
        .scan(Vec::<State>::new(), |states, token| Some(match token {
            Err(err) => Err(err),

            Ok((_, token @Token::ArrayStart(_))) => {
                states.push(State::Array);
                Ok(token)
            }

            Ok((rest, token @Token::ArrayEnd(_))) => if states.last() == Some(&State::Array) {
                states.pop();
                Ok(token)
            } else {
                Err(ParsingError::new(rest, JsonParserErrorKind::UnmatchedSqBracket))
            }

            Ok((_, token @Token::ObjectStart(_))) => {
                states.push(State::Object);
                Ok(token)
            }

            Ok((rest, token @Token::ObjectEnd(_))) => if states.last() == Some(&State::Object) {
                states.pop();
                Ok(token)
            } else {
                Err(ParsingError::new(rest, JsonParserErrorKind::UnmatchedBracket))
            }

            Ok((_, token)) => Ok(token),
        }))
}

pub trait JsonParser<'src> where Self: Iterator<Item = Result<Token<'src>, JsonParserError<'src>>> {
    fn get_kv_pair(&mut self) -> Result<(&'src str, Token<'src>), JsonParserError<'src>> {
        let k = match self.next() {
            Some(Err(err)) => return Err(err),
            Some(Ok(Token::Key(k))) => k,
            Some(Ok(token)) => return Err(ParsingError::new(token.src(), JsonParserErrorKind::NoKey)),
            None => return Err(ParsingError::new(EMPTY_STR, JsonParserErrorKind::NoKey)),
        };
        let v = self.next().ok_or(ParsingError::new(EMPTY_STR, JsonParserErrorKind::NoValue))??;
        Ok((k, v))
    }
}

impl<'src, T: Iterator<Item = Result<Token<'src>, JsonParserError<'src>>>> JsonParser<'src> for T {}
