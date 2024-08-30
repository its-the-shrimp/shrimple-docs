use {
    crate::shared_str::SharedStr,
    shrimple_parser::{any, pattern::{parse, parse_group, parse_until, parse_while, AnyChar}, Parser},
    std::{borrow::Borrow, collections::HashMap, hash::Hash},
};

#[derive(Debug, PartialEq, Eq)]
pub enum Chunk<'borrow> {
    Newline,
    Text(SharedStr<'borrow>),
    Link {
        text: SharedStr<'borrow>,
        dst: Option<SharedStr<'borrow>>,
    },
}

#[derive(Debug)]
enum RawChunk<'borrow> {
    Newline,
    Text(SharedStr<'borrow>),
    Link {
        text: SharedStr<'borrow>,
        dst: Option<SharedStr<'borrow>>,
    },
    LinkDef {
        from: SharedStr<'borrow>,
        to: SharedStr<'borrow>,
    },
}

impl Chunk<'_> {
    pub fn len(&self) -> Option<usize> {
        match self {
            Self::Newline => None,
            Self::Text(text) | Self::Link { text, .. } => Some(text.len()),
        }
    }
}

#[derive(Debug)]
pub struct Markdown<'borrow> {
    pub chunks: Box<[Chunk<'borrow>]>,
    pub max_line_len: u32,
    pub n_lines: u32,
}

impl<'borrow> Markdown<'borrow> {
    #[expect(clippy::cast_possible_truncation, reason = "can't wrap in remotely reasonable input")]
    pub fn parse(
        input: impl Into<SharedStr<'borrow>>,
        links: &HashMap<impl Borrow<str> + Eq + Hash, impl Clone + Into<SharedStr<'borrow>>>,
    ) -> Self {
        let input: SharedStr = input.into();
        let mut aliases = HashMap::new();

        let mut chunks: Vec<_> = input.lines()
            .flat_map(|line| parse_group('[', ']')
                .and(any! {
                    parse(':')
                        .skip(parse_while(' '))
                        .then(parse_while(AnyChar))
                        .map(|x| Some((x, true)))
                        .narrow_reason(()),
                    parse_group('(', ')').and_value(false).maybe(),
                })
                .map(|(text, dst)| match dst {
                    Some((dst, true)) => RawChunk::LinkDef { from: text, to: dst },
                    Some((dst, false)) => RawChunk::Link { text, dst: Some(dst) },
                    None => RawChunk::Link { text, dst: None },
                })
                .or_nonempty(parse_until('[').map(RawChunk::Text).narrow_reason(()))
                .iter(line)
                .filter_map(Result::ok)
                .chain([RawChunk::Newline]))
            .filter_map(|chunk| match chunk {
                RawChunk::Newline => Some(Chunk::Newline),
                RawChunk::Text(text) => Some(Chunk::Text(text)),
                RawChunk::Link { text, dst } => Some(Chunk::Link { text, dst }),
                RawChunk::LinkDef { from, to } => {
                    aliases.insert(from.clone(), to.clone());
                    None
                }
            })
            .collect();

        while chunks.last() == Some(&Chunk::Newline) {
            chunks.pop();
        }

        let mut n_lines = 1u32;
        let mut max_line_len = 0u32;
        for chunk in &mut chunks {
            let Some(chunk_len) = chunk.len() else {
                n_lines = n_lines.wrapping_add(1);
                max_line_len = 0;
                continue;
            };

            max_line_len = max_line_len.wrapping_add(chunk_len as u32);

            if let Chunk::Link { text, dst } = chunk {
                let k = dst.as_ref().or_else(|| aliases.get(text)).unwrap_or(text);
                *dst = links.get(k).map(|x| x.clone().into());
            }
        }

        Self { chunks: chunks.into(), max_line_len, n_lines }
    }

    pub fn chunk_at_pos(&self, mut x: usize, mut y: usize) -> Option<&Chunk<'borrow>> {
        for chunk in &self.chunks {
            if let Some(y_minus_1) = y.checked_sub(1) {
                if *chunk == Chunk::Newline {
                    y = y_minus_1;
                }
                continue;
            }

            let Some(new_x) = x.checked_sub(chunk.len()?) else {
                return Some(chunk);  
            };

            x = new_x;
        }

        None
    }
}

#[test]
fn markdown_parse() {
    assert_eq!(
        &*Markdown::parse("a\nb\n[abc](def)\n[abc]\n[abc]: def", &HashMap::from_iter([
            ("def", "hey"),
        ])).chunks,
        [
            Chunk::Text("a".into()),
            Chunk::Newline,
            Chunk::Text("b".into()),
            Chunk::Newline,
            Chunk::Link { text: "abc".into(), dst: Some("hey".into()) },
            Chunk::Newline,
            Chunk::Link { text: "abc".into(), dst: Some("hey".into()) },
        ],
    );
}
