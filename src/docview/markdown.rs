use {
    crate::shared_str::SharedStr,
    shrimple_parser::{pattern::{parse_group, parse_until}, Input, Parser},
    std::{mem::take, ops::Not},
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

fn split_lines(chunk: Chunk) -> impl Iterator<Item = Chunk> {
    enum SplitLines<'borrow> {
        Split {
            rest: SharedStr<'borrow>,
            pending_nl: bool,
        },
        Forward(Option<Chunk<'borrow>>),
    }

    impl<'borrow> Iterator for SplitLines<'borrow> {
        type Item = Chunk<'borrow>;

        fn next(&mut self) -> Option<Self::Item> {
            let (rest, pending_nl) = match self {
                SplitLines::Split { rest, pending_nl } => (rest, pending_nl),
                SplitLines::Forward(chunk) => return chunk.take(),
            };

            if take(pending_nl) {
                return Some(Chunk::Newline);
            }
            let Some(index) = rest.find('\n') else {
                return rest.is_empty().not().then(|| Chunk::Text(take(rest)));
            };
            if index == 0 {
                *rest = take(rest).after(1);
                return Some(Chunk::Newline);
            }
            let (line, new_rest) = take(rest).split_at(index);
            *rest = new_rest.after(1);
            *pending_nl = true;
            Some(Chunk::Text(line))
        }
    }

    let Chunk::Text(rest) = chunk else {
        return SplitLines::Forward(Some(chunk));
    };
    SplitLines::Split { rest, pending_nl: false }
}

#[derive(Debug)]
pub struct Markdown<'borrow> {
    pub chunks: Box<[Chunk<'borrow>]>,
}

impl<'borrow> Markdown<'borrow> {
    pub fn parse(input: impl Into<SharedStr<'borrow>>) -> Self {
        let input = input.into();

        let mut chunks: Vec<_> = parse_group('[', ']')
            .and(parse_group('(', ')').maybe())
            .map(|(text, dst)| Chunk::Link { text, dst })
            .or_nonempty(parse_until('[').map(Chunk::Text).narrow_reason(()))
            .iter(input.clone())
            .filter_map(Result::ok)
            .flat_map(split_lines)
            .collect();
        if let Some(last) = chunks.pop().filter(|c| !matches!(c, Chunk::Newline)) {
            chunks.push(last);
        }
        Self { chunks: chunks.into() }
    }
}

#[test]
fn markdown_parse() {
    assert_eq!(
        &*Markdown::parse("a\nb\n[abc](def)\n[abc]\n").chunks,
        [
            Chunk::Text("a".into()),
            Chunk::Newline,
            Chunk::Text("b".into()),
            Chunk::Newline,
            Chunk::Link { text: "abc".into(), dst: Some("def".into()) },
            Chunk::Newline,
            Chunk::Link { text: "abc".into(), dst: None },
        ],
    );
}
