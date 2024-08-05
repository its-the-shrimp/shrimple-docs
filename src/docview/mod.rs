mod item_printer;

use {
    crate::{
        docs::Docs,
        errfmt,
        utils::{
            cmp, BoolExt, IntExt, IteratorExt, OptionExt, Result, UIntExt, WriteExt, BOLD, NL,
            NOSTYLE, OK,
        },
        docview::item_printer::print_item,
    },
    anyhow::{bail, Context},
    crossterm::{
        cursor::{MoveDown, MoveToColumn, MoveUp},
        event::{KeyCode, KeyEvent, KeyModifiers},
        QueueableCommand,
    },
    rustdoc_types::{Enum, Function, Id, Item, ItemEnum, ProcMacro, Struct, StructKind, Trait, Union},
    std::{borrow::Cow, fmt::Write as _, mem::transmute, num::Saturating},
};

enum SectionKind {
    Text {
        text: Box<str>,
        /// The length of the longest line of [`SectionKind::Text::text`]
        max_line_len: u32,
        /// The number of lines in [`SectionKind::Text::text`], which might be collapsed
        n_lines: u32,
    },
    Folder {
        subsections: Box<[Section]>,
    },
}

struct Section {
    name: Cow<'static, str>,
    expanded: bool,
    kind: SectionKind,
}

impl Section {
    fn new(name: impl Into<Cow<'static, str>>, text: impl Into<Box<str>>) -> Self {
        Self::_new(name.into(), text.into())
    }

    fn _new(name: Cow<'static, str>, text: Box<str>) -> Self {
        let mut n_lines = Saturating(0u32);
        let max_line_len = text
            .lines()
            .inspect(|_| n_lines += 1)
            .map(str::len)
            .max()
            .map_or(0, |x| x.try_into().unwrap_or(u32::MAX));
        Self {
            name,
            expanded: false,
            kind: SectionKind::Text { text, max_line_len, n_lines: n_lines.0 },
        }
    }

    fn new_folder(
        name: impl Into<Cow<'static, str>>,
        subsections: impl Into<Box<[Self]>>,
    ) -> Self {
        Self {
            name: name.into(),
            expanded: false,
            kind: SectionKind::Folder { subsections: subsections.into() },
        }
    }

    fn count_lines(&self) -> usize {
        if !self.expanded {
            return 1;
        }
        match &self.kind {
            SectionKind::Text { n_lines, .. } => n_lines.into_usize(),
            SectionKind::Folder { subsections, .. } => subsections
                .iter()
                .map(|s| if s.expanded { s.count_lines().saturating_add(1) } else { 1 })
                .fold(0, usize::saturating_add),
        }.saturating_add(1)
    }

    fn _max_line_len(&self, indent: usize) -> usize {
        if !self.expanded {
            return 0;
        }
        match &self.kind {
            SectionKind::Text { max_line_len, .. } => max_line_len
                .into_usize()
                .saturating_add(indent.wrapping_mul(2)),
            SectionKind::Folder { subsections, .. } => subsections
                .iter()
                .map(|s| if s.expanded { s._max_line_len(indent.wrapping_add(1)) } else { 0 })
                .max()
                .unwrap_or(0),
        }
    }

    fn max_line_len(&self) -> usize {
        self._max_line_len(1)
    }

    fn print(&self, out: &mut impl std::io::Write, indent: usize) -> Result {
        for _ in 0 .. indent {
            write!(out, "  ")?;
        }
        write!(out, "{} {BOLD}{}{NOSTYLE}{NL}", self.expanded.pick('-', '+'), self.name)?;
        if !self.expanded {
            return OK;
        }

        match &self.kind {
            SectionKind::Text { text, .. } => for line in text.lines() {
                for _ in 0 .. indent {
                    write!(out, "  ")?;
                }
                write!(out, "  {line}{NL}")?;
            }
            SectionKind::Folder { subsections } => for section in subsections {
                section.print(out, indent.wrapping_add(1))?;
            }
        }
        OK
    }

    #[expect(clippy::unwrap_used, reason = "This will only panic if the closure does")]
    fn fold_subsection<'this, T: 'static, E>(
        &'this mut self,
        init: T,
        mut f: impl Copy + FnMut(T, &'this mut Self) -> Result<T, E>
    ) -> Result<T, E> {
        let expanded = self.expanded;
        let init = f(init, unsafe { transmute::<&mut Self, &'static mut Self>(&mut *self) })?;
        if !expanded {
            return Ok(init);
        }
        let SectionKind::Folder { subsections } = &mut self.kind else {
            return Ok(init);
        };
        let mut state = Some(init);
        for subsection in subsections {
            state = Some(subsection.fold_subsection(state.take().unwrap(), f)?);
        }
        Ok(state.unwrap())
    }
}

pub struct DocView {
    sections: Vec<Section>,
    cursor_x: i32,
    cursor_y: i32,
}

/// needed for fields & variants
fn gather_items_docs<'item>(
    ids: impl IntoIterator<Item = &'item Id>,
    docs: &Docs,
) -> Result<Vec<Section>> {
    let mut err = Ok(());
    let mut res = vec![];
    let index = docs.index();
    add_items_docs(
        ids.into_iter().filter_map(|id| index.get(&*id.0).inspect_none(|| err = Err(id))),
        &mut res,
        docs
    )?;
    match err {
        Ok(_) => Ok(res),
        Err(id) => bail!("failed to find item {id:?}"),
    }
}

fn add_items_docs<'item>(
    items: impl IntoIterator<Item = &'item Item>,
    dst: &mut impl Extend<Section>,
    docs: &Docs,
) -> Result {
    for item in items {
        let mut content = String::new();
        print_item(item, docs, &mut content)?;
        write!(content, "\n{}", item.docs.as_deref().unwrap_or_default())?;
        let mut name = content.lines().next().unwrap_or_default().to_owned();
        content.drain(..name.len());
        if name.bytes().last() == Some(b'\n') {
            name.pop();
        }
        dst.extend([Section::new(name, content)]);
    }
    OK
}

impl DocView {
    pub fn new(item: &Item, docs: &Docs) -> Result<Self> {
        let mut sections = vec![];
        if !matches!(item.inner, ItemEnum::Module(_) | ItemEnum::ProcMacro(_)) {
            let mut decl_content = String::new();
            print_item(item, docs, &mut decl_content)?;
            sections.push(Section::new("Definition", decl_content));
        }

        let index = docs.index();
        match &item.inner {
            ItemEnum::Union(Union { fields, .. }) => {
                sections.push(Section::new_folder("Fields", gather_items_docs(fields, docs)?));
            }

            ItemEnum::Struct(Struct { kind: StructKind::Plain { fields, .. }, .. }) => {
                sections.push(Section::new_folder("Fields", gather_items_docs(fields, docs)?));
            }

            ItemEnum::Enum(Enum { variants, .. }) => {
                sections.push(Section::new_folder("Variants", gather_items_docs(variants, docs)?));
            }

            ItemEnum::Trait(Trait { items: ids, .. }) => {
                #[expect(clippy::trivially_copy_pass_by_ref, reason = "easier to use")]
                const fn is_provided(item: &&Item) -> bool {
                    matches!(&item.inner, ItemEnum::Function(Function { has_body: true, .. })
                                        | ItemEnum::AssocType { default: Some(_), .. }
                                        | ItemEnum::AssocConst { default: Some(_), .. })
                }

                let mut items: Vec<&Item> = ids.iter()
                    .map(|id| index.get(&*id.0).with_context(errfmt!("find item {:?}", id)))
                    .try_collect()?;
                items.sort_unstable_by(|a, b| 
                    cmp(&is_provided(b), &is_provided(b))
                        .then_with(|| cmp(&b.name, &a.name)));

                let (required, provided) = items.split_at(items.partition_point(is_provided));
                let mut item_docs = Vec::<Section>::new();

                if !required.is_empty() {
                    item_docs.push(Section::new("Required:", ""));
                    add_items_docs(required.iter().copied(), &mut item_docs, docs)?;
                }

                if !provided.is_empty() {
                    item_docs.push(Section::new("Provided:", ""));
                    add_items_docs(provided.iter().copied(), &mut item_docs, docs)?;
                }

                sections.push(Section::new_folder("Associated items", item_docs));
            }

            ItemEnum::ProcMacro(ProcMacro { helpers, .. }) => if !helpers.is_empty() {
                let mut content = String::new();
                for attr_name in helpers {
                    writeln!(content, "#[{attr_name}]")?;
                }
                sections.push(Section::new("Helper attributes", content));
            }

            | ItemEnum::Struct(Struct { kind: StructKind::Unit | StructKind::Tuple(_), .. })
            | ItemEnum::StructField(_)
            | ItemEnum::Variant(_)
            | ItemEnum::Function(_)
            | ItemEnum::TraitAlias(_)
            | ItemEnum::Impl(_)
            | ItemEnum::TypeAlias(_) // TODO: show the resulting type with the generics substituted
            | ItemEnum::OpaqueTy(_)
            | ItemEnum::Constant { .. }
            | ItemEnum::Static(_)
            | ItemEnum::ForeignType
            | ItemEnum::Macro(_)
            | ItemEnum::Primitive(_)
            | ItemEnum::AssocConst { .. }
            | ItemEnum::AssocType { .. } 
            | ItemEnum::Module(_)
            | ItemEnum::Import(_)
            | ItemEnum::ExternCrate { .. } => {},
        }

        let mut desc = Section::new("Description", item.docs.clone().unwrap_or_default());
        if sections.is_empty() {
            desc.expanded = true;
        }
        sections.push(desc);

        Ok(Self { sections, cursor_x: 0, cursor_y: 0 })
    }

    pub const fn cursor_x(&self) -> i32 {
        self.cursor_x
    }

    pub fn set_cursor_x(&mut self, cursor_x: i32) {
        self.cursor_x = cursor_x;
    }

    pub const fn cursor_y(&self) -> i32 {
        self.cursor_y
    }

    pub fn set_cursor_y(&mut self, cursor_y: i32) {
        self.cursor_y = cursor_y;
    }

    /// The returned integer is the offset from the header line of the section, if it's 0, the
    /// cursor is on the header line
    ///
    /// If the closures returnes `false`, 
    fn _innermost_hovered_section_mut(
        &mut self,
        mut must_break: impl Copy + FnMut(&mut Section) -> bool,
    ) -> Option<(&mut Section, usize)> {
        let Ok(rem) = usize::try_from(self.cursor_y) else {
            return None;
        };

        self.sections.iter_mut()
            .try_fold(rem, |rem, section|
                section.fold_subsection(rem, move |rem, section| 
                    rem.checked_sub(1)
                        .and_then(|rem| {
                            if must_break(section) {
                                return None;
                            }
                            if !section.expanded {
                                return Some(rem);
                            }
                            let SectionKind::Text { n_lines, .. } = section.kind else {
                                return Some(rem);
                            };
                            rem.checked_sub(n_lines.into_usize())
                        })
                        .ok_or((section, rem))))
            .err()
    }

    fn innermost_hovered_section_mut(&mut self) -> Option<(&mut Section, usize)> {
        self._innermost_hovered_section_mut(|_| false)
    }

    /// Returns a boolean indicating whether the view needs needs to be re-rendered
    pub fn process_key_event(&mut self, event: &KeyEvent) -> bool {
        match event.code {
            KeyCode::Down => self.cursor_y = match event.modifiers {
                KeyModifiers::NONE => self.cursor_y.saturating_add(1),
                KeyModifiers::SHIFT => self.sections
                    .iter()
                    .map(Section::count_lines)
                    .fold(0, usize::saturating_add)
                    .try_into()
                    .unwrap_or(i32::MAX),
                _ => return false,
            },

            KeyCode::Up => self.cursor_y = match event.modifiers {
                KeyModifiers::NONE => self.cursor_y.saturating_sub(1),
                KeyModifiers::SHIFT => 0,
                _ => return false,
            },

            KeyCode::Right => self.cursor_x = match event.modifiers {
                KeyModifiers::NONE => self.cursor_x.saturating_add(1),
                KeyModifiers::SHIFT => match self.sections
                    .iter()
                    .map(Section::max_line_len)
                    .max()
                {
                    Some(max_line_len) => max_line_len.try_into().unwrap_or(i32::MAX),
                    _ => return false,
                }
                _ => return false,
            },

            KeyCode::Left => self.cursor_x = match event.modifiers {
                KeyModifiers::NONE => self.cursor_x.saturating_sub(1),
                KeyModifiers::SHIFT => 0,
                _ => return false,
            },

            KeyCode::Enter => if let Some((section, 0)) = self.innermost_hovered_section_mut() {
                section.expanded = !section.expanded;
            } else {
                return false;
            }

            _ => return false,
        }
        true
    }

    pub fn print(
        &self,
        out: &mut impl std::io::Write,
        _width: u16,
        height: u16,
    ) -> Result {
        let small_margin_y = height.div_ceil(2);
        let margin_y = small_margin_y.into_usize();
        let cursor_y = self.cursor_y.into_isize().saturating_add(1);
        for _ in cursor_y.wrapping_sub_unsigned(margin_y) .. 0 {
            write!(out, "{NL}")?;
        }

        let mut out = out
            .first_lines(margin_y.saturating_add_signed(cursor_y))
            .skip_lines(cursor_y.saturating_sub_unsigned(margin_y).try_into().unwrap_or(0));
        self.sections.iter().try_for_each(|s| s.print(&mut out, 0))?;

        out.inner.inner
            .queue(MoveDown(u16::MAX - 1))?
            .queue(MoveUp(small_margin_y))?
            .queue(MoveToColumn(self.cursor_x.try_into().unwrap_or(u16::MAX)))?;
        Ok(())
    }
}
