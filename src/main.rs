mod utils;
mod item_printer;
mod docs;
mod item_visitor;

use {
    anyhow::{anyhow, bail, ensure, Context},
    crossterm::{
        cursor::{MoveLeft, MoveToColumn, MoveToNextLine, MoveToPreviousLine, MoveToRow},
        event::{Event, KeyCode, KeyEvent, KeyModifiers},
        terminal::{
            disable_raw_mode,
            enable_raw_mode,
            Clear,
            ClearType::FromCursorDown,
            DisableLineWrap,
            EnableLineWrap,
            ScrollUp,
        },
        ExecutableCommand,
        QueueableCommand,
    },
    docs::{Docs, SearchResult},
    rustdoc_types::Id,
    std::{
        io::{stdin, stdout, BufRead, BufReader, Write},
        iter::once,
        process::{Command, Stdio},
        sync::Arc,
    },
    crate::{
        item_printer::print_item,
        utils::{
            str_char_count,
            EmptyError,
            Exit,
            StringView,
            INVERT,
            OK,
            Result,
            NL,
            NULL_EVENT,
            NOSTYLE,
            BOLD,
        },
    },
};

/// if `None` is returned, the app is supposed to exit.
fn next_key_event() -> Result<KeyEvent> {
    loop {
        let Event::Key(event) = crossterm::event::read()? else {continue};
        return match event.code {
            KeyCode::Char('c') if event.modifiers == KeyModifiers::CONTROL => bail!(Exit),
            _ => Ok(event),
        }
    }
}

struct Ctx {
    docs: Docs,
    window_height: u16,
}

impl Ctx {
    const DEFAULT_WINHEIGHT: u16 = 15;

    async fn new(
        r#in: &mut (impl BufRead + Send),
        out: &mut (impl Write + Send),
    ) -> Result<Self> {
        let toolchains = Command::new("rustup")
            .args(["toolchain", "list"])
            .stderr(Stdio::inherit())
            .stdout(Stdio::piped())
            .output()?
            .stdout;
        let toolchains = String::from_utf8(toolchains).context("`rustup` gave non-UTF8 output")?;

        let toolchain = if let Some(x) = toolchains.lines().rfind(|x| x.starts_with("nightly")) {
            x.split_once(' ').map_or(x, |x| x.0)
        } else {
            writeln!(out, "It appears that you don't have a nightly Rust toolchain installed.")?;
            writeln!(out, "A nightly toolchain is essential for this tool to function.")?;
            writeln!(out, "Do you wish to install it?")?;
            write  !(out, "Answer (y/n, anything else will abort the program): ")?;
            out.flush()?;

            let mut resp = [0u8; 2];
            r#in.read_exact(&mut resp)?;
            match &resp {
                b"y\n" => {}
                b"n\n" => bail!(Exit),
                _ => bail!(EmptyError),
            };

            let status = Command::new("rustup")
                .args(["toolchain", "install", "nightly", "--component", "rust-docs-json"])
                .stderr(Stdio::inherit())
                .stderr(Stdio::inherit())
                .status()
                .context("failed to launch `rustup toolchain install nightly`")?;
            ensure!(status.success(), "`rustup` failed");

            "nightly"
        };

        Ok(Self{
            docs: Docs::new(&Arc::from(toolchain), r#in, out).await?,
            window_height: Self::DEFAULT_WINHEIGHT,
        })
    }
}

struct SearchMode<'ctx> {
    selected: Option<usize>,
    shift: usize,
    query: String,
    results: Vec<SearchResult<'ctx>>,
}

impl<'ctx> SearchMode<'ctx> {
    const INPUT_PREFIX: &'static str = "search \u{2192} ";
    const INPUT_PREFIX_LEN: usize = str_char_count(Self::INPUT_PREFIX);

    fn new(query: String, ctx: &'ctx Ctx) -> Self {
        Self {
            query,
            selected: None,
            shift: 0,
            results: Vec::with_capacity(ctx.docs.index.len()),
        }
    }

    fn print_results(&self, ctx: &'ctx Ctx, out: &mut impl Write) -> Result {
        let n = ctx.window_height - 4;

        for (i, result) in self.results.iter().enumerate().skip(self.shift).take(n as _) {
            out.queue(MoveToColumn(0))?.queue(MoveToNextLine(1))?;
            let invert = if self.selected == Some(i) {INVERT} else {""};
            write!(out, "{invert}{i}: {}{NOSTYLE}", result.id.0)?;
        }

        let n_advanced = self.results.len().try_into().unwrap_or(n).min(n);
        if n_advanced > 0 {
            out.queue(MoveToPreviousLine(n_advanced))?;
        }
        out.queue(MoveToColumn((Self::INPUT_PREFIX_LEN + self.query.len()).try_into()?))?;
        OK
    }

    fn init(&self, ctx: &'ctx Ctx, out: &mut impl Write) -> Result {
        write!(out, "{BOLD}<Up/Down>{NOSTYLE} - go 1 line up/down in the results{NL}")?;
        write!(out, "{BOLD}<Ctrl-Up/Down>{NOSTYLE} - go to the top/bottom of the results{NL}")?;
        write!(out, "{}{}", Self::INPUT_PREFIX, self.query)?;
        self.print_results(ctx, out)
    }

    fn process_key_event(&mut self, event: &KeyEvent, ctx: &'ctx Ctx, out: &mut impl Write)
        -> Result<Option<Mode<'ctx>>>
    {
        let n_results = ctx.window_height as usize - 4;

        let changed = match event.code {
            KeyCode::Up => {
                if event.modifiers == KeyModifiers::SHIFT {
                    self.selected = Some(0);
                    self.shift = 0;
                } else {
                    self.selected = self.selected.and_then(|x| x.checked_sub(1));
                    if self.selected.is_some_and(|s| (s - self.shift) < n_results / 2) {
                        self.shift = self.shift.saturating_sub(1);
                    }
                }
                false
            }

            KeyCode::Down => {
                if event.modifiers == KeyModifiers::SHIFT {
                    self.selected = Some(self.results.len() - 1);
                    self.shift = self.results.len() - n_results;
                } else {
                    let new = self.selected.map_or(0, |x| (x + 1) % self.results.len());
                    if new == 0 {
                        self.shift = 0;
                    } else if new - self.shift > n_results / 2 {
                        self.shift = (self.shift + 1).min(self.results.len() - n_results);
                    }
                    self.selected = Some(new);
                }
                false
            }

            KeyCode::Enter => if let Some(selected) = self.selected {
                out.queue(MoveToPreviousLine(3))?.queue(Clear(FromCursorDown))?;
                let docview = DocViewMode::new(self.results[selected].id, ctx)?;
                return Ok(Some(Mode::DocView(docview)))
            } else {
                false
            }

            KeyCode::Char(ch) if ch != '\n' => {
                self.query.push(ch);
                write!(out, "{ch}")?;
                true
            }

            KeyCode::Backspace => if self.query.pop().is_some() {
                out.queue(MoveLeft(1))?;
                write!(out, " ")?;
                true
            } else {
                false
            }

            _ => return Ok(None),
        };
        out.queue(Clear(FromCursorDown))?;

        if changed {
            ctx.docs.search(&self.query, &mut self.results);
        }
        self.print_results(ctx, out)?;
        Ok(None)
    }
}

struct DocViewMode {
    id: Id,
    view: Option<StringView>,
}

impl DocViewMode {
    const INPUT_PREFIX: &'static str = "view \u{2192} ";
    const INPUT_PREFIX_LEN: usize = str_char_count(Self::INPUT_PREFIX);

    fn get_view(id: &Id, ctx: &Ctx) -> Result<Option<StringView>> {
        let Some(item) = ctx.docs.index.get(id) else {
            return Ok(None);
        };
        let mut content = StringView::new(String::new(), -1, -1);
        print_item(item, &ctx.docs, &mut content)?;
        Ok(Some(content))
    }

    fn new(id: &Id, ctx: &Ctx) -> Result<Self> {
        Ok(Self { view: Self::get_view(id, ctx)?, id: id.to_owned() })
    }

    fn init(&self, ctx: &Ctx, out: &mut impl Write) -> Result {
        write!(out, "{}{}{NL}", Self::INPUT_PREFIX, self.id.0)?;
        if let Some(view) = &self.view {
            view.print(out)?;
        } else {
           write!(out, "Not Found")?;
        }
        out.queue(MoveToRow(u16::MAX - 1))?
            .queue(MoveToPreviousLine(ctx.window_height - 2))?
            .queue(MoveToColumn((Self::INPUT_PREFIX_LEN + self.id.0.len()).try_into()?))?;
        OK
    }

    fn print_docs(&self, ctx: &Ctx, out: &mut impl Write) -> Result {
        out.queue(MoveToNextLine(1))?.queue(MoveToColumn(0))?.queue(Clear(FromCursorDown))?;
        if let Some(view) = &self.view {
            view.print(out)?;
        } else {
           write!(out, "Not Found")?;
        }
        out.queue(MoveToRow(u16::MAX - 1))?
            .queue(MoveToPreviousLine(ctx.window_height - 2))?
            .queue(MoveToColumn((Self::INPUT_PREFIX_LEN + self.id.0.len()).try_into()?))?;
        OK
    }

    fn process_key_event<'ctx>(&mut self, event: &KeyEvent, ctx: &'ctx Ctx, out: &mut impl Write)
        -> Result<Option<Mode<'ctx>>>
    {
        let mut changed = self.view.as_mut().is_some_and(|x| x.process_key_event(event));
        changed |= match event.code {
            KeyCode::Enter => {
                let mut new_view = Self::get_view(&self.id, ctx)?;
                if let (Some(new), Some(old)) = (&mut new_view, &mut self.view) {
                    new.set_scroll(old.scroll());
                    new.set_shift(old.shift());
                }
                self.view = new_view;
                true
            }

            KeyCode::Char(ch) if ch != '\n' => {
                self.id.0.push(ch);
                write!(out, "{ch}")?;
                true
            }

            KeyCode::Backspace => if self.id.0.pop().is_some() {
                out.queue(MoveLeft(1))?;
                write!(out, " ")?;
                true
            } else {
                false
            }

            _ => false,
        };

        if changed {
            self.print_docs(ctx, out)?;
        }
        Ok(None)
    }
}

enum Mode<'ctx> {
    Search(SearchMode<'ctx>),
    DocView(DocViewMode),
}

impl<'ctx> Mode<'ctx> {
    fn init(&self, ctx: &'ctx Ctx, out: &mut impl Write) -> Result {
        match self {
            Self::Search(m) => m.init(ctx, out),
            Self::DocView(m) => m.init(ctx, out),
        }
    }

    const fn name(&self) -> &'static str {
        match self {
            Self::Search(_) => "SEARCH",
            Self::DocView(_) => "DOCUMENTATION VIEW",
        }
    }

    fn process_key_event(&mut self, event: &KeyEvent, ctx: &'ctx Ctx, out: &mut impl Write)
        -> Result<Option<Self>>
    {
        match self {
            Self::Search(m) => m.process_key_event(event, ctx, out),
            Self::DocView(m) => m.process_key_event(event, ctx, out),
        }
    }
}

fn print_modes<'ctx>(
    modes: impl IntoIterator<IntoIter = impl DoubleEndedIterator<Item = &'ctx Mode<'ctx>>>,
    out: &mut impl Write,
) -> Result {
    write!(out, "{BOLD}Mode{NOSTYLE}: ")?;
    for mode in modes.into_iter().rev() {
        write!(out, "{} \u{2190} ", mode.name())?;
    }
    write!(out, "{NL}")?;
    OK
}

async fn main_inner(
    r#in: &mut (impl BufRead + Send),
    mut out: &mut (impl Write + Send),
) -> Result {
    let ctx = Ctx::new(r#in, out).await?;
    enable_raw_mode()?;
    out.queue(ScrollUp(ctx.window_height))?
        .queue(MoveToPreviousLine(ctx.window_height))?;
    write!(out, "{BOLD}Shrimple{NOSTYLE} documentation v{}{NL}", env!("CARGO_PKG_VERSION"))?;
    let mut modes = vec![Mode::Search(SearchMode::new(String::new(), &ctx))];
    print_modes(&modes, &mut out)?;
    modes[0].init(&ctx, &mut out)?;
    out.flush()?;

    loop {
        let event = next_key_event()?;
        if event.code == KeyCode::Esc && event.modifiers == KeyModifiers::NONE {
            modes.pop();
            out.queue(MoveToRow(u16::MAX - 1))?
                .queue(MoveToPreviousLine(ctx.window_height - 1))?
                .queue(Clear(FromCursorDown))?;
            print_modes(&modes, &mut out)?;
            let mode = modes.last_mut().ok_or_else(|| anyhow!(Exit))?;
            mode.init(&ctx, &mut out)?;
            mode.process_key_event(&NULL_EVENT, &ctx, &mut out)?;
        } else if let Some(mut new_mode) = modes.last_mut().context("bug: no mode")?
            .process_key_event(&event, &ctx, &mut out)?
        {
            out.queue(MoveToRow(u16::MAX - 1))?
                .queue(MoveToPreviousLine(ctx.window_height - 1))?
                .queue(Clear(FromCursorDown))?;
            print_modes(modes.iter().chain(once(&new_mode)), &mut out)?;
            new_mode.init(&ctx, &mut out)?;
            new_mode.process_key_event(&NULL_EVENT, &ctx, &mut out)?;
            modes.push(new_mode);
        }
        out.flush()?;
        
    }
}

#[tokio::main(flavor = "current_thread")]
async fn main() -> Result {
    let mut stdout = stdout();
    let mut stdin = BufReader::new(stdin());
    stdout.execute(DisableLineWrap).inspect_err(|_| _ = disable_raw_mode())?;
    let res = main_inner(&mut stdin, &mut stdout).await;
    stdout.execute(EnableLineWrap)?;
    disable_raw_mode()?;
    res
}
