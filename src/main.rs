mod utils;
mod item_printer;
mod docs;
mod item_visitor;

use {
    crate::{
        docs::{Docs, SearchResult},
        item_printer::print_item,
        utils::{Exit, Result, StringView, BOLD, INVERT, NL, NOSTYLE, NULL_EVENT, OK},
    },
    anyhow::{bail, Context},
    crossterm::{
        cursor::{
            MoveLeft,
            MoveToColumn,
            MoveToNextLine,
            MoveToPreviousLine,
            MoveToRow,
            RestorePosition,
            SavePosition,
        },
        event::{Event, KeyCode, KeyEvent, KeyModifiers},
        terminal::{
            disable_raw_mode,
            enable_raw_mode,
            size,
            Clear,
            ClearType::FromCursorDown,
            ScrollUp,
        },
        ExecutableCommand,
        QueueableCommand,
    },
    std::{env::args_os, io::{stdin, stdout, Read, Write}, mem::take, panic},
};

fn next_key_event() -> Result<KeyEvent> {
    loop {
        let Event::Key(event) = crossterm::event::read()? else { continue };
        break Ok(event)
    }
}

struct Ctx<'docs> {
    docs: &'docs Docs,
    modes: Vec<Mode>,
    window_height: u16,
    window_width: u16,
}

impl<'docs> Ctx<'docs> {
    const DEFAULT_WINHEIGHT: u16 = 15;

    fn new(docs: &'docs Docs) -> Result<Self> {
        let mut res = Self {
            docs,
            window_height: Self::DEFAULT_WINHEIGHT,
            window_width: size()?.1,
            modes: vec![],
        };
        res.modes.push(Mode::Search(SearchMode::new(String::new(), &res)));
        Ok(res)
    }

    fn print_modes(&mut self, out: &mut impl Write) -> Result {
        out.queue(MoveToRow(u16::MAX - 1))?
            .queue(MoveToPreviousLine(self.window_height - 2))?
            .queue(Clear(FromCursorDown))?;
        write!(out, "{BOLD}Mode{NOSTYLE}: ")?;
        for mode in self.modes.iter().rev() {
            write!(out, "{} \u{2190} ", mode.name())?;
        }
        let mut mode = self.modes.last_mut().map(take).ok_or(Exit)?;
        write!(out, "{NL}")?;
        mode.init(self, out)?;
        mode.process_key_event(&NULL_EVENT, self, out)?;
        if let Some(mode_slot) = self.modes.last_mut() {
            *mode_slot = mode;
        }
        OK
    }

    fn process_key_event(&mut self, event: &KeyEvent, out: &mut impl Write)
        -> Result<Option<Mode>>
    {
        let mut mode = take(self.modes.last_mut().context("no mode while processing a key event")?);
        let res = mode.process_key_event(event, self, out);
        if let Some(mode_slot) = self.modes.last_mut() {
            *mode_slot = mode;
        }
        res
    }
}

struct SearchMode {
    selected: Option<usize>,
    shift: usize,
    query: String,
    results: Vec<SearchResult>,
}

impl SearchMode {
    const INPUT_PREFIX: &'static str = "search \u{2192} ";

    const fn new(query: String, _: &Ctx) -> Self {
        Self {
            query,
            selected: None,
            shift: 0,
            results: vec![],
        }
    }

    fn print_results(&self, ctx: &Ctx, out: &mut impl Write) -> Result {
        let n = ctx.window_height - 5;
        out.queue(SavePosition)?;

        for (i, result) in self.results.iter().enumerate().skip(self.shift).take(n.into()) {
            let invert = if self.selected == Some(i) {INVERT} else {""};
            write!(out, "{NL}{invert}{i}: {}{NOSTYLE}", result.id)?;
        }

        out.queue(RestorePosition)?;
        OK
    }

    fn init(&self, ctx: &Ctx, out: &mut impl Write) -> Result {
        write!(out, "{BOLD}<Up/Down>{NOSTYLE} - go 1 line up/down in the results{NL}")?;
        write!(out, "{BOLD}<Ctrl-Up/Down>{NOSTYLE} - go to the top/bottom of the results{NL}")?;
        write!(out, "{}{}", Self::INPUT_PREFIX, self.query)?;
        self.print_results(ctx, out)
    }

    fn process_key_event(&mut self, event: &KeyEvent, ctx: &Ctx, out: &mut impl Write)
        -> Result<Option<Mode>>
    {
        let n_results = ctx.window_height as usize - 5;

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
                let docview = DocViewMode::new(&self.results[selected].id, ctx)?;
                return Ok(Some(Mode::DocView(docview)))
            } else {
                true
            }

            KeyCode::Char(ch) if ch != '\n' => {
                self.query.push(ch);
                write!(out, "{ch}")?;
                false
            }

            KeyCode::Backspace => {
                if self.query.pop().is_some() {
                    write!(out, "\x1b[1D \x1b[1D")?;
                }
                false
            }

            _ => return Ok(None),
        };
        out.queue(Clear(FromCursorDown))?;

        if changed {
            ctx.docs.search(&mut self.query, &mut self.results)?;
        }
        self.print_results(ctx, out)?;
        Ok(None)
    }
}

struct DocViewMode {
    id: String,
    view: Option<StringView>,
}

impl DocViewMode {
    const INPUT_PREFIX: &'static str = "view \u{2192} ";

    fn get_view(id: &str, ctx: &Ctx) -> Result<Option<StringView>> {
        let Some(item) = ctx.docs.index().get(id) else {
            return Ok(None);
        };
        let mut content = StringView::new(String::new(), -1, -1);
        print_item(item, ctx.docs, &mut content)?;
        Ok(Some(content))
    }

    fn new(id: &str, ctx: &Ctx) -> Result<Self> {
        Ok(Self { view: Self::get_view(id, ctx)?, id: id.to_owned() })
    }

    fn init(&self, ctx: &Ctx, out: &mut impl Write) -> Result {
        write!(out, "{}{}", Self::INPUT_PREFIX, self.id)?;
        self.print_docs(ctx, out)
    }

    fn print_docs(&self, ctx: &Ctx, out: &mut impl Write) -> Result {
        out.queue(SavePosition)?
            .queue(MoveToNextLine(1))?
            .queue(MoveToColumn(0))?
            .queue(Clear(FromCursorDown))?;
        if let Some(view) = &self.view {
            view.print(out, ctx.window_height - 4, ctx.window_width)?;
        } else {
           write!(out, "Not Found")?;
        }
        out.queue(RestorePosition)?;
        OK
    }

    fn process_key_event(&mut self, event: &KeyEvent, ctx: &Ctx, out: &mut impl Write)
        -> Result<Option<Mode>>
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
                self.id.push(ch);
                write!(out, "{ch}")?;
                true
            }

            KeyCode::Backspace => if self.id.pop().is_some() {
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

#[derive(Default)]
enum Mode {
    #[default]
    None,
    Search(SearchMode),
    DocView(DocViewMode),
}

impl Mode {
    fn init(&self, ctx: &Ctx, out: &mut impl Write) -> Result {
        match self {
            Self::None => OK,
            Self::Search(m) => m.init(ctx, out),
            Self::DocView(m) => m.init(ctx, out),
        }
    }

    const fn name(&self) -> &'static str {
        match self {
            Self::None => "",
            Self::Search(_) => "SEARCH",
            Self::DocView(_) => "DOCUMENTATION VIEW",
        }
    }

    fn process_key_event(&mut self, event: &KeyEvent, ctx: &Ctx, out: &mut impl Write)
        -> Result<Option<Self>>
    {
        match self {
            Self::None => Ok(None),
            Self::Search(m) => m.process_key_event(event, ctx, out),
            Self::DocView(m) => m.process_key_event(event, ctx, out),
        }
    }
}

async fn main_inner(
    args: Args,
    r#in: &mut (impl Read + Send),
    out: &mut (impl Write + Send),
) -> Result {
    let docs = Docs::new(r#in, out, args.offline).await?;
    let mut ctx = Ctx::new(&docs)?;
    enable_raw_mode()?;
    out.queue(ScrollUp(ctx.window_height))?
        .queue(MoveToPreviousLine(ctx.window_height))?;
    write!(out, "{BOLD}Shrimple{NOSTYLE} documentation v{}{NL}", env!("CARGO_PKG_VERSION"))?;
    ctx.print_modes(out)?;
    out.flush()?;

    loop {
        let key_event = next_key_event()?;
        if key_event.code == KeyCode::Char('c') && key_event.modifiers == KeyModifiers::CONTROL {
            bail!(Exit)
        } else if key_event.code == KeyCode::Esc && key_event.modifiers == KeyModifiers::NONE {
            ctx.modes.pop();
            ctx.print_modes(out)?;
        } else if let Some(new_mode) = ctx.process_key_event(&key_event, out)? {
            ctx.modes.push(new_mode);
            ctx.print_modes(out)?;
        }
        out.flush()?;
    }
}

struct Args {
    offline: bool,
}

impl Args {
    fn parse() -> Result<Self> {
        let mut args = args_os();
        let _program_name = args.next().context("no program name provided to `shrimple-docs`")?;
        let offline = args.next().is_some_and(|x| x == "--offline");
        if let Some(extra) = args.next() {
            bail!("extra arguments starting from {extra:?}");
        }
        Ok(Self { offline })
    }
}

#[tokio::main(flavor = "current_thread")]
async fn main() -> Result {
    let builtin_hook = panic::take_hook();
    panic::set_hook(Box::new(move |info| {
        _ = stdout().execute(RestorePosition);
        _ = disable_raw_mode();
        builtin_hook(info);
    }));
    let mut stdout = stdout();
    let mut stdin = stdin();
    let args = Args::parse()?;
    stdout.execute(SavePosition)?;
    let res = main_inner(args, &mut stdin, &mut stdout).await;
    disable_raw_mode()?;
    _ = stdout.execute(RestorePosition)?;
    writeln!(stdout)?;
    Ok(match res {
        Err(e) if e.is::<Exit>() => writeln!(stdout, "{Exit}")?,
        x => x?,
    })
}
