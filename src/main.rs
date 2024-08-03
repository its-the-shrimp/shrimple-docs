mod utils;
mod docview;
mod docs;
mod item_visitor;
mod cache;

use {
    crate::{
        docs::{Docs, SearchResult},
        docview::DocView,
        utils::{Exit, Result, BOLD, INVERT, NL, NOSTYLE, NULL_EVENT, OK},
    },
    anyhow::{bail, Context},
    crossterm::{
        cursor::{MoveToPreviousLine, MoveToRow, RestorePosition, SavePosition},
        event::{Event, KeyCode, KeyEvent, KeyModifiers},
        terminal::{
            disable_raw_mode,
            enable_raw_mode,
            size,
            Clear,
            ClearType::FromCursorDown,
            DisableLineWrap,
            EnableLineWrap,
            ScrollUp,
        },
        ExecutableCommand,
        QueueableCommand,
    },
    std::{
        env::args_os,
        io::{stdin, stdout, Read, Write},
        mem::take,
        num::NonZero,
        panic,
        path::PathBuf,
    },
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

    async fn print_modes(&mut self, out: &mut (impl Write + Send)) -> Result {
        out.queue(MoveToRow(u16::MAX - 1))?
            .queue(MoveToPreviousLine(self.window_height.saturating_sub(2)))?
            .queue(Clear(FromCursorDown))?;
        write!(out, "{BOLD}Mode{NOSTYLE}: ")?;
        for mode in self.modes.iter().rev() {
            write!(out, "{} \u{2190} ", mode.name())?;
        }
        let mut mode = self.modes.last_mut().map(take).ok_or(Exit)?;
        write!(out, "{NL}")?;
        mode.init(self, out)?;
        mode.process_key_event(&NULL_EVENT, self, out).await?;
        if let Some(mode_slot) = self.modes.last_mut() {
            *mode_slot = mode;
        }
        OK
    }

    async fn process_key_event(&mut self, event: &KeyEvent, out: &mut (impl Write + Send))
        -> Result<Option<Mode>>
    {
        let mut mode = take(self.modes.last_mut().context("no mode while processing a key event")?);
        let res = mode.process_key_event(event, self, out).await;
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
        let n = ctx.window_height.saturating_sub(5);
        out.queue(SavePosition)?;

        if self.results.is_empty() {
            write!(out, "{NL}<no results>")?;
        }

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

    async fn process_key_event(
        &mut self,
        event: &KeyEvent,
        ctx: &Ctx<'_>,
        out: &mut (impl Write + Send),
    )
        -> Result<Option<Mode>>
    {
        let n_results = usize::saturating_sub(ctx.window_height.into(), 5);

        let mut changed = false;
        match event.code {
            KeyCode::Up => {
                if event.modifiers == KeyModifiers::SHIFT {
                    self.selected = Some(0);
                    self.shift = 0;
                } else {
                    self.selected = self.selected.and_then(|x| x.checked_sub(1));
                    if self.selected.is_some_and(|s| s.wrapping_sub(self.shift) < n_results / 2) {
                        self.shift = self.shift.saturating_sub(1);
                    }
                }
            }

            KeyCode::Down => if let Some(res_len) = NonZero::new(self.results.len()) {
                if event.modifiers == KeyModifiers::SHIFT {
                    self.selected = self.results.len().checked_sub(1);
                    self.shift = self.results.len().saturating_sub(n_results);
                } else {
                    let new = self.selected.map_or(0, |x| x.wrapping_add(1) % res_len);
                    if new == 0 {
                        self.shift = 0;
                    } else if new.wrapping_sub(self.shift) > n_results / 2 {
                        self.shift = self.shift
                            .wrapping_add(1)
                            .min(res_len.get().saturating_sub(n_results));
                    }
                    self.selected = Some(new);
                }
            }

            KeyCode::Enter => if let Some(selected) = self.selected {
                let docview = DocViewMode::new(&self.results[selected].id, ctx)?;
                return Ok(Some(Mode::DocView(docview)))
            } else {
                changed = true;
            }

            KeyCode::Char(ch) if ch != '\n' => {
                self.query.push(ch);
                write!(out, "{ch}")?;
            }

            KeyCode::Backspace => {
                if self.query.pop().is_some() {
                    write!(out, "\x1b[1D \x1b[1D")?;
                }
            }

            _ => return Ok(None),
        }
        out.queue(Clear(FromCursorDown))?;

        if changed {
            ctx.docs.search(&mut self.query, &mut self.results).await?;
        }
        self.print_results(ctx, out)?;
        Ok(None)
    }
}

struct DocViewMode {
    id: String,
    id_changed: bool,
    view: Option<DocView>,
}

impl DocViewMode {
    const INPUT_PREFIX: &'static str = "view \u{2192} ";

    fn get_view(id: &str, ctx: &Ctx) -> Result<Option<DocView>> {
        let Some(item) = ctx.docs.index().get(id) else {
            return Ok(None);
        };
        Ok(Some(DocView::new(item, ctx.docs)?))
    }

    fn new(id: &str, ctx: &Ctx) -> Result<Self> {
        Ok(Self { view: Self::get_view(id, ctx)?, id: id.to_owned(), id_changed: false })
    }

    fn init(&self, ctx: &Ctx, out: &mut impl Write) -> Result {
        write!(out, "{}{}", Self::INPUT_PREFIX, self.id)?;
        self.print_docs(ctx, out)
    }

    fn print_docs(&self, ctx: &Ctx, out: &mut impl Write) -> Result {
        out
            .queue(MoveToRow(u16::MAX - 1))?
            .queue(MoveToPreviousLine(ctx.window_height.saturating_sub(4)))?
            .queue(Clear(FromCursorDown))?;
        if let Some(view) = &self.view {
            view.print(out, ctx.window_width, ctx.window_height.saturating_sub(4))?;
        } else {
            write!(out, "Not Found")?;
        }
        OK
    }

    fn process_key_event(&mut self, event: &KeyEvent, ctx: &Ctx, out: &mut impl Write)
        -> Result<Option<Mode>>
    {
        let mut changed = self.view.as_mut().is_some_and(|x| x.process_key_event(event));
        match event.code {
            KeyCode::Enter => if self.id_changed {
                let mut new_view = Self::get_view(&self.id, ctx)?;
                if let (Some(new), Some(old)) = (&mut new_view, &mut self.view) {
                    new.set_cursor_x(old.cursor_x());
                    new.set_cursor_y(old.cursor_y());
                }
                self.view = new_view;
                changed = true;
            }

            KeyCode::Char(ch) if ch != '\n' => {
                self.id.push(ch);
                write!(out, "{ch}")?;
                self.id_changed = true;
                changed = true;
            }

            KeyCode::Backspace => if self.id.pop().is_some() {
                write!(out, "\x1b[1D \x1b[1D")?;
                self.id_changed = true;
                changed = true;
            }

            _ => {}
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

    async fn process_key_event(
        &mut self,
        event: &KeyEvent,
        ctx: &Ctx<'_>,
        out: &mut (impl Write + Send),
    )
        -> Result<Option<Self>>
    {
        match self {
            Self::None => Ok(None),
            Self::Search(m) => m.process_key_event(event, ctx, out).await,
            Self::DocView(m) => m.process_key_event(event, ctx, out),
        }
    }
}

async fn main_inner(
    args: Args,
    r#in: &mut (impl Read + Send),
    out: &mut (impl Write + Send),
) -> Result {
    let docs = Docs::new(r#in, out, args.offline, args.collect_failures).await?;
    let mut ctx = Ctx::new(&docs)?;
    enable_raw_mode()?;
    out.queue(ScrollUp(ctx.window_height))?
        .queue(MoveToPreviousLine(ctx.window_height))?;
    write!(out, "{BOLD}Shrimple{NOSTYLE} documentation v{}{NL}", env!("CARGO_PKG_VERSION"))?;
    ctx.print_modes(out).await?;
    out.flush()?;

    loop {
        let key_event = next_key_event()?;
        if key_event.code == KeyCode::Char('c') && key_event.modifiers == KeyModifiers::CONTROL {
            bail!(Exit)
        } else if key_event.code == KeyCode::Esc && key_event.modifiers == KeyModifiers::NONE {
            ctx.modes.pop();
            ctx.print_modes(out).await?;
        } else if let Some(new_mode) = ctx.process_key_event(&key_event, out).await? {
            ctx.modes.push(new_mode);
            ctx.print_modes(out).await?;
        }
        out.flush()?;
    }
}

const USAGE: &str = "\
Browse Rust documentation right in the terminal

Usage: shrimple-docs [options]

Options:
    --offline             Run without accessing the network
    --collect-failures    For every crate that couldn't be documented,
                          save the stderr from its compilation in the current crate's target folder
    -h, --help    Print help
";

struct Args {
    offline: bool,
    collect_failures: bool,
}

impl Args {
    fn parse(out: &mut impl Write) -> Result<Self> {
        let mut args = args_os();
        let mut res = Self {
            offline: false,
            collect_failures: false,
        };
        let _program_name = args.next().context("no program name provided to `shrimple-docs`")?;
        for arg in args {
            match arg.as_encoded_bytes() {
                b"--offline" => res.offline = true,
                b"--collect-failures" => res.collect_failures = true,
                b"-h" | b"--help" => {
                    write!(out, "{USAGE}")?;
                    bail!(Exit)
                }
                _ => bail!("unknown argument: {}", PathBuf::from(arg).display())
            }
        }
        Ok(res)
    }
}

#[tokio::main(flavor = "current_thread")]
async fn main() -> Result {
    let builtin_hook = panic::take_hook();
    panic::set_hook(Box::new(move |info| {
        let mut stdout = stdout();
        _ = stdout.execute(RestorePosition);
        _ = stdout.execute(EnableLineWrap);
        _ = disable_raw_mode();
        builtin_hook(info);
    }));
    let mut stdout = stdout();
    let mut stdin = stdin();
    let args = Args::parse(&mut stdout)?;
    stdout.execute(SavePosition)?.execute(DisableLineWrap)?;
    let res = main_inner(args, &mut stdin, &mut stdout).await;
    disable_raw_mode()?;
    _ = stdout.execute(RestorePosition)?.execute(EnableLineWrap)?;
    writeln!(stdout)?;
    Ok(match res {
        Err(e) if e.is::<Exit>() => writeln!(stdout, "{Exit}")?,
        x => x?,
    })
}
