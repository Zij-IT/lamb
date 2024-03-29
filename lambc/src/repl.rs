#![allow(clippy::unused_io_amount)]

use std::{io::Write, path::PathBuf};

use directories::ProjectDirs;
use rustyline::{error::ReadlineError, Config};

#[derive(Debug)]
pub enum Error {
    Io(std::io::Error),
    Rustyline(ReadlineError),
    Runtime(lambc_vm::Error),
    NoValidHomeDir,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Io(err) => write!(f, "[Lamb]: IO Error ({err})"),
            Error::Rustyline(err) => {
                write!(f, "[Lamb]: Rustyline Error ({err})")
            }
            Error::NoValidHomeDir => {
                write!(f, "[Lamb]: Found no valid home directory")
            }
            Error::Runtime(run) => write!(f, "[Lamb]: Runtime Error: {run}"),
        }
    }
}

impl std::error::Error for Error {}

impl From<std::io::Error> for Error {
    fn from(value: std::io::Error) -> Self {
        Self::Io(value)
    }
}

impl From<rustyline::error::ReadlineError> for Error {
    fn from(value: rustyline::error::ReadlineError) -> Self {
        Self::Rustyline(value)
    }
}

impl From<lambc_vm::Error> for Error {
    fn from(value: lambc_vm::Error) -> Self {
        Self::Runtime(value)
    }
}

pub enum Command {
    String(String),
    Quit,
    Run,
}

pub struct Repl {
    inner: rustyline::Editor<(), rustyline::history::DefaultHistory>,
    history: Option<PathBuf>,
}

impl Repl {
    pub const REPL_START: &'static str = concat!(
        ",~~~@> Baaaah... Welcome to the Lamb REPL! (Lamb v0.1.0)\n",
        " W-W'  Type ':quit' to exit.\n",
        "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n",
    );

    pub fn new() -> Result<Self, Error> {
        let config = Config::builder().check_cursor_position(true).build();
        let editor = rustyline::Editor::with_config(config)?;
        let history_path = ProjectDirs::from("", "zij-it", "lamb")
            .map(|pd| pd.data_dir().join("history.txt"));

        Ok(Self { inner: editor, history: history_path })
    }

    pub fn with_history(&mut self) -> Result<&mut Self, Error> {
        let history_path = ProjectDirs::from("", "zij-it", "lamb")
            .map(|pd| pd.data_dir().join("history.txt"));

        match history_path.as_deref() {
            Some(path) => match self.inner.load_history(path) {
                Ok(()) => {}
                Err(ReadlineError::Io(err))
                    if err.kind() == std::io::ErrorKind::NotFound =>
                {
                    std::fs::create_dir_all(
                        path.parent()
                            .expect("ProjectDirs would have returned None if no home dir existed"),
                    )?;
                    std::fs::File::create(path)?;
                }
                Err(err) => return Err(Error::Rustyline(err)),
            },
            _ => return Err(Error::NoValidHomeDir),
        }

        Ok(self)
    }

    pub fn read_line(&mut self) -> Result<Command, Error> {
        loop {
            std::io::stdout().flush()?;
            let line = self.inner.readline(">>> ");
            match line {
                Ok(line) => match &*line {
                    ":quit" => return Ok(Command::Quit),
                    _ => {
                        self.inner.add_history_entry(&line)?;
                        return Ok(Command::String(line));
                    }
                },
                Err(ReadlineError::Interrupted) => continue,
                Err(ReadlineError::Eof) => return Ok(Command::Run),
                Err(err) => return Err(err.into()),
            }
        }
    }
}

impl Drop for Repl {
    fn drop(&mut self) {
        if let Some(path) = self.history.as_deref() {
            let _ = self.inner.save_history(path);
        }
    }
}
