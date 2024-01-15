#![allow(clippy::unused_io_amount)]

use std::path::PathBuf;

use directories::ProjectDirs;
use rustyline::error::ReadlineError;

const REPL_START: &str = concat!(
    ",~~~@> Baaaah... Welcome to the Lamb REPL! (Lamb v0.1.0)\n",
    " W-W'  Type ':quit' to exit, or ':run' to run the input.\n",
    "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n",
);

pub fn input() -> Result<Option<String>, Error> {
    let mut repl = Repl::new()?;
    match repl.with_history() {
        Ok(_) => (),
        Err(err) => println!("[Lamb]: Error while loading history ({err})"),
    }

    let mut input = String::with_capacity(32);

    print!("{REPL_START}");

    loop {
        let line = repl.inner.readline(">>> ");
        match line {
            Ok(line) => match line.trim() {
                ":quit" => {
                    if let Some(path) = repl.history.as_deref() {
                        repl.inner.save_history(path)?;
                    }
                    return Ok(None);
                }
                ":run" => break,
                _ => {
                    repl.inner.add_history_entry(line.as_str())?;
                    input.push_str(&line);
                }
            },
            Err(ReadlineError::Interrupted) => continue,
            Err(ReadlineError::Eof) => {
                if let Some(path) = repl.history.as_deref() {
                    repl.inner.save_history(path)?;
                }
                break;
            }
            Err(err) => {
                println!("Error: {err}");
            }
        }
    }

    Ok(Some(input))
}

#[derive(Debug)]
pub enum Error {
    Io(std::io::Error),
    Rustyline(ReadlineError),
    NoValidHomeDir,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Io(err) => write!(f, "[Lamb]: IO Error ({err})"),
            Error::Rustyline(err) => write!(f, "[Lamb]: Rustyline Error ({err})"),
            Error::NoValidHomeDir => write!(f, "[Lamb]: Found no valid home directory"),
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

struct Repl {
    inner: rustyline::Editor<(), rustyline::history::DefaultHistory>,
    history: Option<PathBuf>,
}

impl Repl {
    pub fn new() -> Result<Self, Error> {
        let editor = rustyline::DefaultEditor::new()?;
        let history_path =
            ProjectDirs::from("", "zij-it", "lamb").map(|pd| pd.data_dir().join("history.txt"));

        Ok(Self {
            inner: editor,
            history: history_path,
        })
    }

    pub fn with_history(&mut self) -> Result<&mut Self, Error> {
        let history_path =
            ProjectDirs::from("", "zij-it", "lamb").map(|pd| pd.data_dir().join("history.txt"));

        match history_path.as_deref() {
            Some(path) => match self.inner.load_history(path) {
                Ok(()) => {}
                Err(ReadlineError::Io(err)) if err.kind() == std::io::ErrorKind::NotFound => {
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
}
