use std::io::{BufRead, Write};

const REPL_START: &[u8] = concat!(
    ",~~~@> Baaaah... Welcome to the Lamb REPL! (Lamb v0.1.0)\n",
    " W-W'  Type ':quit' to exit, or ':run' to run the input.\n",
    "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n",
)
.as_bytes();

pub fn input() -> std::io::Result<Option<String>> {
    let mut input = String::new();
    let mut line = String::new();

    let mut stdin = std::io::stdin().lock();
    let mut stdout = std::io::stdout().lock();
    let mut write = |b| -> std::io::Result<()> {
        stdout.write(b)?;
        stdout.flush()?;
        Ok(())
    };

    write(REPL_START)?;
    write(b">>> ")?;

    while stdin.read_line(&mut line)? != 0 {
        match line.as_str().trim() {
            ":quit" => return Ok(None),
            ":run" => break,
            _ => input.push_str(line.as_str()),
        }

        write(b">>> ")?;
        line.clear();
    }

    write(b"\n")?;
    Ok(Some(input))
}
