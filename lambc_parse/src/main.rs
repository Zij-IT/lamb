use std::error::Error;

use lambc_parse::Parser;

fn main() -> Result<(), Box<dyn Error>> {
    let path = std::env::args().last().unwrap();
    let file = std::fs::read_to_string(&path)?;
    let mut parser = Parser::new(file.as_bytes(), &path);
    if let Err(e) = parser.parse_module() {
        eprintln!("Error in file: {path}");
        eprintln!("{e}");
    }

    Ok(())
}
