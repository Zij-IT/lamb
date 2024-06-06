use std::path::PathBuf;

use clap::Parser;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct LambOptions {
    pub path: PathBuf,
}

impl LambOptions {
    pub fn parse() -> Self {
        <Self as Parser>::parse()
    }
}
