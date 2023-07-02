use clap::{Parser, ValueEnum};
use std::path::PathBuf;

#[repr(C)]
#[derive(ValueEnum, Clone, Copy, Debug)]
pub enum OptLevel {
    None,
    Basic,
    Some,
    All,
}

#[repr(C)]
#[derive(ValueEnum, Clone, Copy, Debug)]
pub enum DebugLevel {
    None,
    Basic,
    Full,
}

#[repr(C)]
#[derive(ValueEnum, Clone, Copy, Debug)]
pub enum GcDebugLevel {
    None,
    Alloc,
    Collection,
    AllocAndCollection,
}

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct LambOptions {
    #[arg(short, long, value_enum, default_value_t = DebugLevel::None)]
    pub debug_level: DebugLevel,

    #[arg(short, long, value_enum, default_value_t = GcDebugLevel::None)]
    pub gc_debug_level: GcDebugLevel,

    #[arg(short, long, value_enum, default_value_t = OptLevel::None)]
    pub optimization_level: OptLevel,

    pub path: Option<PathBuf>,
}

impl LambOptions {
    pub fn parse() -> Self {
        <Self as Parser>::parse()
    }
}
