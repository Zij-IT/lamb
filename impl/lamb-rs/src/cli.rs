use std::{
    ffi::{c_char, CString},
    path::PathBuf,
};

use clap::{Parser, ValueEnum};

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
pub struct Cli {
    #[arg(short, long, value_enum, default_value_t = DebugLevel::None)]
    pub debug_level: DebugLevel,

    #[arg(short, long, value_enum, default_value_t = GcDebugLevel::None)]
    pub gc_debug_level: GcDebugLevel,

    #[arg(short, long, value_enum, default_value_t = OptLevel::None)]
    pub optimization_level: OptLevel,

    pub path: Option<PathBuf>,
}

#[repr(C)]
pub struct CSafeCli {
    pub debug_level: DebugLevel,
    pub gc_debug_level: GcDebugLevel,
    pub optimization_level: OptLevel,
    pub path: *const c_char,
}

// Yes, this function currently leaks memory so that the `*const i8` can be passed to
// C. I should probably expose a function to give up that memory... but... eh.
impl From<Cli> for CSafeCli {
    fn from(value: Cli) -> Self {
        Self {
            debug_level: value.debug_level,
            gc_debug_level: value.gc_debug_level,
            optimization_level: value.optimization_level,
            path: match value.path.as_ref().and_then(|x| x.to_str()) {
                Some(path) => CString::new(path)
                    .expect("There should be no null bytes in the path.")
                    .into_raw(),
                None => std::ptr::null(),
            },
        }
    }
}
