const C_FILES: &[&str] = &[
    "../main.c",
    "../ast/ast.c",
    "../ast/optimization.c",
    "../parsing/built/lexer.c",
    "../parsing/built/parser.tab.c",
    "../compile/value.c",
    "../compile/chunk.c",
    "../compile/ast.c",
    "../compile/object.c",
    "../compile/table.c",
    "../compile/compiler.c",
    "../compile/memory.c",
    "../compile/misc.c",
    "../debug/debug.c",
    "../vm/vm.c",
    "../vm/native.c",
];

fn main() {
    // This here generates the C shared libary that gets hooked onto the
    // Rust build.

    cc::Build::new()
        .files(
            C_FILES
                .iter()
                .inspect(|f| println!("cargo:rerun-if-changed={f}")),
        )
        .flag("-lfl")
        .warnings(true)
        .compile("lamb_c");

    // This build-iful piece here generates the necessary headers required
    // for the bindings.
    bindgen::Builder::default()
        .header("wrapper.h")
        .parse_callbacks(Box::new(bindgen::CargoCallbacks))
        .generate()
        .expect("Unable to generate bindings")
        .write_to_file("src/ffi/bindings.rs")
        .expect("Couldn't write bindings!");
}
