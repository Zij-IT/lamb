const C_FILES: &[&str] = &[
    "./src/ffi/c/main.c",
    "./src/ffi/c/ast/ast.c",
    "./src/ffi/c/ast/optimization.c",
    "./src/ffi/c/parsing/built/lexer.c",
    "./src/ffi/c/parsing/built/parser.tab.c",
    "./src/ffi/c/compile/value.c",
    "./src/ffi/c/compile/chunk.c",
    "./src/ffi/c/compile/ast.c",
    "./src/ffi/c/compile/object.c",
    "./src/ffi/c/compile/table.c",
    "./src/ffi/c/compile/compiler.c",
    "./src/ffi/c/compile/memory.c",
    "./src/ffi/c/compile/misc.c",
    "./src/ffi/c/debug/debug.c",
    "./src/ffi/c/vm/vm.c",
    "./src/ffi/c/vm/native.c",
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
