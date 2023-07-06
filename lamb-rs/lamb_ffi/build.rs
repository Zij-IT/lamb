const C_FILES: &[&str] = &[
    "./src/c/lib.c",
    "./src/c/ast/ast.c",
    "./src/c/compile/value.c",
    "./src/c/compile/chunk.c",
    "./src/c/compile/ast.c",
    "./src/c/compile/object.c",
    "./src/c/compile/table.c",
    "./src/c/compile/compiler.c",
    "./src/c/compile/memory.c",
    "./src/c/compile/misc.c",
    "./src/c/debug/debug.c",
    "./src/c/vm/vm.c",
    "./src/c/vm/native.c",
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
        .allowlist_function("run_ast")
        .allowlist_function("free_ast")
        .allowlist_function("new_astnode")
        .allowlist_function("strdup")
        .allowlist_var("MAX_AST_KID_COUNT")
        .allowlist_type("AstNode_T")
        .allowlist_type("AstNodeType")
        .parse_callbacks(Box::new(bindgen::CargoCallbacks))
        .generate()
        .expect("Unable to generate bindings")
        .write_to_file("src/bindings.rs")
        .expect("Couldn't write bindings!");
}
