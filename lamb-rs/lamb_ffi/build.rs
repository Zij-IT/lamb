const C_FILES: &[&str] = &[
    "./src/c/lib.cpp",
    "./src/c/ast/ast.cpp",
    "./src/c/compile/value.cpp",
    "./src/c/compile/chunk.cpp",
    "./src/c/compile/ast.cpp",
    "./src/c/compile/object.cpp",
    "./src/c/compile/table.cpp",
    "./src/c/compile/compiler.cpp",
    "./src/c/compile/memory.cpp",
    "./src/c/compile/misc.cpp",
    "./src/c/debug/debug.cpp",
    "./src/c/vm/vm.cpp",
    "./src/c/vm/native.cpp",
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
        .cpp(true)
        .flag("-lfl")
        .flag("-std=c++20")
        .warnings(true)
        .compile("lamb_c");

    // This build-iful piece here generates the necessary headers required
    // for the bindings.
    bindgen::Builder::default()
        .clang_arg("-std=c++20")
        .header("wrapper.hpp")
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
