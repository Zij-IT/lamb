use std::path::Path;

fn main() {
    // Tell cargo to invalidate the built crate whenever the wrapper changes
    println!("cargo:rerun-if-changed=wrapper.h");

    cc::Build::new()
        .file("../main.c")
        .file("../ast/ast.c")
        .file("../ast/optimization.c")
        .file("../parsing/built/lexer.c")
        .file("../parsing/built/parser.tab.c")
        .file("../compile/value.c")
        .file("../compile/chunk.c")
        .file("../compile/ast.c")
        .file("../compile/object.c")
        .file("../compile/table.c")
        .file("../compile/compiler.c")
        .file("../compile/memory.c")
        .file("../compile/misc.c")
        .file("../debug/debug.c")
        .file("../vm/vm.c")
        .file("../vm/native.c")
        .flag("-lfl")
        .warnings(true)
        .compile("lamb_c");

    // The bindgen::Builder is the main entry point
    // to bindgen, and lets you build up options for
    // the resulting bindings.
    let bindings = bindgen::Builder::default()
        // The input header we would like to generate
        // bindings for.
        .header("wrapper.h")
        // Tell cargo to invalidate the built crate whenever any of the
        // included header files changed.
        .parse_callbacks(Box::new(bindgen::CargoCallbacks))
        // Finish the builder and generate the bindings.
        .generate()
        // Unwrap the Result and panic on failure.
        .expect("Unable to generate bindings");

    // Write the bindings to the $OUT_DIR/bindings.rs file.
    let out_path = Path::new("src/ffi");
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
}
