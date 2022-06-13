extern crate bindgen;

fn main() {
    println!("cargo:rustc-link-search=../llvm_c_2/build");
    println!("cargo:rustc-link-lib=llvm_c_2");
    println!("cargo:rerun-if-changed=src/wrapper.h");

    let bindings = bindgen::Builder::default()
        .clang_args(&["-I../llvm_c_2/include"])
        .header("src/wrapper.h")
        .parse_callbacks(Box::new(bindgen::CargoCallbacks))
        .generate()
        .expect("Unable to generate bindings");

    bindings
        .write_to_file("src/bindings.rs")
        .expect("Couldn't write bindings!");
}
