use std::process::Command;

extern crate bindgen;

fn main() {
    let llvm_libdir = Command::new("./llvm_c_2/llvm-config")
        .arg("--libdir")
        .output()
        .unwrap();
    let llvm_libdir = String::from_utf8(llvm_libdir.stdout).unwrap();

    println!("cargo:rustc-link-search={}", llvm_libdir);
    println!("cargo:rustc-link-search=./llvm_c_2/build");
    println!("cargo:rustc-link-lib=llvm_c_2");
    println!("cargo:rustc-link-lib=c++");
    println!("cargo:rustc-link-lib=LLVM-13");

    println!("cargo:rerun-if-changed=src/wrapper.h");

    let bindings = bindgen::Builder::default()
        .clang_args(&["-I./llvm_c_2/include"])
        .header("src/wrapper.h")
        .parse_callbacks(Box::new(bindgen::CargoCallbacks))
        .generate()
        .expect("Unable to generate bindings");

    bindings
        .write_to_file("src/bindings.rs")
        .expect("Couldn't write bindings!");
}
