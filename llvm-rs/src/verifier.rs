use crate::*;

pub fn verify_function(function: &Function, out: &mut std::fs::File) -> bool {
    use std::os::unix::io::AsRawFd;
    let fd = out.as_raw_fd();
    unsafe {
        let os = c::llvm__raw_fd_ostream__create(
            fd,
            false,
            false,
            c::llvm_OStreamKind_llvm_OStreamKind__OK_OStream,
        );
        let ret = c::llvm_verifyFunction(function.as_ptr(), os as c::llvm__raw_ostream__ref);
        // c::llvm__raw_fd_ostream__dispose(os);
        ret
    }
}

/// # Returns
/// (ok: bool, broken_debug_info: bool)
pub fn verify_module(module: &Module, out: &mut std::fs::File) -> (bool, bool) {
    use std::os::unix::io::AsRawFd;
    let fd = out.as_raw_fd();
    unsafe {
        let os = c::llvm__raw_fd_ostream__create(
            fd,
            false,
            false,
            c::llvm_OStreamKind_llvm_OStreamKind__OK_OStream,
        );
        let mut broken_debug_info = false;
        let ret = c::llvm_verifyModule(
            module.ptr,
            os as c::llvm__raw_ostream__ref,
            &mut broken_debug_info as *mut bool,
        );
        // c::llvm__raw_fd_ostream__dispose(os);
        (ret, broken_debug_info)
    }
}
