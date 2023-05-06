mod disequality;
mod multiset;
mod polynomial;
mod proof;
pub mod proof_search;
mod substitution;
mod term;

extern "C" {
    pub fn alert(ptr: *const u8, length: usize);
}

#[no_mangle]
pub extern "C" fn greet() {
    let s = "Hellow, world";
    unsafe { alert(s.as_ptr(), s.len()) };
}
