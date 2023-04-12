#[derive(Debug)]
struct Polynomial(Vec<u32>);

fn main() {
    let p = Polynomial(vec![1, 2, 3]);
    println!("{p:?}");
}
