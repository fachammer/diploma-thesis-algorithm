use std::ops::Add;

#[derive(Debug, PartialEq, Eq)]
struct Polynomial(Vec<u32>);

impl Add for Polynomial {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        if self.0.len() > rhs.0.len() {
            rhs + self
        } else {
            Self(
                self.0
                    .into_iter()
                    .chain(std::iter::repeat(0))
                    .zip(rhs.0.into_iter())
                    .map(|(x, y)| x + y)
                    .collect(),
            )
        }
    }
}

fn main() {
    let p = Polynomial(vec![1, 2, 3]);
    let q = Polynomial(vec![1, 2]);
    assert_eq!(p + q, Polynomial(vec![2, 4, 3]));
    println!("{p:?}", p = Polynomial(vec![1, 2, 3]));
}
