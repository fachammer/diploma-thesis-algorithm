use std::ops::{Add, Mul};

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

impl Mul for Polynomial {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        let mut coefficients = vec![0; usize::max(0, self.0.len() + rhs.0.len() - 1)];
        for i in 0..self.0.len() {
            for j in 0..rhs.0.len() {
                if i + j < coefficients.len() {
                    coefficients[i + j] += self.0[i] * rhs.0[j];
                }
            }
        }
        Self(coefficients)
    }
}

#[test]
fn add_polynomials() {
    let p = Polynomial(vec![1, 2, 3]);
    let q = Polynomial(vec![1, 2]);
    assert_eq!(p + q, Polynomial(vec![2, 4, 3]));
}

#[test]
fn mul_polynomials() {
    let p = Polynomial(vec![1, 2, 3]);
    let q = Polynomial(vec![1, 2]);
    assert_eq!(p * q, Polynomial(vec![1, 4, 3 + 4, 6]));
}

fn main() {
    println!("{p:?}", p = Polynomial(vec![1, 2, 3]));
}
