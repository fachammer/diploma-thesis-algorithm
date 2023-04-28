use std::collections::{HashMap, HashSet};
use std::hash::Hash;

#[derive(Debug, Clone)]
pub struct Multiset<T>
where
    T: Hash + Eq,
{
    elements: HashMap<T, u32>,
}

impl<T> FromIterator<(T, u32)> for Multiset<T>
where
    T: Eq + Hash,
{
    fn from_iter<U: IntoIterator<Item = (T, u32)>>(iter: U) -> Self {
        let mut multiset = Self::new();
        for e in iter {
            *multiset.amount_mut(e.0) = e.1;
        }
        multiset
    }
}

impl<T> PartialEq for Multiset<T>
where
    T: Eq + Hash,
{
    fn eq(&self, other: &Self) -> bool {
        self.is_multisubset_of(other) && other.is_multisubset_of(self)
    }
}

impl<T> Eq for Multiset<T> where T: Eq + Hash {}

impl<T> Hash for Multiset<T>
where
    T: Eq + Hash + Ord,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let mut support: Vec<&T> = self.support().collect();
        support.sort();
        let hashable: Vec<(&T, u32)> = support
            .into_iter()
            .filter(|e| self.amount(e) > 0)
            .map(|e| (e, self.amount(e)))
            .collect();
        hashable.hash(state)
    }
}

impl<T> Multiset<T>
where
    T: Eq + Hash,
{
    pub fn new() -> Self {
        Self {
            elements: HashMap::new(),
        }
    }

    pub fn support(&self) -> impl Iterator<Item = &T> {
        self.elements.keys()
    }

    pub fn amount(&self, element: &T) -> u32 {
        *self.elements.get(element).unwrap_or(&0)
    }

    pub fn amount_mut(&mut self, element: T) -> &mut u32 {
        self.elements.entry(element).or_insert(0)
    }

    pub fn amount_iter(&self) -> impl Iterator<Item = (&T, &u32)> {
        self.elements.iter()
    }

    pub fn into_amount_iter(self) -> impl Iterator<Item = (T, u32)> {
        self.elements.into_iter()
    }

    pub fn is_multisubset_of(&self, other: &Multiset<T>) -> bool {
        for e in self.support() {
            if self.amount(e) > other.amount(e) {
                return false;
            }
        }

        true
    }

    #[cfg(any(target_pointer_width = "32", target_pointer_width = "64"))]
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        let mut elements = vec![];
        for (element, amount) in self.amount_iter() {
            elements.extend(
                std::iter::repeat(element).take(
                    (*amount)
                        .try_into()
                        .expect("target pointer width must be at least 32-bit"),
                ),
            );
        }
        elements.into_iter()
    }
}

impl<T> Multiset<T>
where
    T: Eq + Hash + Clone,
{
    pub fn union(&self, other: &Self) -> Self {
        let mut result = Self::new();
        let self_support: HashSet<&T> = self.support().collect();
        let other_support = other.support().collect();

        let support = self_support.union(&other_support);

        for e in support {
            *result.amount_mut((*e).clone()) = self.amount(e) + other.amount(e);
        }

        result
    }

    pub fn subtract(&self, other: &Self) -> Self {
        let mut result = Self::new();

        for e in self.support() {
            *result.amount_mut((*e).clone()) = self.amount(e).saturating_sub(other.amount(e));
        }

        result
    }

    #[cfg(any(target_pointer_width = "32", target_pointer_width = "64"))]
    pub fn into_monomials_iter(self) -> impl Iterator<Item = T> {
        let mut elements = vec![];
        for (element, amount) in self.into_amount_iter() {
            elements.extend(
                std::iter::repeat(element).take(
                    amount
                        .try_into()
                        .expect("target pointer width must be at least 32-bit"),
                ),
            );
        }
        elements.into_iter()
    }
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn multiset_union() {
        let left = Multiset::from_iter(vec![(0, 2), (1, 1)]);
        let right = Multiset::<u32>::from_iter(vec![(1, 1), (2, 2)]);
        let union = left.union(&right);

        assert_eq!(union, Multiset::from_iter(vec![(0, 2), (1, 2), (2, 2)]));
    }

    #[test]
    fn multiset_subtract() {
        let left = Multiset::from_iter(vec![(0, 2), (1, 1)]);
        let right = Multiset::<u32>::from_iter(vec![(1, 1), (2, 2)]);
        let difference = left.subtract(&right);

        assert_eq!(difference.amount(&0), 2);
        assert_eq!(difference.amount(&1), 0);
        assert_eq!(difference.amount(&2), 0);
    }

    #[test]
    fn multiset_from_iter() {
        let multiset = Multiset::from_iter(vec![(0, 1), (1, 2), (2, 0), (0, 2)]);

        assert_eq!(multiset.amount(&0), 2);
        assert_eq!(multiset.amount(&1), 2);
        assert_eq!(multiset.amount(&2), 0);
        assert_eq!(multiset.amount(&3), 0);
    }

    #[test]
    fn multiset_eq() {
        let left = Multiset::from_iter(vec![(0, 1), (1, 2), (2, 0)]);
        let right = Multiset::from_iter(vec![(0, 1), (1, 2)]);

        assert_eq!(left, right);
    }

    #[test]
    fn multiset_eq_for_empty() {
        let left = Multiset::new();
        let right = Multiset::from_iter(vec![(0, 0)]);

        assert_eq!(left, right);
    }

    #[test]
    fn multiset_iter() {
        let multiset = Multiset::from_iter(vec![(0, 1), (1, 2), (2, 3)]);
        let mut elements: Vec<u32> = multiset.iter().copied().collect();
        elements.sort();
        assert_eq!(elements, vec![0, 1, 1, 2, 2, 2])
    }
}
