use self::vec::MultisetVec;

pub(crate) type Multiset<T> = MultisetVec<T>;

// TODO: add trait for multiset, and get rid of allowing dead code

mod vec {
    use std::hash::Hash;

    #[derive(Debug, Clone)]
    pub(crate) struct MultisetVec<T>
    where
        T: Eq,
    {
        elements: Vec<(T, u32)>,
    }

    impl<T> FromIterator<(T, u32)> for MultisetVec<T>
    where
        T: Eq,
    {
        fn from_iter<U: IntoIterator<Item = (T, u32)>>(iter: U) -> Self {
            let iter = iter.into_iter();
            let (lower_bound, upper_bound) = iter.size_hint();
            let mut multiset = Self {
                elements: Vec::with_capacity(upper_bound.unwrap_or(lower_bound)),
            };
            multiset.extend(iter);
            multiset
        }
    }

    impl<T> PartialEq for MultisetVec<T>
    where
        T: Eq,
    {
        fn eq(&self, other: &Self) -> bool {
            self.is_multisubset_of(other) && other.is_multisubset_of(self)
        }
    }

    impl<T> Eq for MultisetVec<T> where T: Eq {}

    impl<T> Hash for MultisetVec<T>
    where
        T: Eq + Hash + Ord,
    {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            let mut support: Vec<(&T, &u32)> = self.amount_iter().collect();
            support.sort();
            support.hash(state)
        }
    }

    impl<T> MultisetVec<T>
    where
        T: Eq,
    {
        pub(crate) fn new() -> Self {
            Self {
                elements: Vec::new(),
            }
        }

        #[allow(dead_code)]
        pub(crate) fn with_capacity(capacity: usize) -> Self {
            Self {
                elements: Vec::with_capacity(capacity),
            }
        }

        pub(crate) fn support(&self) -> impl Iterator<Item = &T> {
            self.elements
                .iter()
                .filter_map(|(k, v)| if *v > 0 { Some(k) } else { None })
        }

        pub(crate) fn amount(&self, element: &T) -> u32 {
            *self
                .elements
                .iter()
                .filter_map(|(x, amount)| if x == element { Some(amount) } else { None })
                .next()
                .unwrap_or(&0)
        }

        #[allow(dead_code)]
        pub(crate) fn amount_mut_by_ref(&mut self, element: &T) -> Option<&mut u32> {
            self.elements
                .iter_mut()
                .filter_map(|(k, v)| if k == element { Some(v) } else { None })
                .next()
        }

        pub(crate) fn amount_mut(&mut self, element: T) -> &mut u32 {
            let index = self
                .elements
                .iter()
                .enumerate()
                .filter_map(|(i, (k, _))| if k == &element { Some(i) } else { None })
                .next();

            if let Some(index) = index {
                &mut self.elements[index].1
            } else {
                let length = self.elements.len();
                self.elements.push((element, 0));
                &mut self.elements[length].1
            }
        }

        pub(crate) fn amount_iter(&self) -> impl Iterator<Item = (&T, &u32)> {
            self.elements
                .iter()
                .filter_map(|(k, v)| if *v > 0 { Some((k, v)) } else { None })
        }

        #[allow(dead_code)]
        pub(crate) fn amount_iter_mut(&mut self) -> impl Iterator<Item = (&mut T, &mut u32)> {
            self.elements
                .iter_mut()
                .filter_map(|(k, v)| if *v > 0 { Some((k, v)) } else { None })
        }

        pub(crate) fn into_amount_iter(self) -> impl Iterator<Item = (T, u32)> {
            self.elements.into_iter()
        }

        pub(crate) fn is_multisubset_of(&self, other: &MultisetVec<T>) -> bool {
            self.support().all(|k| self.amount(k) <= other.amount(k))
        }

        #[cfg(any(target_pointer_width = "32", target_pointer_width = "64"))]
        pub(crate) fn iter(&self) -> impl Iterator<Item = &T> {
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

        pub(crate) fn contains(&self, element: &T) -> bool {
            for (k, v) in self.elements.iter() {
                if k == element && v > &0 {
                    return true;
                }
            }
            false
        }

        pub(crate) fn remove_all(&mut self, element: &T) {
            for i in 0..self.elements.len() {
                if &self.elements[i].0 == element {
                    self.elements.swap_remove(i);
                    return;
                }
            }
        }
    }

    impl<T> Default for MultisetVec<T>
    where
        T: Eq,
    {
        fn default() -> Self {
            Self::new()
        }
    }

    impl<T> MultisetVec<T>
    where
        T: Eq + Clone,
    {
        #[cfg(any(target_pointer_width = "32", target_pointer_width = "64"))]
        pub(crate) fn into_iter(self) -> impl Iterator<Item = T> {
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

    impl<T> Extend<(T, u32)> for MultisetVec<T>
    where
        T: Eq,
    {
        fn extend<U: IntoIterator<Item = (T, u32)>>(&mut self, iter: U) {
            for (k, v) in iter {
                *self.amount_mut(k) += v;
            }
        }
    }

    #[cfg(test)]
    mod test {
        use super::*;

        #[test]
        fn multiset_extend() {
            let mut left = MultisetVec::from_iter(vec![(0, 2), (1, 1)]);
            let right = MultisetVec::<u32>::from_iter(vec![(1, 1), (2, 2)]);
            left.extend(right.into_amount_iter());

            assert_eq!(left, MultisetVec::from_iter(vec![(0, 2), (1, 2), (2, 2)]));
        }

        #[test]
        fn multiset_from_iter() {
            let multiset = MultisetVec::from_iter(vec![(0, 1), (1, 2), (2, 0), (0, 2)]);

            assert_eq!(multiset.amount(&0), 3);
            assert_eq!(multiset.amount(&1), 2);
            assert_eq!(multiset.amount(&2), 0);
            assert_eq!(multiset.amount(&3), 0);
        }

        #[test]
        fn multiset_eq() {
            let left = MultisetVec::from_iter(vec![(0, 1), (1, 2), (2, 0)]);
            let right = MultisetVec::from_iter(vec![(0, 1), (1, 2)]);

            assert_eq!(left, right);
        }

        #[test]
        fn multiset_eq_for_empty() {
            let left = MultisetVec::new();
            let right = MultisetVec::from_iter(vec![(0, 0)]);

            assert_eq!(left, right);
        }

        #[test]
        fn multiset_iter() {
            let multiset = MultisetVec::from_iter(vec![(0, 1), (1, 2), (2, 3)]);
            let mut elements: Vec<u32> = multiset.iter().copied().collect();
            elements.sort();
            assert_eq!(elements, vec![0, 1, 1, 2, 2, 2])
        }
    }
}

pub(crate) mod hash_map {
    use std::{collections::HashMap, hash::Hash};

    #[derive(Debug, Clone)]
    pub(crate) struct MultisetHashMap<T>
    where
        T: Eq,
    {
        elements: HashMap<T, u32>,
    }

    impl<T> FromIterator<(T, u32)> for MultisetHashMap<T>
    where
        T: Eq + Hash,
    {
        fn from_iter<U: IntoIterator<Item = (T, u32)>>(iter: U) -> Self {
            let iter = iter.into_iter();
            let (lower_bound, upper_bound) = iter.size_hint();
            let mut multiset = Self {
                elements: HashMap::with_capacity(upper_bound.unwrap_or(lower_bound)),
            };
            multiset.extend(iter);
            multiset
        }
    }

    impl<T> PartialEq for MultisetHashMap<T>
    where
        T: Eq + Hash,
    {
        fn eq(&self, other: &Self) -> bool {
            self.is_multisubset_of(other) && other.is_multisubset_of(self)
        }
    }

    impl<T> Eq for MultisetHashMap<T> where T: Eq + Hash {}

    impl<T> Hash for MultisetHashMap<T>
    where
        T: Eq + Hash + Ord,
    {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            let mut support: Vec<(&T, &u32)> = self.amount_iter().collect();
            support.sort();
            support.hash(state)
        }
    }

    impl<T> MultisetHashMap<T>
    where
        T: Eq + Hash,
    {
        pub(crate) fn new() -> Self {
            Self {
                elements: HashMap::new(),
            }
        }

        pub(crate) fn with_capacity(capacity: usize) -> Self {
            Self {
                elements: HashMap::with_capacity(capacity),
            }
        }

        pub(crate) fn support(&self) -> impl Iterator<Item = &T> {
            self.elements
                .iter()
                .filter_map(|(k, v)| if *v > 0 { Some(k) } else { None })
        }

        pub(crate) fn amount(&self, element: &T) -> u32 {
            *self.elements.get(element).unwrap_or(&0)
        }

        pub(crate) fn amount_mut_by_ref(&mut self, element: &T) -> Option<&mut u32> {
            self.elements.get_mut(element)
        }

        pub(crate) fn amount_mut(&mut self, element: T) -> &mut u32 {
            self.elements.entry(element).or_insert(0)
        }

        pub(crate) fn amount_iter(&self) -> impl Iterator<Item = (&T, &u32)> {
            self.elements
                .iter()
                .filter_map(|(k, v)| if *v > 0 { Some((k, v)) } else { None })
        }

        pub(crate) fn amount_iter_mut(&mut self) -> impl Iterator<Item = (&T, &mut u32)> {
            self.elements
                .iter_mut()
                .filter_map(|(k, v)| if *v > 0 { Some((k, v)) } else { None })
        }

        pub(crate) fn into_amount_iter(self) -> impl Iterator<Item = (T, u32)> {
            self.elements.into_iter()
        }

        pub(crate) fn is_multisubset_of(&self, other: &MultisetHashMap<T>) -> bool {
            self.support().all(|k| self.amount(k) <= other.amount(k))
        }

        #[allow(dead_code)]
        #[cfg(any(target_pointer_width = "32", target_pointer_width = "64"))]
        pub(crate) fn iter(&self) -> impl Iterator<Item = &T> {
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

        pub(crate) fn contains(&self, element: &T) -> bool {
            match self.elements.get(element) {
                Some(value) => value > &0,
                None => false,
            }
        }

        #[allow(dead_code)]
        pub(crate) fn remove_all(&mut self, element: &T) {
            self.elements.remove(element);
        }
    }

    impl<T> Default for MultisetHashMap<T>
    where
        T: Eq + Hash,
    {
        fn default() -> Self {
            Self::new()
        }
    }

    impl<T> MultisetHashMap<T>
    where
        T: Eq + Hash + Clone,
    {
        #[cfg(any(target_pointer_width = "32", target_pointer_width = "64"))]
        pub(crate) fn into_iter(self) -> impl Iterator<Item = T> {
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

    impl<T> Extend<(T, u32)> for MultisetHashMap<T>
    where
        T: Eq + Hash,
    {
        fn extend<U: IntoIterator<Item = (T, u32)>>(&mut self, iter: U) {
            for (k, v) in iter {
                *self.amount_mut(k) += v;
            }
        }
    }

    #[cfg(test)]
    mod test {
        use super::*;

        #[test]
        fn multiset_extend() {
            let mut left = MultisetHashMap::from_iter(vec![(0, 2), (1, 1)]);
            let right = MultisetHashMap::<u32>::from_iter(vec![(1, 1), (2, 2)]);
            left.extend(right.into_amount_iter());

            assert_eq!(
                left,
                MultisetHashMap::from_iter(vec![(0, 2), (1, 2), (2, 2)])
            );
        }

        #[test]
        fn multiset_from_iter() {
            let multiset = MultisetHashMap::from_iter(vec![(0, 1), (1, 2), (2, 0), (0, 2)]);

            assert_eq!(multiset.amount(&0), 3);
            assert_eq!(multiset.amount(&1), 2);
            assert_eq!(multiset.amount(&2), 0);
            assert_eq!(multiset.amount(&3), 0);
        }

        #[test]
        fn multiset_eq() {
            let left = MultisetHashMap::from_iter(vec![(0, 1), (1, 2), (2, 0)]);
            let right = MultisetHashMap::from_iter(vec![(0, 1), (1, 2)]);

            assert_eq!(left, right);
        }

        #[test]
        fn multiset_eq_for_empty() {
            let left = MultisetHashMap::new();
            let right = MultisetHashMap::from_iter(vec![(0, 0)]);

            assert_eq!(left, right);
        }

        #[test]
        fn multiset_iter() {
            let multiset = MultisetHashMap::from_iter(vec![(0, 1), (1, 2), (2, 3)]);
            let mut elements: Vec<u32> = multiset.iter().copied().collect();
            elements.sort();
            assert_eq!(elements, vec![0, 1, 1, 2, 2, 2])
        }
    }
}
