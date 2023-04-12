use std::array;
use std::borrow::{Borrow, BorrowMut};
use std::iter;
use std::ops::{Deref, DerefMut};
use std::{mem, slice};

use crate::count_repetition;

/// Selects a child [`Element`] within an `Element`. The selector must match the parent
/// type.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum ElementSelector {
    /// Selects between the true and false paths of a [`Branch`].
    Branch(bool),
    /// Indexes into the children of a [`Block`].
    Block(usize),
}

impl ElementSelector {
    /// Get this [`ElementSelector`] as an [`ElementPath`] slice.
    #[inline]
    pub fn as_path(&self) -> &ElementPath {
        ElementPath::from_slice(slice::from_ref(self))
    }

    /// Get this [`ElementSelector`] as a mut [`ElementPath`] slice.
    #[inline]
    pub fn as_mut_path(&mut self) -> &mut ElementPath {
        ElementPath::from_mut_slice(slice::from_mut(self))
    }
}

impl From<bool> for ElementSelector {
    #[inline]
    fn from(cond: bool) -> Self {
        Self::Branch(cond)
    }
}

impl From<usize> for ElementSelector {
    #[inline]
    fn from(index: usize) -> Self {
        Self::Block(index)
    }
}

impl Borrow<ElementPath> for ElementSelector {
    #[inline]
    fn borrow(&self) -> &ElementPath {
        self.as_path()
    }
}

impl BorrowMut<ElementPath> for ElementSelector {
    #[inline]
    fn borrow_mut(&mut self) -> &mut ElementPath {
        self.as_mut_path()
    }
}

impl ElementIndex for ElementSelector {
    type PathSelectors = iter::Once<Self>;

    #[inline]
    fn path_selectors(self) -> Self::PathSelectors {
        iter::once(self)
    }
}

/// A path to a particular child [`Element`].
#[derive(Debug, Default, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(transparent)]
pub struct ElementPathBuf {
    /// The sequence of selectors that lead to the element.
    path: Vec<ElementSelector>,
}

impl ElementPathBuf {
    /// Create a new empty element path buf.
    #[inline]
    pub fn new() -> Self {
        Default::default()
    }

    /// Create a new selector by copying the given path.
    pub fn from_path(path: &ElementPath) -> Self {
        Self {
            path: path.as_slice().to_vec(),
        }
    }

    /// Get this [`ElementPathBuf`] as an [`ElementPath`] slice.
    #[inline]
    pub fn as_path(&self) -> &ElementPath {
        ElementPath::from_slice(&self.path)
    }

    /// Get this [`ElementPathBuf`] as a mut [`ElementPath`] slice.
    #[inline]
    pub fn as_mut_path(&mut self) -> &mut ElementPath {
        ElementPath::from_mut_slice(&mut self.path)
    }

    /// Push the given selector onto the end of this path.
    #[inline]
    pub fn push<S>(&mut self, selector: S)
    where
        S: Into<ElementSelector>,
    {
        self.path.push(selector.into());
    }

    /// Append another [`ElementIndex`] to the end of this path.
    #[inline]
    pub fn append<P>(&mut self, path: P)
    where
        P: ElementIndex,
    {
        self.path.extend(path.path_selectors())
    }
}

impl Deref for ElementPathBuf {
    type Target = ElementPath;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.as_path()
    }
}

impl DerefMut for ElementPathBuf {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut_path()
    }
}

impl Borrow<ElementPath> for ElementPathBuf {
    #[inline]
    fn borrow(&self) -> &ElementPath {
        self.as_path()
    }
}

impl BorrowMut<ElementPath> for ElementPathBuf {
    #[inline]
    fn borrow_mut(&mut self) -> &mut ElementPath {
        self.as_mut_path()
    }
}

impl AsRef<ElementPath> for ElementPathBuf {
    #[inline]
    fn as_ref(&self) -> &ElementPath {
        self.as_path()
    }
}

impl AsMut<ElementPath> for ElementPathBuf {
    #[inline]
    fn as_mut(&mut self) -> &mut ElementPath {
        self.as_mut_path()
    }
}

impl AsRef<[ElementSelector]> for ElementPathBuf {
    #[inline]
    fn as_ref(&self) -> &[ElementSelector] {
        self.as_slice()
    }
}

impl AsMut<[ElementSelector]> for ElementPathBuf {
    #[inline]
    fn as_mut(&mut self) -> &mut [ElementSelector] {
        self.as_mut_slice()
    }
}

impl IntoIterator for ElementPathBuf {
    type IntoIter = std::vec::IntoIter<ElementSelector>;
    type Item = ElementSelector;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.path.into_iter()
    }
}

impl<'p> IntoIterator for &'p ElementPathBuf {
    type IntoIter = slice::Iter<'p, ElementSelector>;
    type Item = &'p ElementSelector;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'p> IntoIterator for &'p mut ElementPathBuf {
    type IntoIter = slice::IterMut<'p, ElementSelector>;
    type Item = &'p mut ElementSelector;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

impl PartialEq<ElementPath> for ElementPathBuf {
    #[inline]
    fn eq(&self, other: &ElementPath) -> bool {
        self.as_path() == other
    }
}

impl PartialEq<&ElementPath> for ElementPathBuf {
    #[inline]
    fn eq(&self, other: &&ElementPath) -> bool {
        self.as_path() == *other
    }
}

impl PartialEq<&mut ElementPath> for ElementPathBuf {
    #[inline]
    fn eq(&self, other: &&mut ElementPath) -> bool {
        self.as_path() == *other
    }
}

impl<'p> ElementIndex for ElementPathBuf {
    type PathSelectors = std::vec::IntoIter<ElementSelector>;

    #[inline]
    fn path_selectors(self) -> Self::PathSelectors {
        self.into_iter()
    }
}

impl<'p> ElementIndex for &'p ElementPathBuf {
    type PathSelectors = iter::Copied<slice::Iter<'p, ElementSelector>>;

    #[inline]
    fn path_selectors(self) -> Self::PathSelectors {
        self.iter().copied()
    }
}

impl<'p> ElementIndex for &'p mut ElementPathBuf {
    type PathSelectors = iter::Copied<slice::Iter<'p, ElementSelector>>;

    #[inline]
    fn path_selectors(self) -> Self::PathSelectors {
        self.iter().copied()
    }
}

/// A path to a particular child [`Element`].
#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(transparent)]
pub struct ElementPath {
    path: [ElementSelector],
}

impl ElementPath {
    /// Create an [`ElementPath`] ref from a slice of [`ElementSelector`].
    #[inline]
    pub fn from_slice<'p>(slice: &'p [ElementSelector]) -> &'p Self {
        // Safety: ElementPath is repr(transparent) to [ElementSelector], so transmuting
        // from &[ElementSelector] to &ElementPath is safe.
        unsafe { mem::transmute(slice) }
    }

    /// Create a mut [`ElementPath`] ref from a mut slice of [`ElementSelector`].
    #[inline]
    pub fn from_mut_slice<'p>(slice: &'p mut [ElementSelector]) -> &'p mut Self {
        // Safety: ElementPath is repr(transparent) to [ElementSelector], so transmuting
        // from &mut [ElementSelector] to &mut ElementPath is safe.
        unsafe { mem::transmute(slice) }
    }

    /// Creates an [`ElementPathBuf`] from this [`ElementPath`].
    #[inline]
    pub fn to_buf(&self) -> ElementPathBuf {
        ElementPathBuf::from_path(self)
    }

    /// Get this [`ElementPath`] as a slice.
    #[inline]
    pub fn as_slice(&self) -> &[ElementSelector] {
        &self.path
    }

    /// Get this [`ElementPath`] as a mutable slice.
    #[inline]
    pub fn as_mut_slice(&mut self) -> &mut [ElementSelector] {
        &mut self.path
    }

    /// Get an iterator over the selectors in this path.
    #[inline]
    pub fn iter(&self) -> slice::Iter<ElementSelector> {
        self.as_slice().iter()
    }

    /// Get an iterator over mutable selectors in this path.
    #[inline]
    pub fn iter_mut(&mut self) -> slice::IterMut<ElementSelector> {
        self.as_mut_slice().iter_mut()
    }

    /// Get the parent of this [`ElementPath`] if any. Returns `None` for the root (empty)
    /// path.
    pub fn parent(&self) -> Option<&ElementPath> {
        self.path
            .split_last()
            .map(|(_, head)| ElementPath::from_slice(head))
    }
}

impl AsRef<[ElementSelector]> for ElementPath {
    #[inline]
    fn as_ref(&self) -> &[ElementSelector] {
        self.as_slice()
    }
}

impl AsMut<[ElementSelector]> for ElementPath {
    #[inline]
    fn as_mut(&mut self) -> &mut [ElementSelector] {
        self.as_mut_slice()
    }
}

impl ToOwned for ElementPath {
    type Owned = ElementPathBuf;

    #[inline]
    fn to_owned(&self) -> Self::Owned {
        self.to_buf()
    }
}

impl<'p> IntoIterator for &'p ElementPath {
    type IntoIter = slice::Iter<'p, ElementSelector>;
    type Item = &'p ElementSelector;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'p> IntoIterator for &'p mut ElementPath {
    type IntoIter = slice::IterMut<'p, ElementSelector>;
    type Item = &'p mut ElementSelector;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

impl<'p> ElementIndex for &'p ElementPath {
    type PathSelectors = iter::Copied<slice::Iter<'p, ElementSelector>>;

    #[inline]
    fn path_selectors(self) -> Self::PathSelectors {
        self.iter().copied()
    }
}

impl<'p> ElementIndex for &'p mut ElementPath {
    type PathSelectors = iter::Copied<slice::Iter<'p, ElementSelector>>;

    #[inline]
    fn path_selectors(self) -> Self::PathSelectors {
        self.iter().copied()
    }
}

impl PartialEq<ElementPathBuf> for ElementPath {
    #[inline]
    fn eq(&self, other: &ElementPathBuf) -> bool {
        self == other.as_path()
    }
}

impl PartialEq<ElementPathBuf> for &ElementPath {
    #[inline]
    fn eq(&self, other: &ElementPathBuf) -> bool {
        *self == other.as_path()
    }
}

impl PartialEq<ElementPathBuf> for &mut ElementPath {
    #[inline]
    fn eq(&self, other: &ElementPathBuf) -> bool {
        *self == other.as_path()
    }
}

/// Trait for types which can be used to index into an [`Element`].
pub trait ElementIndex {
    type PathSelectors: Iterator<Item = ElementSelector>
        + DoubleEndedIterator
        + ExactSizeIterator
        + iter::FusedIterator;

    fn path_selectors(self) -> Self::PathSelectors;
}

impl ElementIndex for bool {
    type PathSelectors = iter::Once<ElementSelector>;

    #[inline]
    fn path_selectors(self) -> Self::PathSelectors {
        ElementSelector::from(self).path_selectors()
    }
}

impl ElementIndex for usize {
    type PathSelectors = iter::Once<ElementSelector>;

    #[inline]
    fn path_selectors(self) -> Self::PathSelectors {
        ElementSelector::from(self).path_selectors()
    }
}

macro_rules! tuple_element_index {
    ($($t:ident),* $(,)?) => {
        impl<$($t),*> ElementIndex for ($($t,)*)
        where $($t: Into<ElementSelector>,)*
        {
            type PathSelectors = array::IntoIter<ElementSelector, {count_repetition!($($t,)*)}>;

            fn path_selectors(self) -> Self::PathSelectors {
                #[allow(non_snake_case)]
                let ($($t,)*) = self;
                [$($t.into(),)*].into_iter()
            }
        }
    };
}

tuple_element_index!();
tuple_element_index!(A);
tuple_element_index!(A, B);
tuple_element_index!(A, B, C);
tuple_element_index!(A, B, C, D);
tuple_element_index!(A, B, C, D, E);
tuple_element_index!(A, B, C, D, E, F);
tuple_element_index!(A, B, C, D, E, F, G);
tuple_element_index!(A, B, C, D, E, F, G, H);
tuple_element_index!(A, B, C, D, E, F, G, H, I);
tuple_element_index!(A, B, C, D, E, F, G, H, I, J);
