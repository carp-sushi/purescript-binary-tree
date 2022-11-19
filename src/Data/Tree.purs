module Data.Tree
  ( Tree(..)
  , contains
  , insert
  , invert
  , isLeaf
  , max
  , min
  , mkTree
  , remove
  , search
  , toArray
  ) where

import Prelude
  ( class Eq
  , class Ord
  , class Show
  , compare
  , show
  , (==)
  , (&&)
  , (<$>)
  , (<*>)
  )
import Control.Monad (class Applicative, class Apply, class Bind, class Monad, ap, bind, pure)
import Data.Foldable (class Foldable, foldr, foldl)
import Data.Functor (class Functor, map)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Ordering (Ordering(..))
import Data.Semigroup (class Semigroup, (<>))
import Data.Traversable (class Traversable, traverse, sequence)

-- | A tree is either nil or a branch with a value and two sub-trees. The left tree of a
-- | a branch contains values less than its element. The right contains greater elements. 
data Tree a
  = Nil
  | Branch a (Tree a) (Tree a)

-- | Convert a tree to a string
instance showTree :: Show a => Show (Tree a) where
  show Nil = "Nil"
  show (Branch a t1 t2) =
    "(Branch "
      <> (show a)
      <> " "
      <> (show t1)
      <> " "
      <> (show t2)
      <> ")"

-- | Determine whether trees are equal
instance eqTree :: Eq a => Eq (Tree a) where
  eq Nil Nil = true
  eq _ Nil = false
  eq Nil _ = false
  eq (Branch x ta tb) (Branch y tc td) =
    (x == y) && (ta == tc) && (tb == td)

-- | Map a function over a tree
instance functorTree :: Functor Tree where
  map _ Nil = Nil
  map f (Branch x t1 t2) =
    Branch (f x) (map f t1) (map f t2)

-- | Combine two trees
instance semigroupTree :: Semigroup a => Semigroup (Tree a) where
  append Nil Nil = Nil
  append t1 Nil = t1
  append Nil t2 = t2
  append (Branch x ta tb) (Branch y tc td) =
    Branch (x <> y) (ta <> tc) (tb <> td)

-- | Define an empty tree
instance monoidTree :: Semigroup (Tree a) => Monoid (Tree a) where
  mempty = Nil

-- | Define how to fold over a tree.
instance foldableTree :: Foldable Tree where
  -- fold map
  foldMap f =
    foldl (\acc x -> acc <> (f x)) mempty
  -- right fold
  foldr _ acc Nil = acc
  foldr f acc (Branch x t1 t2) = foldr f acc' t1
    where
    acc' = f x (foldr f acc t2)
  -- left fold
  foldl _ acc Nil = acc
  foldl f acc (Branch x t1 t2) = foldl f acc' t2
    where
    acc' = f (foldl f acc t1) x

-- | Define apply for tree.
instance applyTree :: Apply Tree where
  apply = ap

-- | Define applicative for tree
instance applicativeTree :: Applicative Tree where
  pure a = Branch a Nil Nil

-- | Define bind on tree.
instance bindTree :: Bind Tree where
  bind Nil _ = Nil
  bind (Branch x t1 t2) f = go (f x)
    where
    go Nil = Nil
    go (Branch y _ _) =
      Branch y (bind t1 f) (bind t2 f)

-- | Define monad on tree.
instance monadTree :: Monad Tree

-- | Define traversable on tree.
instance traversableTree :: Traversable Tree where
  -- traverse tree
  traverse _ Nil = pure Nil
  traverse f (Branch x t1 t2) =
    Branch <$> f x <*> traverse f t1 <*> traverse f t2
  -- sequence tree
  sequence Nil = pure Nil
  sequence (Branch x t1 t2) =
    Branch <$> x <*> sequence t1 <*> sequence t2

-- | Build a tree from an array
mkTree :: forall a. Ord a => Array a -> Tree a
mkTree xs =
  foldl (\t x -> insert x t) Nil xs

-- | Search for the sub-tree with the given root element
search :: forall a. Ord a => a -> Tree a -> Tree a
search _ Nil = Nil
search x b@(Branch y t1 t2) =
  case (compare x y) of
    LT -> search x t1
    GT -> search x t2
    EQ -> b

-- | Add an element to a tree
insert :: forall a. Ord a => a -> Tree a -> Tree a
insert x Nil = Branch x Nil Nil
insert x b@(Branch y t1 t2) =
  case (compare x y) of
    GT -> Branch y t1 (insert x t2)
    LT -> Branch y (insert x t1) t2
    EQ -> b

-- | Remove an element from a tree.
remove :: forall a. Ord a => a -> Tree a -> Tree a
remove _ Nil = Nil
remove x (Branch y t1 t2) =
  case (compare x y) of
    LT -> Branch y (remove x t1) t2
    GT -> Branch y t1 (remove x t2)
    EQ -> case (min t2) of -- find smallest value > x
      Nothing -> t1
      (Just z) -> Branch z t1 (remove z t2)

-- | Find the deepest left value of a tree.
min :: forall a. Ord a => Tree a -> Maybe a
min Nil = Nothing
min (Branch x t1 _) =
  if t1 == Nil then (Just x) else min t1

-- | Find the deepest right value of a tree.
max :: forall a. Ord a => Tree a -> Maybe a
max Nil = Nothing
max (Branch x _ t2) =
  if t2 == Nil then (Just x) else max t2

-- | Invert a tree
invert :: forall a. Ord a => Tree a -> Tree a
invert Nil = Nil
invert (Branch a t1 t2) =
  Branch a (invert t2) (invert t1)

-- | Determine whether a tree contains an element.
contains :: forall a. Ord a => a -> Tree a -> Boolean
contains x t =
  case (search x t) of
    Nil -> false
    _ -> true

-- | Deterine whether a tree is a leaf - a branch with an element and no children.
isLeaf :: forall a. Ord a => Tree a -> Boolean
isLeaf Nil = false
isLeaf (Branch _ t1 t2) =
  t1 == Nil && t2 == Nil

-- | Create an array from a tree (depth first).
toArray :: forall a. Tree a -> Array a
toArray Nil = []
toArray (Branch x t1 t2) =
  [ x ] <> (toArray t1) <> (toArray t2)

