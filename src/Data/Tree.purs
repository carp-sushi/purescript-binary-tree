module Data.Tree
  ( Tree(..)
  , insert
  , insert'
  , invert
  , max
  , min
  , mkTree
  , remove
  , search
  , toArray
  , (:+)
  , (+:)
  ) where

import Prelude
  ( class Eq
  , class Ord
  , class Show
  , compare
  , flip
  , show
  , (==)
  , (<$>)
  , (<*>)
  )
import Control.Monad
  ( class Applicative
  , class Apply
  , class Bind
  , class Monad
  , apply
  , bind
  , pure
  )
import Data.Foldable (class Foldable, foldl, foldr)
import Data.Functor (class Functor)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Ordering (Ordering(..))
import Data.Semigroup (class Semigroup, (<>))
import Data.Traversable (class Traversable, traverse, sequence)

-- | A tree is either nil or a node with a value and two sub-trees. The left tree of a
-- | a node contains values less than its value; the right, greater. 
data Tree a
  = Nil
  | Node a (Tree a) (Tree a)

-- | Convert a tree to a string
instance showTree :: Show a => Show (Tree a) where
  show Nil = "Nil"
  show (Node x t1 t2) =
    "(Node "
      <> (show x)
      <> " "
      <> (show t1)
      <> " "
      <> (show t2)
      <> ")"

-- | Derive tree equality
derive instance eqTree :: Eq a => Eq (Tree a)

-- | Map a function over a tree
derive instance functorTree :: Functor Tree

-- | Combine two trees
instance semigroupTree :: Semigroup a => Semigroup (Tree a) where
  append Nil Nil = Nil
  append t1 Nil = t1
  append Nil t2 = t2
  append (Node x ta tb) (Node y tc td) =
    Node (x <> y) (ta <> tc) (tb <> td)

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
  foldr f acc (Node x t1 t2) = foldr f acc' t1
    where
    acc' = f x (foldr f acc t2)
  -- left fold
  foldl _ acc Nil = acc
  foldl f acc (Node x t1 t2) = foldl f acc' t2
    where
    acc' = f (foldl f acc t1) x

-- | Define apply for tree.
instance applyTree :: Apply Tree where
  apply Nil _ = Nil
  apply _ Nil = Nil
  apply ft@(Node f _ _) (Node x t1 t2) =
    Node (f x) (apply ft t1) (apply ft t2)

-- | Define applicative for tree
instance applicativeTree :: Applicative Tree where
  pure x = Node x Nil Nil

-- | Define bind on tree.
instance bindTree :: Bind Tree where
  bind Nil _ = Nil
  bind (Node x t1 t2) f = go (f x)
    where
    -- Unpack function result to process children
    go Nil = Nil
    go (Node y t1' t2') =
      Node y
        (t1' `orElse` (bind t1 f))
        (t2' `orElse` (bind t2 f))
    -- Determine which tree to use: choose first unless nil
    orElse Nil t = t
    orElse t _ = t

-- | Define monad on tree.
instance monadTree :: Monad Tree

-- | Define traversable on tree.
instance traversableTree :: Traversable Tree where
  -- traverse tree
  traverse _ Nil = pure Nil
  traverse f (Node x t1 t2) =
    Node <$> f x <*> traverse f t1 <*> traverse f t2
  -- sequence tree
  sequence Nil = pure Nil
  sequence (Node x t1 t2) =
    Node <$> x <*> sequence t1 <*> sequence t2

-- | Build a tree from a foldable type.
mkTree :: forall f a. Foldable f => Ord a => f a -> Tree a
mkTree =
  foldl (\t x -> t +: x) Nil

-- | Add an element to a tree
insert :: forall a. Ord a => a -> Tree a -> Tree a
insert x Nil = pure x
insert x n@(Node y t1 t2) =
  case (compare x y) of
    EQ -> n
    GT -> Node y t1 (insert x t2)
    LT -> Node y (insert x t1) t2

-- | Insert infix operator
infixr 5 insert as :+

-- | Add an element to a tree
insert' :: forall a. Ord a => Tree a -> a -> Tree a
insert' = flip insert

-- | Insert infix operator
infixl 5 insert' as +:

-- | Search for the sub-tree with the given root element
search :: forall a. Ord a => a -> Tree a -> Tree a
search _ Nil = Nil
search x n@(Node y t1 t2) =
  case (compare x y) of
    EQ -> n
    LT -> search x t1
    GT -> search x t2

-- | Remove an element from a tree.
remove :: forall a. Ord a => a -> Tree a -> Tree a
remove _ Nil = Nil
remove x (Node y t1 t2) =
  case (compare x y) of
    LT -> Node y (remove x t1) t2
    GT -> Node y t1 (remove x t2)
    EQ -> case (min t2) of -- find smallest value > x
      (Just z) -> Node z t1 (remove z t2)
      Nothing -> t1

-- | Find the deepest left value of a tree.
min :: forall a. Ord a => Tree a -> Maybe a
min Nil = Nothing
min (Node x t1 _) =
  if t1 == Nil then Just x
  else min t1

-- | Find the deepest right value of a tree.
max :: forall a. Ord a => Tree a -> Maybe a
max Nil = Nothing
max (Node x _ t2) =
  if t2 == Nil then Just x
  else max t2

-- | Invert a tree
invert :: forall a. Tree a -> Tree a
invert Nil = Nil
invert (Node x t1 t2) =
  Node x (invert t2) (invert t1)

-- | Create an array from a tree (depth first).
toArray :: forall a. Tree a -> Array a
toArray Nil = []
toArray (Node x t1 t2) =
  [ x ] <> (toArray t1) <> (toArray t2)

