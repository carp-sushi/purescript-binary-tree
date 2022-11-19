module Test.Main where

import Prelude
  ( Unit
  , apply
  , discard
  , identity
  , map
  , show
  , (+)
  , (*)
  , (<>)
  , (<$>)
  , (<*>)
  , (>>=)
  , (<<<)
  )

import Control.Monad (pure)
import Data.Foldable (foldl, foldr, foldMap)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, sequence)
import Effect (Effect)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

-- Under test
import Data.Tree
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
  )

-- Input
numbers :: Array Int
numbers = [ 4, 2, 1, 3, 7, 6, 9 ]

-- Leaf helper function
leaf :: Int -> Tree Int
leaf a =
  Branch a Nil Nil

-- Expected tree
tree :: Tree Int
tree =
  ( Branch 4
      (Branch 2 (leaf 1) (leaf 3))
      (Branch 7 (leaf 6) (leaf 9))
  )

-- Expected tree with 10 added
tree2 :: Tree Int
tree2 =
  ( Branch 4
      (Branch 2 (leaf 1) (leaf 3))
      ( Branch 7
          (leaf 6)
          (Branch 9 Nil (leaf 10))
      )
  )

-- Expected inverted tree
inverted :: Tree Int
inverted =
  ( Branch 4
      (Branch 7 (leaf 9) (leaf 6))
      (Branch 2 (leaf 3) (leaf 1))
  )

-- Expected tree after removal of 7
removed7 :: Tree Int
removed7 =
  ( Branch 4
      (Branch 2 (leaf 1) (leaf 3))
      (Branch 9 (leaf 6) Nil)
  )

-- Expected tree after removal of 4 (the root)
removed4 :: Tree Int
removed4 =
  ( Branch 6
      (Branch 2 (leaf 1) (leaf 3))
      (Branch 7 Nil (leaf 9))
  )

-- An empty tree
nilTree :: Tree Int
nilTree =
  Nil

-- Expected tree from traverse and sequence tests
numTree :: Tree Int
numTree =
  (Branch 2 (leaf 1) (leaf 3))

-- Run the unit tests
main :: Effect Unit
main = runTest do
  suite "Tree tests" do
    test "to/from array" do
      Assert.equal tree (mkTree numbers)
      Assert.equal numbers (toArray tree)
    test "insert" do
      Assert.equal tree2 (insert 10 tree)
      Assert.equal (Branch 6 (leaf 5) (leaf 7)) (mkTree [ 6, 5, 5, 5, 5, 5, 5, 7, 5, 5 ])
    test "remove" do
      Assert.equal removed7 (remove 7 tree)
      Assert.equal removed4 (remove 4 tree)
      Assert.equal tree (remove 99 tree)
    test "max" do
      Assert.equal (Just 9) (max tree)
      Assert.equal Nothing (max nilTree)
    test "min" do
      Assert.equal (Just 1) (min tree)
      Assert.equal Nothing (min nilTree)
    test "contains" do
      Assert.equal true (contains 7 tree)
      Assert.equal false (contains 99 tree)
    test "isLeaf" do
      Assert.equal true (isLeaf (leaf 9))
      Assert.equal false (isLeaf tree)
      Assert.equal false (isLeaf nilTree)
    test "invert" do
      Assert.equal inverted (invert tree)
    test "search" do
      Assert.equal (Branch 7 (leaf 6) (leaf 9)) (search 7 tree)
      Assert.equal Nil (search 99 tree)
    test "functor" do
      let
        f = \i -> i + 1
        g = \i -> i * 2
        h = tree
      Assert.equal (identity h) (map identity h)
      Assert.equal (map (g <<< f) h) ((map g <<< map f) h)
    test "apply" do
      let
        f = Branch (_ + 1) Nil Nil
        g = Branch (_ * 2) Nil Nil
        h = tree
      Assert.equal ((<<<) <$> f <*> g <*> h) (f <*> (g <*> h))
    test "bind" do
      let
        f = \i -> mkTree [ i + 1 ]
        g = \i -> mkTree [ i * 2 ]
        x = tree
        f2 = Branch (_ + 1) Nil Nil
      Assert.equal ((x >>= f) >>= g) (x >>= (\k -> f k >>= g))
      Assert.equal (f2 >>= (\f' -> map f' x)) (apply f2 x)
    test "monad" do
      let
        x = tree
        f = toArray
      Assert.equal (pure x >>= f) (f x)
      Assert.equal (x >>= pure) x
    test "foldable" do
      Assert.equal "1234679" (foldl (\a x -> a <> show x) "" tree)
      Assert.equal "9764321" (foldr (\x a -> a <> show x) "" tree)
      Assert.equal [ 2, 4, 6, 8, 12, 14, 18 ] (foldMap (\x -> [ 2 * x ]) tree)
    test "traverse" do
      Assert.equal (Just numTree) (traverse fromNumber (mkTree [ 2.0, 1.0, 3.0 ]))
      Assert.equal Nothing (traverse fromNumber (mkTree [ 2.0, 1.5, 3.0 ]))
    test "sequence" do
      Assert.equal (Just numTree) (sequence (mkTree [ Just 2, Just 1, Just 3 ]))
      Assert.equal Nothing (sequence (mkTree [ Just 2, Nothing, Just 3 ]))
