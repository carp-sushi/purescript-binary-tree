module Test.Main where

import Prelude
  ( Unit
  , apply
  , discard
  , flip
  , identity
  , map
  , pure
  , show
  , (+)
  , (*)
  , ($)
  , (<>)
  , (<$>)
  , (<*>)
  , (>>=)
  , (<<<)
  )

import Data.Foldable (foldl, foldr, foldMap)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, sequence)
import Effect (Effect)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

-- Under test
import Data.Tree
  ( Tree(..)
  , insert
  , invert
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
leaf a = pure a

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

-- Run the unit tests
main :: Effect Unit
main =
  runTest do
    treeTests
    functorLaws
    applyLaws
    applicativeLaws
    bindLaws
    monadLaws
    foldableTests
    traversableTests

-- Tests binary tree operations
treeTests :: TestSuite
treeTests = do
  suite "Basic tree operations" do
    test "constructor" do
      Assert.equal tree (mkTree numbers)
    test "insert" do
      Assert.equal tree2 (insert 10 tree)
      Assert.equal (mkTree [ 6, 5, 7 ]) (mkTree [ 6, 5, 7, 6, 5, 7, 6, 5, 7 ])
      Assert.equal tree (insert 2 tree)
    test "search" do
      Assert.equal (Branch 7 (leaf 6) (leaf 9)) (search 7 tree)
      Assert.equal Nil (search 99 tree)
    test "remove" do
      Assert.equal removed7 (remove 7 tree)
      Assert.equal removed4 (remove 4 tree)
      Assert.equal tree (remove 99 tree)
    test "min" do
      Assert.equal (Just 1) (min tree)
      Assert.equal Nothing (min nilTree)
    test "max" do
      Assert.equal (Just 9) (max tree)
      Assert.equal Nothing (max nilTree)
    test "invert" do
      Assert.equal inverted (invert tree)
    test "to array" do
      Assert.equal numbers (toArray tree)

-- Tests for functor laws
functorLaws :: TestSuite
functorLaws = do
  suite "functor laws" do
    let
      f = (_ + 1)
      g = (_ * 2)
      h = tree
    test "identity" do
      Assert.equal (identity h) (map identity h)
    test "composition" do
      Assert.equal (map (f <<< g) h) ((map f <<< map g) h)

-- Tests for apply laws
applyLaws :: TestSuite
applyLaws = do
  suite "apply laws" do
    test "associative composition" do
      let
        f = pure (_ + 1)
        g = pure (_ * 2)
        h = tree
      Assert.equal ((<<<) <$> f <*> g <*> h) (f <*> (g <*> h))

-- Tests for apply laws
applicativeLaws :: TestSuite
applicativeLaws = do
  suite "applicative laws" do
    test "identity" do
      let v = tree
      Assert.equal ((pure identity) <*> v) v
    test "composition" do
      let
        f = pure (_ + 1)
        g = pure (_ * 2)
        h = tree
      Assert.equal (pure (<<<) <*> f <*> g <*> h) (f <*> (g <*> h))
    test "homomorphism" do
      let
        f = toArray
        puref = (pure f) :: Tree (Tree Int -> Array Int)
        x = tree
      Assert.equal (puref <*> (pure x)) ((pure (f x)) :: Tree (Array Int))
    test "interchange" do
      let
        u = (pure toArray) :: Tree (Tree Int -> Array Int)
        y = tree
      Assert.equal (u <*> (pure y)) ((pure (_ $ y)) <*> u)

-- Tests for bind laws
bindLaws :: TestSuite
bindLaws = do
  suite "bind laws" do
    test "associativity" do
      let
        x = tree
        f = \i -> pure (i + 1)
        g = \i -> pure (i * 2)
      Assert.equal ((x >>= f) >>= g) (x >>= (\k -> f k >>= g))
    test "apply superclass" do
      let
        f = pure (_ + 1)
        x = tree
      Assert.equal (apply f x) (f >>= \f' -> map f' x)

-- Tests for monad laws
monadLaws :: TestSuite
monadLaws = do
  suite "monad laws" do
    let
      x = tree
      f = toArray
    test "left identity" do
      Assert.equal (pure x >>= f) (f x)
    test "right identity" do
      Assert.equal (x >>= pure) x

-- Tests for foldable
foldableTests :: TestSuite
foldableTests = do
  suite "foldable tests" do
    let accShow = \acc x -> acc <> show x
    test "fold left" do
      Assert.equal "1234679" (foldl accShow "" tree)
    test "fold right" do
      Assert.equal "9764321" (foldr (flip accShow) "" tree)
    test "fold map" do
      Assert.equal ([ 2, 4, 6, 8, 12, 14, 18 ]) (foldMap (\x -> [ 2 * x ]) tree)

-- Tests for traversable.
-- TODO: What is runConst?
--   let f = \i -> 2 * i
--   Assert.equal (foldMap f tree) ((runConst <<< traverse (Const <<< f)) tree)
traversableTests :: TestSuite
traversableTests = do
  suite "traversable tests" do
    let
      t1 = mkTree [ 2.0, 1.0, 3.0 ]
      t2 = mkTree [ Just 2, Just 1, Just 3 ]
      expected = Just $ mkTree [ 2, 1, 3 ] -- expected result
    test "traverse" do
      Assert.equal expected (traverse fromNumber t1)
      Assert.equal Nothing (traverse fromNumber (mkTree [ 2.0, 1.5, 3.0 ]))
    test "sequence" do
      Assert.equal expected (sequence t2)
      Assert.equal Nothing (sequence (mkTree [ Just 2, Nothing, Just 3 ]))
    test "compatibility" do
      Assert.equal (traverse fromNumber t1) (sequence (fromNumber <$> t1))
      Assert.equal (sequence t2) (traverse identity t2)

