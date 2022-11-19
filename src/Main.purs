module Main where

import Prelude (Unit, discard, map, pure, ($), (+), (>>=))

import Data.Semigroup (class Semigroup, (<>))
import Effect (Effect)
import Effect.Console (log, logShow)

import Data.Tree (Tree, toArray, mkTree, invert, insert, remove, search)

-- | Need to wrap Int type to implement semigroup.
newtype MyInt = MyInt Int
instance semigroupMyInt :: Semigroup MyInt where
  append (MyInt x) (MyInt y) = MyInt (x + y)

-- | Add two Int trees.
treeSum :: Tree Int -> Tree Int -> Tree Int
treeSum tree1 tree2 =
  map unwrap (t1 <> t2)
  where
  t1 = map MyInt tree1
  t2 = map MyInt tree2
  unwrap = \(MyInt i) -> i

-- | Demonstrate some basic operations on trees
main :: Effect Unit
main = do
  let numbers = [ 4, 2, 1, 3, 7, 6, 9 ]
      tree = mkTree numbers
      inverted = invert tree
  log "Numbers"
  logShow numbers
  log "Tree"
  logShow tree
  log "Invert Tree"
  logShow inverted
  log "Inverted As Array"
  logShow $ toArray inverted
  log "Insert 5"
  logShow $ insert 5 tree
  log "Remove 7"
  logShow $ remove 7 tree
  log "Search 7"
  logShow $ search 7 tree
  log "Node Sum"
  logShow tree
  logShow inverted
  logShow $ treeSum tree inverted
  log "---"
  logShow $ map (_+1) tree
  logShow $ (pure (_+1) >>= \f -> map f tree)
