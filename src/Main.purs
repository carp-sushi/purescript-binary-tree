module Main where

import Prelude (class Eq, Unit, discard, map, ($), (+), (*), (<<<))

import Data.Semigroup (class Semigroup, (<>))
import Data.Semiring (class Semiring)
import Effect (Effect)
import Effect.Console (log, logShow)

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
  , (:+)
  , (+:)
  )

-- | Need to wrap Int type to define append (ie Semigroup).
newtype MyInt = MyInt Int

-- | Defer to underlying Int for Semiring (to get addition).
derive newtype instance semiringMyInt :: Semiring MyInt

-- | Defer to underlying Int for Eq.
derive newtype instance eqMyInt :: Eq MyInt

-- | Define append as addition for MyInt.
instance semigroupMyInt :: Semigroup MyInt where
  append x y = x + y

-- | Add two Int trees.
treeSum :: Tree Int -> Tree Int -> Tree Int
treeSum tree1 tree2 =
  let
    t1 = map MyInt tree1
    t2 = map MyInt tree2
    unwrap = \(MyInt i) -> i
  in
    map unwrap (t1 <> t2)

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
  log ":+"
  logShow $ 9 :+ 6 :+ 7 :+ 3 :+ 1 :+ 2 :+ 4 :+ Nil
  log "+:"
  logShow $ Nil +: 4 +: 2 +: 1 +: 3 +: 7 +: 6 +: 9

  log "Inverted Tree"
  logShow inverted
  log "Inverted As Array"
  logShow $ toArray inverted

  log "Insert 5, 8"
  logShow $ (insert 8 <<< insert 5) tree
  log "Search 7"
  logShow $ search 7 tree
  log "Remove 7"
  logShow $ remove 7 tree

  log "Max"
  logShow $ max tree
  log "Min"
  logShow $ min tree

  log "Tree Sum (tree + inverted)"
  logShow $ treeSum tree inverted
  log "Tree x 2"
  logShow $ map (_*2) tree

