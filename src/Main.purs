module Main where

import Prelude (Unit, discard, map, pure, show, ($), (+), (*), (>>=))

import Data.Foldable (foldl, foldr, foldMap)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..))
import Data.Semigroup (class Semigroup, (<>))
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Effect.Console (log, logShow)

import Data.Tree (Tree, toArray, mkTree, invert, insert, remove, search)

-- | Need to wrap Int type to get semigroup.
newtype MyInt = MyInt Int
instance semigroupMyInt :: Semigroup MyInt where
  append (MyInt x) (MyInt y) = MyInt (x + y)

-- | Define combining two integer trees as the sum of each node.
nodeSum :: Tree Int -> Tree Int -> Tree Int
nodeSum tree1 tree2 =
  map unwrap (t1 <> t2)
  where
  t1 = map MyInt tree1
  t2 = map MyInt tree2
  unwrap = \(MyInt i) -> i

-- | Convert a node from int to string
tenStr :: Int -> Tree String
tenStr i = mkTree [ show (i*10) ]

-- | Create trees and invert
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
  log "Functor(map) + Semigroup"
  logShow tree
  logShow inverted
  logShow $ nodeSum tree inverted
  log "Fold Left"
  logShow $ foldl (\acc a -> acc <> (show a)) "" tree
  log "Fold Right"
  logShow $ foldr (\a acc -> acc <> (show a)) "" tree
  log "Fold Map"
  logShow $ foldMap (\i -> [i*2]) tree
  log "Bind"
  logShow $ tree >>= tenStr
  log "Traverse"
  logShow $ traverse fromNumber (mkTree [2.0, 1.0, 3.0])
  logShow $ traverse fromNumber (mkTree [2.0, 1.5, 3.0])
  log "Sequence"
  logShow $ sequence (mkTree [ Just 2, Just 1, Just 3 ])
  logShow $ sequence (mkTree [ Just 2, Nothing, Just 3 ])
  log "pure"
  pure tree >>= logShow
  logShow tree

