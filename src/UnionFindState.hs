{-# LANGUAGE ScopedTypeVariables #-}

module UnionFindState where

import Common.Operators
import Control.Monad.State
import Control.Monad
import Common.Maps (indexM)
import Data.Map as Map hiding (map, foldl', foldr, update)
import Prelude hiding (map)
import Data.Foldable (foldl')

newtype UnionFind a = UnionFind (Map a a)
map (UnionFind x) = x

update :: (Map a a -> Map a a) -> UnionFind a -> UnionFind a
update f = map .> f .> UnionFind

type UnionFindState a b = State (UnionFind a) b

empty :: UnionFindState a ()
empty = put $ UnionFind Map.empty

join :: Ord a => a -> a -> UnionFindState a ()
join x y = do
    add x
    add y
    repX <- representative x
    repY <- representative y
    inSameUnion <- sameUnion x y
    -- TODO this could be a bit more efficient: add repX and all its union to repY
    unless inSameUnion $ modify (update $ insert repX repY)

add :: Ord a => a -> UnionFindState a ()
add x = do
  map <- gets map
  unless (member x map) $ modify (update $ insert x x)

-- If an element isn't a member, returns the element itself.
representative :: Ord a => a -> UnionFindState a a
representative x = do
  map <- gets map
  let (res:tail) = aux map [x]
  let updatedMap = foldr (`insert` res) map tail
  return res where
    aux map xs@(x:_) =
      let value = findWithDefault x x map in if value == x then xs else aux map $ value:xs

sameUnion :: Ord a => a -> a -> UnionFindState a Bool
sameUnion x y = (==) <$> representative x <*> representative y

elements :: UnionFindState a [a]
elements = gets $ keys . map

unions :: Ord a => UnionFindState a [[a]]
unions = fmap elems $ elements >>= indexM representative (return . (: []))
