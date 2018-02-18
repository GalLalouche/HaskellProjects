{-# LANGUAGE ScopedTypeVariables #-}

-- A not particularly effective implementation of the Union-Find data structure.
-- The only optimization (via path minimization) it does is when joining.
-- If you need a more optimized version for some reason, use UnionFindState.
module UnionFind(UnionFind, empty, join, add, sameUnion, elements, union, unions) where

import Common.Operators
import Common.Maps (index, upsert, closure)

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (find)
import Data.Foldable (toList)
import Data.Tuple (swap)
import Control.Monad (mfilter)

newtype UnionFind a = UnionFind (Map a a)

empty :: UnionFind a
empty = UnionFind M.empty

-- If x and y are already in the same equivalence set, does nothing
-- Otherwise, joins x and y whiles also shortening the paths.
join :: Ord a => a -> a -> UnionFind a -> UnionFind a
join x y uf = let
    -- Adds X and Y since otherwise if either aren't in, elements and fold will act funky.
    -- In other words, the invariant is that all elements in the UnionFind are also keys.
    withXAndY = uf |> add x |> add y
  in if withXAndY |> sameUnion x y then withXAndY
  else let
    (px, uf1) = repWithShortcuts x withXAndY
    (py, UnionFind updatedTo) = repWithShortcuts y uf1
  in UnionFind $ M.insert px py updatedTo

-- If x is already in, does nothing; otherwise, adds x with a pointer to itself.
add :: Ord a => a -> UnionFind a -> UnionFind a
add x uf@(UnionFind to) = case M.lookup x to of
  Just _ -> uf
  Nothing -> UnionFind $ M.insert x x to

-- Internal: returns the representative and an optimized data structure, where all elements in the
-- path from the input to the representative point directly to the representative.
repWithShortcuts :: Ord a => a -> UnionFind a -> (a, UnionFind a)
repWithShortcuts x (UnionFind to) = aux x [] where
  aux x xs = case M.lookup x to |> mfilter (/= x) of
    Nothing -> (x, insertAll x xs)
    Just p -> aux p $ p : xs
  insertAll key values = UnionFind $ foldr (M.insert key) to values

-- If an element isn't a member, returns the element itself.
representative :: Ord a => a -> UnionFind a -> a
representative = repWithShortcuts ..> fst

-- An element is always in the same union as itself, even if it has not been added explicitly as
-- a member.
sameUnion :: Ord a => a -> a -> UnionFind a -> Bool
sameUnion x y uf = let rep = (`representative` uf) in rep x == rep y

elements :: UnionFind a -> [a]
elements (UnionFind map) = M.keys map

-- All elements in the same union as x
union :: Ord a => a -> UnionFind a -> [a]
union x uf = unions uf |> find (elem x) |> fromMaybe [x]

unions :: Ord a => UnionFind a -> [[a]]
unions uf = uf |> elements |> index (`representative` uf) (: []) |> M.elems

instance (Show a, Ord a) => Show (UnionFind a) where
  show = show . unions
instance Foldable UnionFind where
  foldMap f uf = foldMap f $ elements uf


