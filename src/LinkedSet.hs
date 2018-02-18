module LinkedSet(LinkedSet, member, index, appendOrNothing, appendOrSame, empty) where

import qualified Data.Map as M
import Common.Operators
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, isJust)

data LinkedSet a = LinkedSet (M.Map a Int) [a]

member :: Ord a => a -> LinkedSet a -> Bool
member = index ..> isJust

index :: Ord a => a -> LinkedSet a -> Maybe Int
index x (LinkedSet map _) = M.lookup x map

appendOrNothing :: Ord a => a -> LinkedSet a -> Maybe (LinkedSet a)
appendOrNothing x ls@(LinkedSet map xs) =
  if ls |> member x then Nothing else Just $ LinkedSet (map |> M.insert x (M.size map + 1)) (x : xs)

appendOrSame :: Ord a => a -> LinkedSet a -> LinkedSet a
appendOrSame x ls = fromMaybe ls $ appendOrNothing x ls

empty :: Ord a => LinkedSet a
empty = LinkedSet M.empty []

instance Foldable LinkedSet where
  foldMap f (LinkedSet _ xs) = foldMap f $ reverse xs

instance Show a => Show (LinkedSet a) where
  show (LinkedSet _ xs) = show $ toList xs
