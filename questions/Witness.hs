module Witness where

import Common.Lists
import Common.Operators
import Data.Foldable (toList)
import Data.Maybe
import qualified UnionFind as UF
import Data.Tuple (swap)
import qualified Data.Set as S
import Debug.Trace
import LinkedSet
import Prelude hiding (Left, Right, and, or)

data Coordinate = Coordinate Int Int deriving (Show, Eq, Ord)
isTop :: Coordinate -> Grid -> Bool
isTop (Coordinate _ y) (Grid size) = y == size
isRight :: Coordinate -> Grid -> Bool
isRight (Coordinate x _) (Grid size) = x == size

-- A square is define by its bottom left coordinate
newtype Square = Square Coordinate deriving (Show, Eq, Ord)
connectingCoordinates :: Square -> Square -> Maybe (Coordinate, Coordinate)
connectingCoordinates (Square (Coordinate x1 y1)) (Square (Coordinate x2 y2))
    | x1 == x2 && adj y1 y2 = let y = max y1 y2 in Just (Coordinate x1 y, Coordinate (x1 + 1) y)
    | adj x1 x2 && y1 == y2 = let x = max x1 x2 in Just (Coordinate x y1, Coordinate x (y1 + 1))
    | otherwise = Nothing where
  adj :: Int -> Int -> Bool
  adj x y = abs (x - y) == 1

unsafeDescribe :: Show a => String -> a -> a
unsafeDescribe name x = trace (name ++ ": " ++ show x) x

newtype Grid = Grid Int

class GridElement a where
  top :: a -> a
  left :: a -> a
  bottom :: a -> a
  right :: a -> a
  isValid :: a -> Grid -> Bool
  neighbors :: a -> Grid -> [a]
  neighbors x g = filter (`isValid` g) [top x, left x, bottom x, right x]

instance GridElement Coordinate where
  top (Coordinate x y) = Coordinate x (y + 1)
  left (Coordinate x y) = Coordinate (x - 1) y
  bottom (Coordinate x y) = Coordinate x (y - 1)
  right (Coordinate x y) = Coordinate (x + 1) y
  isValid (Coordinate x y) (Grid size) = x >= 0 && y >= 0 && x <= size && y <= size

instance GridElement Square where
  top (Square c) = Square $ top c
  left (Square c) = Square $ left c
  bottom (Square c) = Square $ bottom c
  right (Square c) = Square $ right c
  isValid (Square c) g@(Grid size) = isValid c g && not (isTop c g) && not (isRight c g)

coordinates :: Grid -> [Coordinate]
coordinates (Grid size) = [Coordinate x y | x <- [0 .. size], y <- [0 .. size]]

foo :: Int -> Int -> Int
foo = (+) ..> (+ 4)


-- TODO move to common
and :: (a -> Bool) -> (a -> Bool) -> a -> Bool
and f g x = f x && g x

or :: (a -> Bool) -> (a -> Bool) -> a -> Bool
or f g x = f x || g x

squares :: Grid -> [Square]
squares g = filterNot ((`isTop` g) `or` (`isRight` g)) (coordinates g) <$$> Square

type Path = [Coordinate]
coordinatesAsTree :: Path -> S.Set (Coordinate, Coordinate)
coordinatesAsTree = pairWindows .> S.fromList
uniquePaths :: Coordinate -> Coordinate -> Grid -> [Path]
uniquePaths startingPoint goal grid = aux empty startingPoint where
  aux :: LinkedSet Coordinate -> Coordinate -> [Path]
  aux ls current = let
      visitedWithCurrent = ls |> appendOrNothing current
      newNeighbors = grid |> neighbors current
    in if current == goal then [toList ls] else case visitedWithCurrent of
      Nothing -> []
      Just newVisited -> concatMap (aux newVisited) newNeighbors

uniqueMoves :: Coordinate -> Coordinate -> Grid -> Int
uniqueMoves startingPoint goal = length . uniquePaths startingPoint goal

type Cluster = [Square]
clusters :: Path -> Grid -> [Cluster]
clusters path grid = buildUnions UF.empty (squares grid) |> UF.unions where
  coordinateSet = coordinatesAsTree path
  hasEdge :: (Coordinate, Coordinate) -> Bool
  hasEdge cs = [cs, swap cs] |> any (`S.member` coordinateSet)
  sameCluster :: Square -> Square -> Bool
  sameCluster = connectingCoordinates ..> fmap (not . hasEdge) ..> fromJust
  -- TODO move to common
  joinAll :: Ord a => a -> UF.UnionFind a -> [a] -> UF.UnionFind a
  joinAll = foldr . UF.join

  buildUnions :: UF.UnionFind Square -> [Square] -> UF.UnionFind Square
  buildUnions uf [] = uf
  buildUnions uf (s:ss) = let
      squaresInSameClusters = neighbors s grid |> filter (sameCluster s) :: [Square]
      updatedUf = joinAll s (uf |> UF.add s) squaresInSameClusters
    in buildUnions updatedUf ss

main = do
  let grid = Grid 2
--   print $ squares grid
  let paths = grid |> uniquePaths (Coordinate 0 0) (Coordinate 2 2)
  let cs = map (`clusters` grid) paths
  print $ paths <$- (`clusters` grid) |> nth 3
