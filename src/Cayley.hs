{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cayley where

import Common.Operators
import Common.Foldables (headOpt)
import Common.Bools as B

import UnionFind as UF

import Data.List (findIndex, find, sort, minimum)
import Data.Foldable (any)
import Data.Maybe(fromJust)
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad (guard, unless)

import Text.Format
import Control.Exception.Base (assert)
import Control.Monad.Except (throwError)

{-# ANN module ("HLint: ignore Use head"::String) #-}

data PruferSequence a = PruferSequence {
    code :: [a]
  , alphabet :: Set a
} deriving (Show)


validateGenWord :: (Ord a, Show a) => [a] -> Set a -> Either String (PruferSequence a)
validateGenWord xs alpha = do
  let sizeError =
       format "expected alphabet size to be 2 + word size, but |word| = {0}, |alphabet| = {1}"
           [show $ length xs, show $ S.size alpha]
  unless (S.size alpha == (2 + length xs)) (throwError sizeError)
  let asSet = S.fromList xs
  let subsetError =
       format "word ({0}) wasn't a subset of the alphabet ({1})" [show asSet, show alpha]
  unless (asSet `S.isProperSubsetOf` alpha) (throwError subsetError)
  return $ PruferSequence xs alpha

orRaise :: String -> Maybe a -> a
orRaise err Nothing = error err
orRaise _ (Just x) = x

data Edge a = Edge { src :: a, dst :: a } deriving (Show)
srcOrDst x (Edge a b) = x == a || x == b

canonical (Edge x y) = if x < y then Edge x y else Edge y x
instance Eq a => Eq (Edge a) where
  (Edge x1 y1) == (Edge x2 y2) = x1 == x2 && y1 == y2 || x1 == y2 && y1 == x2

instance Ord a => Ord (Edge a) where
  compare e1 e2 = let
      (Edge x1 y1) = canonical e1
      (Edge x2 y2) = canonical e2
    in if | x1 < x2 -> LT
          | x1 > x2 -> GT
          | otherwise -> compare y1 y2

data Tree a = Tree { unTree :: Set (Edge a)} | EmptyTree | SingletonTree a deriving (Eq, Show)

edges :: Tree a -> Set (Edge a)
edges (Tree x) = x
edges _ = S.empty

vertices :: Ord a => Tree a -> Set a
vertices EmptyTree = S.empty
vertices (SingletonTree x) = S.singleton x
vertices t = let es = edges t in S.map src es `S.union` S.map dst es

degree :: Ord a => a -> Tree a -> Int
degree x = edges .> S.filter (srcOrDst x) .> S.size

-- No way to represent a SingletonTree as a set of edges, but why would you want to?
validateTree :: Ord a => Set (Edge a) -> Either String (Tree a)
validateTree edges = if
  | S.null edges -> Right EmptyTree
  | not isConnected -> Left "Set don't describe a connected graph!"
  | numberOfEdges /= numberOfVertices - 1 -> Left tooManyEdgesError
  | otherwise -> Right $ Tree edges where
    isConnected = 1 == length (UF.unions $ foldr joinEdges UF.empty edges) where
      joinEdges (Edge x y) = UF.join x y
    numberOfEdges = S.size edges
    numberOfVertices = let srcs = S.map src edges
                           dsts = S.map dst edges
                       in S.size $ S.union srcs dsts
    tooManyEdgesError = format "Too many edges; exptected {0} but was {1}" $
      map show [numberOfEdges, numberOfVertices - 1]

toTree :: Ord a => PruferSequence a -> Tree a
toTree = Tree . aux where
  nextVertex :: Ord a => PruferSequence a -> a
  nextVertex (PruferSequence code@(c:cs) remainingAlphabet) =
    S.findMin $ remainingAlphabet S.\\ S.fromList code
  nextSequence :: Ord a => PruferSequence a -> (a, PruferSequence a)
  nextSequence ps@(PruferSequence (_:cs) remainingAlphabet) = let
      nextV = nextVertex ps
      nextSet = S.delete nextV remainingAlphabet
    in (nextV, PruferSequence cs nextSet)
  aux (PruferSequence code remainingAlphabet) = case code of
    [] -> let
        asList = S.toList remainingAlphabet
        (v1, v2) = assert (length asList == 2) (asList !! 0, asList !! 1)
      in S.singleton $ canonical (Edge v1 v2)
    (c:cs) -> let
        nextFromSet = S.findMin $ remainingAlphabet S.\\ S.fromList code
        nextEdge = canonical $ Edge c nextFromSet
        nextRemaining = S.delete nextFromSet remainingAlphabet
        recursive = aux $ PruferSequence cs nextRemaining
      in S.insert nextEdge recursive

contains :: Ord a => a -> Tree a -> Bool
contains x (SingletonTree y) = x == y
contains x (Tree edges) = any (srcOrDst x) edges
contains _ _ = False

data DirectedTree a = DirectedTree a [DirectedTree a] | EmptyDirectedTree deriving (Eq, Show, Ord)
root :: DirectedTree a -> a
root (DirectedTree x _) = x

direct :: Ord a => a -> Tree a -> DirectedTree a
direct _ EmptyTree = EmptyDirectedTree
direct root (SingletonTree y) = DirectedTree y []
direct root tree@(Tree edges) = let
    nonRoot (Edge x y) = if x == root then y else assert (x /= root) x
    successors = S.toList edges |> filter (srcOrDst root) |> map nonRoot
    childrenTree = map (\c -> direct c (Tree $ S.delete (Edge c root) edges)) successors
  in assert (tree |> contains root) (DirectedTree root childrenTree)

