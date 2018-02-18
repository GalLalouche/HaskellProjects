{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.Foldable(toList, fold)
import Data.Map (Map)
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as S

import Control.Exception (assert)
import Control.Monad (forM_, forM, mapM, (>=>))
import Control.Monad.State (modify, execState, State)

import Common.Lists
import Common.Operators

import Arbitraries
import Cayley
import UnionFind as UF

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck as QC

main = defaultMain $ testGroup "tests" [props]

instance Arbitrary (UnionFind Int) where
  arbitrary = do
    n <- (`mod` 100) <$> arbitrary
    shuffledList <- shuffle [1 .. n]
    parts <- part shuffledList
    fold <$> mapM unionize parts
    where
      part :: [a] -> Gen [[a]]
      part [] = return []
      part [x] = return [[x]]
      part (x : xs) = do
        (h:t) <- part xs
        appendToPrev <- QC.elements [True, False]
        return $ if appendToPrev then (x:h) : t else [x]:h:t
      fromTreeToUnionFind :: Ord a => DirectedTree a -> UnionFind a
      fromTreeToUnionFind EmptyDirectedTree = UF.empty
      fromTreeToUnionFind dt = execState (aux dt) UF.empty where
        aux :: Ord a => DirectedTree a -> State (UnionFind a) ()
        aux (DirectedTree r cs) = do
          modify $ UF.add r
          forM_ cs aux
          let childrenRoots = map root cs
          forM_ childrenRoots (modify . join r)
      genWordFromSet :: Ord a => Set a -> Gen (PruferSequence a)
      genWordFromSet alphabet = do
        let asList = S.toList alphabet
        code <- vectorOf (S.size alphabet - 2) (QC.elements asList)
        return $ PruferSequence code alphabet
      genDirectedTreeFromWord :: Ord a => PruferSequence a -> Gen (DirectedTree a)
      genDirectedTreeFromWord code = do
        let tree = toTree code
        r <- QC.elements $ S.toList $ vertices tree
        return $ direct r tree
      unionize :: Ord a => [a] -> Gen (UnionFind a)
      unionize [x] = return $ UF.empty |> add x
      unionize xs = xs |> S.fromList |> (genWordFromSet >=> genDirectedTreeFromWord >$> fromTreeToUnionFind)
      fold = foldr aux UF.empty where
        aux next agg = let
            us = unions next
            u = assert (length us == 1) (head us)
            h = head u
          in foldr (`join` h) agg u

props = testGroup "(checked by QuickCheck)" [
    QC.testProperty "empty - same union is false if x /= y" $
      \x y -> x /=y ==> not $ sameUnion x y emptyUf
  , QC.testProperty "empty - same union is true if x == y" $
      \x -> sameUnion x x emptyUf
  , QC.testProperty "after join both elements are in the same union" $
      \x y -> emptyUf |> join x y |> sameUnion x y
  , QC.testProperty "unions of empty is empty" $
      unions emptyUf === []
  , QC.testProperty "unions of add is just the element added" $
      \x -> let actual = emptyUf |> add x |> unions in actual === [[x]]
  , QC.testProperty "unions of two add is just the elements added" $
      \x y -> x /= y ==> let actual = emptyUf |> add x |> add y |> unions
                         in deepSetSame actual [[x], [y]]
  , QC.testProperty "unions of join is its elements" $
      \x y -> x /= y ==> let actual = emptyUf |> join x y |> unions |> deepSet
                         in actual === deepSet [[x, y]]
  , QC.testProperty "union of empty is a singleton" $
      \x -> let u = emptyUf |> union x in u === [x]
  , QC.testProperty "union of single element is the single element" $
      \x -> let u = emptyUf |> add x |> union x in u === [x]
  , QC.testProperty "empty + add + unions" $
      \x y z -> x /= y && x /= z && y /= z ==> let
          actual = emptyUf |> add z |> join x y |> unions |> deepSet
          expected = S.fromList [S.fromList [x, y], S.fromList [z]]
        in expected === actual
  , QC.testProperty "unions' elements are equal to the elements" $
      \uf -> let
          elems = UF.elements (uf :: UnionFind Int)
          unionElements = unions uf |> concat
        in setSame elems unionElements
  , QC.testProperty "unions are disjoint" $
      \uf -> let
          us = unions (uf :: UnionFind Int)
          listSize = sum $ map length us
          setSize = S.size $ fold $ map S.fromList us
        in listSize === setSize
  , QC.testProperty "large join" $
      \xs -> let
          uniques = S.toList $ S.fromList xs
          result = foldr add emptyUf uniques |>
            flip (foldr (uncurry join)) (pairWindows uniques) |>
            unions
        in deepSetSame result $ if null uniques then [] else [uniques]
  , QC.testProperty "toList after join should contain both elements" $
      \x y -> x /= y ==> setSame (emptyUf |> join x y |> toList) [x, y]
  , QC.testProperty "For every list in unions, every element's union is equal to the list itself" $
      \uf -> let
          sameUnion :: [Int] -> Property
          sameUnion xs = conjoin $ map (setSame xs . (`union` uf)) xs
        in conjoin $ map sameUnion $ unions uf
  , QC.testProperty "Unions -> no list is empty" $
      \uf -> all (not . null) $ unions (uf :: UnionFind Int)
  ] where
      emptyUf :: UnionFind Int
      emptyUf = empty
      setSame xs ys = S.fromList xs === S.fromList ys
      deepSet :: Ord a => [[a]] -> S.Set (S.Set a)
      deepSet = map S.fromList .> S.fromList
      deepSetSame xs ys = deepSet xs === deepSet ys


