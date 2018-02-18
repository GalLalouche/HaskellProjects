{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Cayley
import Arbitraries

import Common.Operators
import Common.Lists

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import System.Random(StdGen, next, mkStdGen)
import Data.Either (isRight, isLeft)
import qualified Data.Set as S

main = defaultMain $ testGroup "tests" [group]

-- TODO move to common
conjoinMap :: Testable p => (a -> p) -> [a] -> Property
conjoinMap = map ..> conjoin

group = testGroup "Tree tests" [validateTreeTests, toTreeTests, directTests]

validateTreeTests = testGroup "validateTree" [
    testCase "Empty set is an empty tree" $
      Right EmptyTree @?= (validateTree S.empty :: Either String (Tree Int))
  , testProperty "Loop isn't valid" $
      \x -> isLeft $ validateTree $ S.fromList [Edge x (x :: Int)]
  , testProperty "Two edges are valid iff x /= y" $ \x y -> let
      validated = validateTree $ S.fromList [Edge x (y :: Int)]
    in validated |> if x == y then isLeft else isRight
  , testProperty "Three edges forming a cycle aren't valid" $
      \x y z -> isLeft $ validateTree $ S.fromList [Edge x (y :: Int), Edge y z, Edge z x]
  , testProperty "Three linear edges is valid" $
      \x y z -> areUnique [x, y, z] ==> isRight $
          validateTree $ S.fromList [Edge x (y :: Int), Edge y z]
  ] where
    areUnique xs = let
        setSize = S.size $ S.fromList xs
        listSize = length xs
      in listSize == setSize

toTreeTests = testGroup "toTree" [
    testCase "Empty word" $ let
        actual = toTree $ seqFromList []
        expected = Tree $ S.singleton $ Edge 1 2
      in actual @?= expected
  , testCase "[1] = 0 <-> 1 <-> 2" $ let
        actual = toTree $ seqFromList [2]
        expected = Tree $ S.fromList [Edge 1 2, Edge 2 3]
      in actual @?= expected
  , testCase "6122 (from Combi)" $ let
        actual = toTree $ seqFromList [6, 1, 2, 2]
        expected = Tree $ S.fromList [
            Edge 4 1
          , Edge 1 2
          , Edge 2 5
          , Edge 2 6
          , Edge 3 6
          ]
      in actual @?= expected
  , testProperty "Is always valid" $
    \seq -> let
        tree = toTree seq :: Tree Int
        set = unTree tree
      in isRight $ validateTree set
  , testProperty "Vertices IDs are equal to the alphabet" $
    \seq -> let tree = toTree seq :: Tree Int in vertices tree === alphabet seq
  -- This is proof enough, as there is only one undirected tree satisfying the degree requirement.
  , testProperty "The degree of every node is its number of occurence in the sequence + 1" $
    \seq -> let
        tree = toTree seq :: Tree Int
        sameDegree x = let
            expectedDegree = let
                sequence = code seq
                n = length $ filter (== x) sequence
              in n + 1
            actualDegree = degree x tree
          in expectedDegree === actualDegree
      in conjoinMap sameDegree $ S.toList $ vertices tree
  ] where seqFromList word = PruferSequence word (S.fromList [1 .. length word + 2])

directTests = testGroup "direct" [
    testProperty "the root is always the requested root" $
      treeAndRoot $ \tree r -> let directed = direct r tree in r === root directed
  , testProperty "The out degree of every node is equal to its degree - 1 (except the root)" $
      treeAndRoot $ \tree root -> let
        -- origDegreeF is only used for the first invocation, since the root's in-degree is 0.
        correctDegree origDegreeF (DirectedTree root cs) =
              origDegreeF root === length cs + 1 .&&. conjoinMap (correctDegree (`degree` tree)) cs
        directed = direct root tree
      in correctDegree (\x -> degree x tree + 1) directed
  ] where
    treeAndRoot test = do
      let genRoot = elements . S.toList . vertices
      tree <- arbitrary
      root <- genRoot (tree :: Tree Int)
      return $ test tree root
