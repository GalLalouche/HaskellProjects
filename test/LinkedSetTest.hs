module Main where

import Common.Operators

import Test.QuickCheck
import LinkedSet
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import qualified Data.Set as S
import Data.Foldable(toList)

import Data.List
import Data.Ord

main = defaultMain $ testGroup "tests" [props]

props = testGroup "(checked by QuickCheck)" [
    QC.testProperty "empty - no element is a member" $
      \x -> emptyLs |> appendOrSame x |> member x
  , QC.testProperty "only the added member is a member" $
      \x y -> x /= y ==> not $ emptyLs |> appendOrSame x |> member y
  , QC.testProperty "elements keep order of insertions" $
      \x y z -> x /= y && y /= z && x /= z ==> let
        list = emptyLs |> appendOrSame x |> appendOrSame y |> appendOrSame z |> toList
      in [x, y, z] === list
  , QC.testProperty "index of elements which does not exist is Nothing" $
      \x -> Nothing === index x emptyLs
  ] where
      emptyLs :: LinkedSet Int
      emptyLs = empty
--       setSame xs ys = S.fromList xs === S.fromList ys
--       deepSet :: Ord a => [[a]] -> S.Set (S.Set a)
--       deepSet = map S.fromList .> S.fromList
--       deepSetSame xs ys = deepSet xs === deepSet ys


