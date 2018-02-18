{-# LANGUAGE FlexibleInstances #-}

module Arbitraries where

import qualified Data.Set as S
import Test.QuickCheck
import Cayley

instance Arbitrary (PruferSequence Int) where
  arbitrary = do
    wordLength <- (`mod` 100) <$> arbitrary
    let alphabetSize = wordLength + 2
    let letterGen = ((1 +) . (`mod` alphabetSize)) <$> arbitrary
    word <- vectorOf wordLength letterGen
    let alphabet = S.fromList [1 .. alphabetSize]
    return $ PruferSequence word alphabet

instance Arbitrary (Tree Int) where
  arbitrary = toTree <$> arbitrary

instance Arbitrary (DirectedTree Int) where
  arbitrary = do
    tree <- arbitrary
    root <- elements $ S.toList $ vertices (tree :: Tree Int)
    return $ direct root tree
