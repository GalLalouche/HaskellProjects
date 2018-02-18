module Main where

import Test.HUnit
import Potential

-- import Common.Operators
-- import Data.List(intercalate)
-- import Common.Parsec(Parser)

tests = test [
    "reifyIfAnyFulfilled [] is Nothing" ~: do
    reifyIfAnyFulfilled ([] :: [Potential Int]) ~=? Nothing
  , "reifyIfAnyFulfilled [Fulfilled] is Just" ~: do
    reifyIfAnyFulfilled [Fulfilled 0] ~=? Just [0]
  , "reifyIfAnyFulfilled [Unfulfilled] is Nothing" ~: do
    reifyIfAnyFulfilled [Unfulfilled 0] ~=? Nothing
  , "More tests 1" ~: do
    reifyIfAnyFulfilled [Fulfilled 0, Unfulfilled 1] ~=? Just [0, 1]
  , "More tests 2" ~: do
    reifyIfAnyFulfilled [Unfulfilled 0, Unfulfilled 1] ~=? Nothing
  , "More tests 3" ~: do
    reifyIfAnyFulfilled [Unfulfilled 0, Fulfilled 1] ~=? Just [0, 1]
  ]

main :: IO Counts
main = runTestTT tests


