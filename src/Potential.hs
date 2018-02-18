module Potential where

-- A value wrapped with two constructors
-- Useful when applying reifyIfAnyFulfilled
data Potential a = Fulfilled a | Unfulfilled a deriving (Show, Eq)

get :: Potential a -> a
get (Fulfilled a) = a
get (Unfulfilled a) = a

reify :: Potential a -> Potential a
reify (Unfulfilled a) = Fulfilled a
reify e = e

toMaybe :: Potential a -> Maybe a
toMaybe (Fulfilled a) = Just a
toMaybe _ = Nothing

instance Functor Potential where
  fmap f (Unfulfilled a) = Unfulfilled $ f a
  fmap f (Fulfilled a) = Fulfilled $ f a

reifyIfAnyFulfilled :: [Potential a] -> Maybe [a]
reifyIfAnyFulfilled [] = Nothing
reifyIfAnyFulfilled ((Fulfilled x) : xs) = Just $ x : (map get xs)
reifyIfAnyFulfilled ((Unfulfilled x) : xs) = case reifyIfAnyFulfilled xs of
  Just xs -> Just $ x : xs
  Nothing -> Nothing
-- foldl aux $ Unfulfilled [] where
--   aux (Potential xs) (Fulfilled x) = reify agg <$> (a ::)
