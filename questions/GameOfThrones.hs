module GameOfThrones where

import Potential
import Common.Operators

type Name = String
type Line = [Family]

data Family = Family Name [Family] deriving (Show, Eq)

newBorn :: Name -> Family
newBorn n = Family n []
  
birth :: Name -> Name -> Line -> Maybe Line
birth parent son = map aux .> reifyIfAnyFulfilled where
  aux :: Family -> Potential Family
  aux family@(Family current children) = 
    if (current == parent) then Fulfilled $ Family parent ((newBorn son) : children)
    else case reifyIfAnyFulfilled $ map aux children of 
      Nothing -> Unfulfilled family
      Just xs -> Fulfilled $ Family current xs
      
death :: Name -> Line -> Maybe Line 
death name = map aux .> reifyIfAnyFulfilled .> fmap concat where
  aux :: Family -> Potential [Family]
  aux family@(Family parent children) =
    if parent == name then Fulfilled $ reverse children 
    else case reifyIfAnyFulfilled $ map aux children of 
      Nothing -> Unfulfilled [family]
      Just xs -> Fulfilled $ [Family parent (concat xs)]


--   if current == parent then Just $ Data $ current son :: children 
--   else foldl aux (Fake []) $ map (birth parent son) children where
--     aux :: Phantom [Line] -> Line -> Phantom [Line]
    

-- death :: Name -> Line -> Line
-- death = undefined

successionLine :: Line -> [Name]
successionLine = map aux .> concat where
  aux (Family parent children) = parent : concat (map aux (reverse children))

main = do
  let root = [Family "foo" []]
  let family = birth "foo" "bar" root >>= (birth "bar" "bazz") >>= (birth "foo" "quux") >>= (death "bar")
  print family
  print $ successionLine <$> family

