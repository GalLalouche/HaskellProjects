module GameOfThrones where

import Potential

data Line = Line Name [Line]
type Name = String
  
birth :: Name -> Name -> Line -> Maybe Line
birth parent son line = undefined where
  aux :: Name -> Name -> Line -> Phantom Line
  aux :: parent son line@(current children) = 
    if (current == parent) then Fulfilled $ Line Parent (son : children)
    else case (reifyIfAnyFulfilled $ map (birth parent son) children) of 
      Nothing -> Unfulfilled line
      Just xs -> Fulfilled $ current xs
      
--   if current == parent then Just $ Data $ current son :: children 
--   else foldl aux (Fake []) $ map (birth parent son) children where
--     aux :: Phantom [Line] -> Line -> Phantom [Line]
    

-- death :: Name -> Line -> Line
-- death = undefined

-- successionLine :: Line -> [Name]
-- successionLine = undefined
main = do
  print "hello world!"
