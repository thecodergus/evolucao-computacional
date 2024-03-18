module Sat where


    
replaceWithBools :: [[Int]] -> [Bool] -> [[Bool]]
replaceWithBools intLists boolList = map (map (replace boolList)) intLists
  where
    replace boolList i
      | i < 0 = not (boolList !! abs (i))
      | otherwise = boolList !! i