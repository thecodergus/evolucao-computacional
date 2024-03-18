{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Sat where



replaceWithBools :: [[Int]] -> [Bool] -> [[Bool]]
replaceWithBools intLists boolList = map (map (replace boolList)) intLists
  where
    replace :: [Bool] -> Int -> Bool
    replace boolList i
      | i < 0 = not (boolList !! (abs i - 1))
      | otherwise = boolList !! (i - 1)