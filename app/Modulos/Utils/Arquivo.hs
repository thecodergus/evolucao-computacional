module Arquivo where
import Data.Maybe (mapMaybe)


fileToIntLists :: FilePath -> IO [[Int]]
fileToIntLists filePath = do
  content <- readFile filePath
  let lines' = drop 2 $ lines content -- Remove as duas primeiras linhas
  return $ removeEmptySublists $ stringsToIntLists lines'
  where
    stringsToIntLists :: [String] -> [[Int]]
    stringsToIntLists = map stringToIntList
      where
        stringToIntList :: String -> [Int]
        stringToIntList input = filter (/= 0) $ mapMaybe safeRead $ filter (not . null) $ concatMap words $ takeWhile ((/=) '%' . head) $ lines input
          where
            safeRead s = case reads s of
                          [(x, "")] -> Just x
                          _ -> Nothing

    removeEmptySublists :: [[a]] -> [[a]]
    removeEmptySublists = filter (not . null)

    