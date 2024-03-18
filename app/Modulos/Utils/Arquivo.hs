module Arquivo where

-- Converte uma string em uma lista de inteiros, descartando os zeros e os não numéricos
stringToIntList :: String -> [Int]
stringToIntList input =
  let lines' = takeWhile ((/=) '%' . head) $ lines input
      words' = concatMap words lines'
      nums = filter (not . null) words'
      ints = map read nums
   in filter (/= 0) ints

-- Converte uma lista de strings em uma lista de lista de inteiros
stringsToIntLists :: [String] -> [[Int]]
stringsToIntLists = map stringToIntList

-- Lê o arquivo e transforma em uma lista de lista de inteiros
fileToIntLists :: FilePath -> IO [[Int]]
fileToIntLists filePath = do
  content <- readFile filePath
  let lines' = tail $ lines content -- Remove a primeira linha
  return $ removeEmptySublists $ stringsToIntLists lines'

-- Remover Sublistas vazias
removeEmptySublists :: [[a]] -> [[a]]
removeEmptySublists = filter (not . null)
