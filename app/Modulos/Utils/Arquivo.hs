module Arquivo where
import Data.Maybe (mapMaybe)


-- A função fileToIntLists lê um arquivo e converte suas linhas em uma lista de listas de inteiros.
fileToIntLists :: FilePath -> IO [[Int]]
-- A função recebe o caminho do arquivo como parâmetro e retorna uma lista de listas de inteiros envolvida em uma ação IO.
fileToIntLists filePath = do
  content <- readFile filePath
  -- Lê o conteúdo do arquivo.
  let lines' = drop 2 $ lines content -- Remove as duas primeiras linhas
  -- Ignora as duas primeiras linhas do arquivo, supostamente contendo metadados.
  return $ removeEmptySublists $ stringsToIntLists lines'
  -- Converte as linhas em listas de inteiros e remove as listas vazias.
  where
    -- A função stringsToIntLists converte cada string em uma lista de inteiros.
    stringsToIntLists :: [String] -> [[Int]]
    stringsToIntLists = map stringToIntList

    -- A função stringToIntList converte uma string em uma lista de inteiros.
    stringToIntList :: String -> [Int]
    stringToIntList input = filter (/= 0) $ mapMaybe safeRead $ filter (not . null) $ concatMap words $ takeWhile ((/=) '%' . head) $ lines input
    -- Lê as linhas da string de entrada até encontrar um caractere '%',
    -- separa as palavras, filtra as palavras vazias,
    -- converte as palavras em inteiros usando safeRead
    -- e filtra os zeros.
      where
        safeRead s = case reads s of
                      [(x, "")] -> Just x
                      -- Converte uma string em um inteiro, retornando Nothing se não for possível.
                      _ -> Nothing

    -- A função removeEmptySublists remove as listas vazias de uma lista de listas.
    removeEmptySublists :: [[a]] -> [[a]]
    removeEmptySublists = filter (not . null)