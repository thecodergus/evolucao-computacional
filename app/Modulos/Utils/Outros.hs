module Utils.Outros where

import Control.Monad (forM)
import Data.Array.IO
  ( IOArray,
    newListArray,
    readArray,
    writeArray,
  )
import System.Random (randomRIO, newStdGen)
import Tipos (Individuo (Individuo))
import Data.List (intercalate, (\\))
import System.Random.Stateful (Random(randomRs))

-- Função que troca dois elementos de uma lista em posições específicas
swapElementsAt :: Int -> Int -> [a] -> [a]
swapElementsAt _ _ [] = []
swapElementsAt i j xs =
  -- Recupera os elementos a serem trocados e as partes da lista antes e depois deles
  let elemI = xs !! i
      elemJ = xs !! j
      left = take i xs
      middle = take (j - i - 1) (drop (i + 1) xs)
      right = drop (j + 1) xs
   in -- Concatena as partes da lista com os elementos trocados
      left ++ [elemJ] ++ middle ++ [elemI] ++ right

-- Função que embaralha uma lista qualquer
shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs =
  newArray n xs
    >>= \ar -> forM [1 .. n] $ \i ->
      randomRIO (i, n)
        >>= \j ->
          readArray ar i
            >>= \vi ->
              readArray ar j
                >>= \vj -> writeArray ar j vi >> return vj
  where
    n = length xs

    -- Cria um array com os elementos da lista
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n' = newListArray (1, n')

-- Função que divide uma lista em 3 com base nos dois inputs de inteiros
splitListAtTwoIndices :: [a] -> Int -> Int -> ([a], [a], [a])
splitListAtTwoIndices xs i j
  | i < 0 || j < 0 || i > length xs || j > length xs || i > j = error "Invalid indices"
  | otherwise = (take i xs, mid, drop (j + 1) xs)
  where
    mid = drop i $ take (j + 1) xs

-- Função que verifica se determinado item pertence a lista
ifIn :: Eq a => a -> [a] -> Bool
ifIn _ [] = False
ifIn a (b : bs)
  | a == b = True
  | otherwise = ifIn a bs

tratamento :: [Int] -> [Int]
tratamento vetor = zipWith (curry (length vetor `transformar`)) vetor [0 ..]
  where
    transformar :: Int -> (Int, Int) -> Int
    transformar n'' (a, b)
      | even (b `mod` n'') = (round . sqrt) (fromIntegral a :: Double)
      | otherwise = (round . (10 `logBase`)) (fromIntegral a :: Double)

exibirTabuleiro :: Int -> Individuo Int -> IO ()
exibirTabuleiro n (Individuo gene f) = do
  putStrLn ("Fitness: " ++ show f)
  let pieces = zip [0 ..] gene

  putStrLn $ drawBoard n pieces
  where
    -- Função para desenhar o tabuleiro
    drawBoard :: Int -> [(Int, Int)] -> String
    drawBoard n' pieces = intercalate "\n" $ map drawLine [0 .. n' - 1]
      where
        drawLine :: Int -> String
        drawLine x = intercalate "|" $ map drawCell [0 .. n' - 1]
          where
            drawCell :: Int -> String
            drawCell y = if (y, x) `elem` pieces then " X" else "  "

toPairs :: [a] -> [(a, a)]
toPairs [] = []
toPairs [x] = [(x, x)]
toPairs (x : y : xs) = (x, y) : toPairs xs

pairToList :: [(a, a)] -> [a]
pairToList = concatMap (\(x, y) -> [x, y])

randomPairAndRest :: Eq a => [a] -> IO ((a, a), [a])
randomPairAndRest xs = do
  g <- newStdGen
  let indices = take 2 $ randomRs (0, length xs - 1) g
  let pair = map (xs !!) indices
  return ((head pair, pair !! 1), xs \\ pair)