module Utils.Outros where

import Control.Monad ( forM )
import Data.Array.IO
    ( newListArray, readArray, writeArray, IOArray )
import System.Random ( randomRIO )
import Utils.Aleatoriedades (randomInt)


-- Função que troca dois elementos de uma lista em posições específicas
swapElementsAt :: Int -> Int -> [a] -> [a]
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
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs

    -- Cria um array com os elementos da lista
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n' = newListArray (1,n')


selecionarRemover :: [a] -> IO (a, [a])
selecionarRemover itens = do
  random <- randomInt (0, tam - 1)

  return (itens !! random, removeAt random itens)
  where
    tam :: Int
    tam = length itens

    removeAt :: Int -> [a] -> [a]
    removeAt n xs = let (as, bs) = splitAt n xs in as ++ tail bs

