module Avaliacoes.NRainhas where

import Data.List (foldl')
import Debug.Trace (trace)
import Tipos (Individuo (Individuo))

-- Esta função avalia um individuo no problema das N rainhas,
-- onde 'n' é o tamanho do tabuleiro de xadrez e 'Individuo Int' é a representação do tabuleiro.
avaliacao :: Bool -> Int -> Individuo Int -> Individuo Int
avaliacao tipo n (Individuo gene _)
  | tipo = Individuo gene (fitness' gene n)
  | otherwise = Individuo gene (fitness gene n)
  where
    -- A aptidão (fitness) é calculada a partir da quantidade de rainhas não atacando entre si.
    fitness :: [Int] -> Int -> Float
    fitness vetor n' = 1 - (fromIntegral (length (rainhasAtacando vetor)) / fromIntegral n')

    fitness' :: [Int] -> Int -> Float
    fitness' vetor n' =
      (1 - (fromIntegral (length (rainhasAtacando vetor)) / fromIntegral n'))
        * let tabuleiro = criarTabuleiro n' in (somarValoresTabuleiroValorado (tranformarParaValorado n' (tranformarCoordenadaCartesiana' vetor)) tabuleiro / realToFrac (sum tabuleiro))

    -- Esta função verifica as rainhas que estão se atacando no tabuleiro.
    rainhasAtacando :: [Int] -> [(Int, Int)]
    rainhasAtacando xs = rainhasAtacando' (tranformarCoordenadaCartesiana' xs)
      where
        -- Esta função verifica as rainhas que estão se atacando na mesma linha.
        rainhasAtacando' :: [(Int, Int)] -> [(Int, Int)]
        rainhasAtacando' xs' = filter verificar xs'
          where
            verificar :: (Int, Int) -> Bool
            verificar a = any (\b -> linhaColuna a b || diagonal a b) xs'
              where
                linhaColuna :: (Int, Int) -> (Int, Int) -> Bool
                linhaColuna a'@(x', y') b'@(x'', y'') = (x' == x'' || y' == y'') && (a' /= b')

                diagonal :: (Int, Int) -> (Int, Int) -> Bool
                diagonal a'@(x', y') b'@(x'', y'') = a' /= b' && (b_growing || b_shrinking)
                  where
                    b_growing = x' - y' == x'' - y''
                    b_shrinking = x' + y' == x'' + y''

    criarTabuleiro :: Int -> [Double]
    criarTabuleiro n' = zipWith (curry (n' `transformar`)) [0 .. (n' * n') - 1] [0 .. (n' * n') - 1]
      where
        transformar :: Int -> (Int, Int) -> Double
        transformar n'' (a, b)
          | even (b `mod` n'') = sqrt (fromIntegral a :: Double)
          | otherwise = logBase 10 (fromIntegral a :: Double)

    tranformarCoordenadaCartesiana' :: [Int] -> [(Int, Int)]
    tranformarCoordenadaCartesiana' = zip [0 ..]

    tranformarParaValorado :: Int -> [(Int, Int)] -> [Int]
    tranformarParaValorado _ [] = []
    tranformarParaValorado n' ((x, y) : ts) = (n' * x) + y : tranformarParaValorado n' ts

    somarValoresTabuleiroValorado :: [Int] -> [Double] -> Float
    somarValoresTabuleiroValorado indices valores =
      realToFrac $ foldl' (\acc i -> acc + valores !! i) 0 indices