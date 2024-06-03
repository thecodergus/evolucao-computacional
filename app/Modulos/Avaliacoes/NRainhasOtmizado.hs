module Avaliacoes.NRainhasOtmizado where
import Tipos (Individuo(Individuo))
import qualified Avaliacoes.NRainhas as NRainhas
import Debug.Trace(trace)


avaliacao :: Int -> Individuo Int -> Individuo Int
avaliacao n (Individuo gene _)
  | length gene /= n = error "O tamanho do gene é diferente do tamanho do tabuleiro."
  | otherwise = NRainhas.avaliacao n (Individuo (tratamento n gene) 0)
  where
    tratamento :: Int -> [Int] -> [Int]
    tratamento n' vetor =  zipWith (curry (n' `transformar`)) vetor [0 ..]
        where
            transformar :: Int -> (Int, Int) -> Int
            transformar n'' (a, b)
                | even (b `mod` n'') = (round . sqrt ) (fromIntegral a :: Double)
                | otherwise = (round . (10 `logBase`)) (fromIntegral a :: Double)
