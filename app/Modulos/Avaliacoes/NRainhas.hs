module Avaliacoes.NRainhas where
import Tipos (Individuo (Individuo))
import Utils.Aleatoriedades (randomFloat)



avaliacao :: Int -> Individuo Int -> Individuo Int
avaliacao n (Individuo gene _) = Individuo gene fitness
    where
        fitness :: Float
        fitness = fromIntegral (n - length (rainhasAtacando gene))

        rainhasAtacando :: [Int] -> [(Int, Int)]
        rainhasAtacando [] = []
        rainhasAtacando (x : xs) = rainhasAtacando xs ++ rainhasAtacandoNaLinha x xs

        rainhasAtacandoNaLinha :: Int -> [Int] -> [(Int, Int)]
        rainhasAtacandoNaLinha _ [] = []
        rainhasAtacandoNaLinha x (y:ys) = if abs (x - y) == abs (length ys) then (x, y) : rainhasAtacandoNaLinha x ys else rainhasAtacandoNaLinha x ys