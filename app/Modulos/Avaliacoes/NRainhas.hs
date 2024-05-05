module Avaliacoes.NRainhas where
import Tipos (Individuo (Individuo))
import Utils.Aleatoriedades (randomFloat)



-- Esta função avalia um individuo no problema das N rainhas,
-- onde 'n' é o tamanho do tabuleiro de xadrez e 'Individuo Int' é a representação do tabuleiro.
avaliacao :: Int -> Individuo Int -> Individuo Int
avaliacao n (Individuo gene _) = Individuo gene fitness
  where
    -- A aptidão (fitness) é calculada a partir da quantidade de rainhas não atacando entre si.
    fitness :: Float
    fitness = fromIntegral (n - length (rainhasAtacando gene))

    -- Esta função verifica as rainhas que estão se atacando no tabuleiro.
    rainhasAtacando :: [Int] -> [(Int, Int)]
    rainhasAtacando [] = []
    rainhasAtacando (x : xs) = rainhasAtacando xs ++ rainhasAtacandoNaLinha x xs

    -- Esta função verifica as rainhas que estão se atacando na mesma linha.
    rainhasAtacandoNaLinha :: Int -> [Int] -> [(Int, Int)]
    rainhasAtacandoNaLinha _ [] = []
    rainhasAtacandoNaLinha x (y : ys) =
      -- Se a diferença absoluta entre as posições das rainhas for igual à distância entre elas, elas estão se atacando.
      if abs (x - y) == abs (length ys)
        then (x, y) : rainhasAtacandoNaLinha x ys
        else rainhasAtacandoNaLinha x ys
