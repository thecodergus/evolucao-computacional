module Avaliacoes.NRainhas where
import Tipos (Individuo (Individuo))


-- Esta função avalia um individuo no problema das N rainhas,
-- onde 'n' é o tamanho do tabuleiro de xadrez e 'Individuo Int' é a representação do tabuleiro.
avaliacao :: Int -> Individuo Int -> Individuo Int
avaliacao n (Individuo gene _) = Individuo gene fitness
  where
    -- A aptidão (fitness) é calculada a partir da quantidade de rainhas não atacando entre si.
    fitness :: Float
    fitness = fromIntegral (n * (n - 1) `div` 2 - length (rainhasAtacando gene))

    -- Esta função verifica as rainhas que estão se atacando na mesma linha.
    rainhasAtacandoNaLinha :: [Int] -> Int -> [(Int, Int)]
    rainhasAtacandoNaLinha xs y = [(i, y) | (i, x) <- zip [0 ..] xs, x == y]

    -- Esta função verifica as rainhas que estão se atacando na mesma coluna.
    rainhasAtacandoNaColuna :: [Int] -> [(Int, Int)]
    rainhasAtacandoNaColuna xs = [(i, j) | (i, x) <- zip [0 ..] xs, (j, y) <- zip [0 ..] xs, i /= j, x == y]

    -- Esta função verifica as rainhas que estão se atacando na mesma diagonal.
    rainhasAtacandoNaDiagonal :: [Int] -> [(Int, Int)]
    rainhasAtacandoNaDiagonal xs = [(i, j) | (i, x) <- zip [0 ..] xs, (j, y) <- zip [0 ..] xs, i /= j, abs (x - y) == abs (i - j)]

    -- Esta função verifica as rainhas que estão se atacando no tabuleiro.
    rainhasAtacando :: [Int] -> [(Int, Int)]
    rainhasAtacando xs = concat [rainhasAtacandoNaLinha xs y | y <- [0 .. n' - 1]] ++ rainhasAtacandoNaColuna xs ++ rainhasAtacandoNaDiagonal xs
        where
            n' = length xs
