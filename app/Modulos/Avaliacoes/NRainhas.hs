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
                diagonal a'@(x', y') b'@(x'', y'') = (abs (x' - x'') /= abs (y' - y'')) && a' /= b'
            

    tranformarCoordenadaCartesiana' :: [Int] -> [(Int, Int)]
    tranformarCoordenadaCartesiana' = zip [1..]
