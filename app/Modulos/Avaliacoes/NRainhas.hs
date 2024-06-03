module Avaliacoes.NRainhas where
import Tipos (Individuo (Individuo))
import Debug.Trace (trace)


-- Esta função avalia um individuo no problema das N rainhas,
-- onde 'n' é o tamanho do tabuleiro de xadrez e 'Individuo Int' é a representação do tabuleiro.
avaliacao :: Bool -> Int -> Individuo Int -> Individuo Int
avaliacao True n (Individuo gene _) = Individuo gene 0
avaliacao False n (Individuo gene _) = Individuo gene (fitness gene n)
  where
    -- A aptidão (fitness) é calculada a partir da quantidade de rainhas não atacando entre si.
    fitness :: [Int] -> Int -> Float
    fitness vetor n' = 1 - (fromIntegral (length (rainhasAtacando vetor)) / fromIntegral n')

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


    tranformarCoordenadaCartesiana' :: [Int] -> [(Int, Int)]
    tranformarCoordenadaCartesiana' = zip [1..]
