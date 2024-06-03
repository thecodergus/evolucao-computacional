module Avaliacoes.NRainhasValorada where
import Tipos (Individuo (Individuo))
import Utils.Outros (tratamento)


avaliacao :: Individuo Int -> Individuo Int
avaliacao (Individuo gene _) = Individuo gene (fitness gene)
    where
        -- A aptidão (fitness) é calculada a partir da quantidade de rainhas não atacando entre si.
        fitness :: [Int] -> Float
        fitness vetor = 1 - (fromIntegral (length ((rainhasAtacando . (`zip` [0 ..]) . tratamento) vetor)) / fromIntegral (length vetor))
        
        -- Esta função verifica as rainhas que estão se atacando na mesma linha.
        rainhasAtacando :: [(Int, Int)] -> [(Int, Int)]
        rainhasAtacando xs' = filter verificar xs'
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
