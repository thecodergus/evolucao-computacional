module Avaliacoes.Radio where
import Tipos (Individuo (Individuo))
import Debug.Trace(trace)


-- | Função 'avaliacao' recebe um 'Individuo' contendo um gene de tipo 'Bool' e retorna um 'Maybe' contendo um novo 'Individuo' com o valor de aptidão calculado.
-- A função verifica se o tamanho do gene é igual a 10. Caso contrário, retorna 'Nothing'.
-- Caso o tamanho seja válido, o gene é dividido em duas partes, 'st' e 'lx', cada uma contendo 5 elementos.
-- Os valores de 'st' e 'lx' são convertidos de 'Bool' para 'Float' e divididos pelos valores de 'divSt' e 'divLx', respectivamente.
-- O valor de aptidão é calculado chamando a função 'fitness' com os valores de 'st' e 'lx'.
-- O novo 'Individuo' com o gene e o valor de aptidão é retornado dentro de um 'Just'.
avaliacao :: Individuo Bool -> Maybe (Individuo Bool)
avaliacao (Individuo gene _)
  | length gene /= 10 = Nothing
  | otherwise = do
      let (st, lx) = splitAt 5 gene
          st' = boolToFloat st / divSt
          lx' = boolToFloat lx / divLx
          fitnessValue = fitness st' lx'
      Just $ Individuo gene fitnessValue
  where
    divSt :: Float
    divSt = 24 / 5

    divLx :: Float
    divLx = 16 / 5

    boolToFloat :: [Bool] -> Float
    boolToFloat = foldr (\x acc -> (if x then 1 else 0) + (-2) * acc) 0 
    
    funcaoObjetivo :: Float -> Float -> Float
    funcaoObjetivo st lx = 30 * st + 40 * lx

    restricao :: Float -> Float -> Bool
    restricao st lx = st + 2 * lx <= 40 && 0 <= st && st <= 24 && 0 <= lx && lx <= 16

    fitness :: Float -> Float -> Float
    fitness st lx
      | restricao st lx = foN + r * hN
      | otherwise = 0
      where
        r :: Float
        r = -1

        foN :: Float
        foN = funcaoObjetivo st lx / 1360

        hN :: Float
        hN = max 0 ((st + 2 * lx - 40) / 16)
