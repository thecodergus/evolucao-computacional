module Avaliacoes.Radio where 
import Tipos (Individuo (Individuo))


-- Esta função realiza a avaliação de um indivíduo com base em seu gene.
-- Ela retorna um novo indivíduo com o mesmo gene, mas com o valor de fitness calculado.
avaliacao :: Individuo Bool -> Individuo Bool
avaliacao (Individuo gene _)
  -- Se o tamanho do gene não for 10, uma mensagem de erro é exibida.
  | length gene /= 10 = error "O gene deve ter 10 bits obrigatoriamente!"
  | otherwise = do
      -- Divide o gene em dois grupos de 5 bits cada.
      let (st, lx) = splitAt 5 gene

      -- Converte o grupo 'st' de bits para float e divide pelo divisor 'divSt'.
      let st' = boolToFloat st / divSt
      -- Converte o grupo 'lx' de bits para float e divide pelo divisor 'divLx'.
      let lx' = boolToFloat lx / divLx

    -- Retorna um novo indivíduo com o mesmo gene, mas com o valor de fitness calculado.
      Individuo gene (fitness st' lx')
  where

    -- Divisor utilizado para a conversão do grupo 'st'.
    divSt :: Float
    divSt = 24 / 5

    -- Divisor utilizado para a conversão do grupo 'lx'.
    divLx :: Float
    divLx = 16 / 5

    -- Converte uma lista de booleanos em um número float.
    boolToFloat :: [Bool] -> Float
    boolToFloat = foldr (\x acc -> (if x then 1 else 0) + (-2) * acc) 0

    -- Função objetivo que calcula o valor baseado nos parâmetros 'st' e 'lx'.
    funcaoObjetivo :: Float -> Float -> Float
    funcaoObjetivo st lx = 30 * st + 40 * lx

    -- Verifica se os parâmetros 'st' e 'lx' atendem às restrições do problema.
    restricao :: Float -> Float -> Bool
    restricao st lx = st + 2 * lx <= 40 && 0 <= st && st <= 24 && 0 <= lx && lx <= 16

    fitness :: Float -> Float -> Float
    fitness st lx
      -- Calcula o valor de fitness com base na função objetivo e nas restrições.
      | restricao st lx = foN + r * hN
        -- Se as restrições não forem atendidas, o valor de fitness é zero.
      | otherwise = 0
      where

        -- Penalização aplicada quando há violação das restrições.
        r :: Float
        r = -1

        -- Valor normalizado da função objetivo.
        foN :: Float
        foN = funcaoObjetivo st lx / 1360

        -- Valor normalizado da violação das restrições.
        hN :: Float
        hN = maximum [0 .. ((st + 2 * lx - 40) / 16)]

