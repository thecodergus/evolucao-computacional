module Outros where

import Tipos (Individuo (Individuo), Populacao)


-- Função para realizar escalonamento linear em um indivíduo de uma população
escalonamentoLinear :: Float -> Populacao a -> Individuo a -> Float
escalonamentoLinear c
  | 1.2 <= c && c <= 2.0 = escalonamentoLinear' -- O parâmetro c deve estar entre 1.2 e 2.0
  | otherwise = error "O valor de c deve estar entre 1.2 e 2.0"
   where
      -- A função auxiliar que realiza o escalonamento linear
      escalonamentoLinear' :: Populacao a -> Individuo a -> Float
      escalonamentoLinear' pop (Individuo _ fit) = alfa * fit + beta
         where
            -- Encontra o valor mínimo de fitness da população
            fMin :: Float
            fMin = minimum $ map (\(Individuo _ f) -> f) pop

            -- Encontra o valor máximo de fitness da população
            fMax :: Float
            fMax = maximum $ map (\(Individuo _ f) -> f) pop

            -- Encontra o valor médio de fitness da população
            fAvg :: Float
            fAvg = sum (map (\(Individuo _ f) -> f) pop) / fromIntegral (length pop)

            -- Calcula o valor de alfa para o escalonamento linear
            alfa :: Float
            alfa
               | fMin > ((c * fAvg) - fMax) / c - 1 = (c - 1) * fAvg / (fMax - fAvg)
               | otherwise = fAvg / (fAvg - fMin)

            -- Calcula o valor de beta para o escalonamento linear
            beta :: Float
            beta
               | fMin > ((c * fAvg) - fMax) / c - 1 = fAvg * (fMax - (c * fAvg)) / (fMax - fAvg)
               | otherwise = (-fMin) * fAvg / (fAvg - fMin)
