{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Sat where

import Tipos(Individuo (genes, fitness))


funcaoObjetivo :: Individuo Bool -> [[Bool]] -> Individuo Bool
funcaoObjetivo individuo clausulas =
  -- Converte os genes do indivíduo de Bool para Int
  let genesInt = map (toEnum . fromEnum) $ genes individuo

      -- Conta o número de cláusulas satisfazidas pelo indivíduo
      num_satisfeitas = length $ filter (any fst . filter snd . zip genesInt) clausulas

      -- Calcula o valor da função objetivo como a proporção de cláusulas satisfazidas
      fitness' = fromIntegral num_satisfeitas / fromIntegral (length clausulas)
   in -- Atualiza o campo fitness do indivíduo com o valor da função objetivo
      individuo {fitness = fitness'}
