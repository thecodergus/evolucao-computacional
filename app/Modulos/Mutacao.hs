module Mutacao where

import Tipos (Individuo(genes, Individuo))
import GerarAleatoriedades (randomFloat, randomInt)


mutacao :: Individuo (*) -> Float -> IO (Individuo (*))
mutacao individuo probabilidade_mutacao = do
  novo_individuo <- mutacao' (genes individuo)
  return (Individuo novo_individuo 0)
    where
        mutacao' :: [*] -> IO [*]
        mutacao' xs = mapM mutar xs

        mutar :: * -> IO *
        mutar item = do
            prob <- randomFloat (0, 1)
            if probabilidade_mutacao >= prob then do
                indice <- randomInt (0, length (genes individuo) - 1)
                return (genes individuo !! indice)
            else
                return item
